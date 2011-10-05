(ns redis.command-tests
  (:refer-clojure :exclude [keys type get set sort])
  (:require [redis.core :as redis]
            [redis.commands :as cmd])
  (:use [clojure.test]))

(defn server-fixture [f]
  (redis/with-server
   {:host "127.0.0.1"
    :port 6379
    :db 15}
   ;; String value
   (cmd/set "foo" "bar")
   ;; List with three items
   (cmd/rpush "list" "one")
   (cmd/rpush "list" "two")
   (cmd/rpush "list" "three")
   ;; Set with three members
   (cmd/sadd "set" "one")
   (cmd/sadd "set" "two")
   (cmd/sadd "set" "three")
   ;; Hash with three fields
   (cmd/hset "hash" "one" "foo")
   (cmd/hset "hash" "two" "bar")
   (cmd/hset "hash" "three" "baz")
   (f)
   (cmd/flushdb)))

(use-fixtures :each server-fixture)

(deftest ping
  (is (= "PONG" (cmd/ping))))

(deftest exists
  (is (= true (cmd/exists "foo")))
  (is (= false (cmd/exists "nonexistent"))))

(deftest del
  (is (= 0 (cmd/del "nonexistent")))
  (is (= 1 (cmd/del "foo")))
  (is (= nil  (cmd/get "foo")))
  (cmd/mset "one" "1" "two" "2" "three" "3")
  (is (= 3 (cmd/del "one" "two" "three"))))

(deftest type
  (is (= :none (cmd/type "nonexistent")))
  (is (= :string (cmd/type "foo")))
  (is (= :list (cmd/type "list")))
  (is (= :set (cmd/type "set"))))

(deftest keys
  (is (= [] (cmd/keys "a*")))
  (is (= ["foo"] (cmd/keys "f*")))
  (is (= ["foo"] (cmd/keys "f?o")))
  (cmd/set "fuu" "baz")
  (is (= #{"foo" "fuu"} (clojure.core/set (cmd/keys "f*")))))

(deftest randomkey
  (cmd/flushdb)
  (cmd/set "foo" "bar")
  (is (= "foo" (cmd/randomkey)))
  (cmd/flushdb)
  (is (nil? (cmd/randomkey))))

(deftest rename
  (is (thrown? Exception (cmd/rename "foo" "foo")))
  (is (thrown? Exception (cmd/rename "nonexistent" "foo")))
  (cmd/rename "foo" "bar")
  (is (= "bar" (cmd/get "bar")))
  (is (= nil (cmd/get "foo")))
  (cmd/set "foo" "bar")
  (cmd/set "bar" "baz")
  (cmd/rename "foo" "bar")
  (is (= "bar" (cmd/get "bar")))
  (is (= nil (cmd/get "foo"))))

(deftest renamenx
  (is (thrown? Exception (cmd/renamenx "foo" "foo")))
  (is (thrown? Exception (cmd/renamenx "nonexistent" "foo")))
  (is (= true (cmd/renamenx "foo" "bar")))
  (is (= "bar" (cmd/get "bar")))
  (is (= nil (cmd/get "foo")))
  (cmd/set "foo" "bar")
  (cmd/set "bar" "baz")
  (is (= false (cmd/renamenx "foo" "bar"))))

(deftest dbsize
  (let [size-before (cmd/dbsize)]
    (cmd/set "anewkey" "value")
    (let [size-after (cmd/dbsize)]
      (is (= size-after
             (+ 1 size-before))))))

(deftest expire
  (is (= true (cmd/expire "foo" 1)))
  (Thread/sleep 2000)
  (is (= false (cmd/exists "foo")))
  (cmd/set "foo" "bar")
  (is (= true (cmd/expire "foo" 20)))
  (is (= false (cmd/expire "nonexistent" 42))))

(deftest ttl
  (is (= -1 (cmd/ttl "nonexistent")))
  (is (= -1 (cmd/ttl "foo")))
  (cmd/expire "foo" 42)
  (is (< 40 (cmd/ttl "foo"))))

(deftest select
  (cmd/select 0)
  (is (= nil (cmd/get "akeythat_probably_doesnotexsistindb0"))))

(deftest flushdb
  (cmd/flushdb)
  (is (= 0 (cmd/dbsize))))


;;
;; String commands
;; 
(deftest set
  (cmd/set "bar" "foo")
  (is (= "foo" (cmd/get "bar")))
  (cmd/set "foo" "baz")
  (is (= "baz" (cmd/get "foo"))))

(deftest get
  (is (= nil (cmd/get "bar")))
  (is (= "bar" (cmd/get "foo"))))

(deftest getset
  (is (= nil   (cmd/getset "bar" "foo")))
  (is (= "foo" (cmd/get "bar")))
  (is (= "bar" (cmd/getset "foo" "baz")))
  (is (= "baz" (cmd/get "foo"))))

(deftest mget
  (is (= [nil] (cmd/mget "bar")))
  (cmd/set "bar" "baz")
  (cmd/set "baz" "buz")
  (is (= ["bar"] (cmd/mget "foo")))
  (is (= ["bar" "baz"] (cmd/mget "foo" "bar")))
  (is (= ["bar" "baz" "buz"] (cmd/mget "foo" "bar" "baz")))
  (is (= ["bar" nil "buz"] (cmd/mget "foo" "bra" "baz"))))

(deftest mset
  (is (thrown?  Exception (cmd/mset "key1"))) 
  (is (thrown?  Exception (cmd/mset "key" "value" "key1")))
  (cmd/mset "key1" "value1" "key2" "value2" "key3" "value3")
  (is (= ["value1" "value2" "value3"] (cmd/mget "key1" "key2" "key3"))))

(deftest msetnx
  (is (thrown? Exception (cmd/msetnx "key1")))
  (is (thrown? Exception (cmd/msetnx "key1" "value1" "key2")))
  (is (= true (cmd/msetnx "key1" "value1" "key2" "value2" "key3" "value3")))
  (is (= ["value1" "value2" "value3"] (cmd/mget "key1" "key2" "key3")))
  (is (= false (cmd/msetnx "key4" "value4" "key2" "newvalue" "key5" "value5")))
  (is (= [nil "value2" nil] (cmd/mget "key4" "key2" "key5"))))

(deftest setnx
  (is (= true (cmd/setnx "bar" "foo")))
  (is (= "foo" (cmd/get "bar")))
  (is (= false (cmd/setnx "foo" "baz")))
  (is (= "bar" (cmd/get "foo"))))

(deftest incr
  (is (= 1 (cmd/incr "nonexistent")))
  (is (thrown? Exception (cmd/incr "foo")))
  (is (= 1 (cmd/incr "counter")))
  (is (= 2 (cmd/incr "counter"))))

(deftest incrby
  (is (= 42 (cmd/incrby "nonexistent" 42)))
  (is (thrown? Exception (cmd/incrby "foo" 42)))
  (is (= 0 (cmd/incrby "counter" 0)))
  (is (= 42 (cmd/incrby "counter" 42))))

(deftest decr
  (is (= -1 (cmd/decr "nonexistent")))
  (is (thrown? Exception (cmd/decr "foo")))
  (is (= -1 (cmd/decr "counter")))
  (is (= -2 (cmd/decr "counter"))))

(deftest decrby
  (is (= -42 (cmd/decrby "nonexistent" 42)))
  (is (thrown? Exception (cmd/decrby "foo" 0)))
  (is (= 0 (cmd/decrby "counter" 0)))
  (is (= -42 (cmd/decrby "counter" 42))))

(deftest append
  (is (= 5 (cmd/append "string" "Hello")))
  (is (= 11 (cmd/append "string" " World")))
  (is (= "Hello World" (cmd/get "string"))))

(deftest substr
  (cmd/set "s" "This is a string")
  (is (= "This" (cmd/substr "s" 0 3)))
  (is (= "ing" (cmd/substr "s" -3 -1)))
  (is (= "This is a string" (cmd/substr "s" 0 -1)))
  (is (= " string" (cmd/substr "s" 9 100000))))

;;
;; List commands
;;
(deftest rpush
  (is (thrown? Exception (cmd/rpush "foo")))
  (cmd/rpush "newlist" "one")
  (is (= 1 (cmd/llen "newlist")))
  (is (= "one" (cmd/lindex "newlist" 0)))
  (cmd/del "newlist")
  (cmd/rpush "list" "item")
  (is (= "item" (cmd/rpop "list"))))

(deftest lpush
  (is (thrown? Exception (cmd/lpush "foo")))
  (cmd/lpush "newlist" "item")
  (is (= 1 (cmd/llen "newlist")))
  (is (= "item" (cmd/lindex "newlist" 0)))
  (cmd/lpush "list" "item")
  (is (= "item" (cmd/lpop "list"))))

(deftest llen
  (is (thrown? Exception (cmd/llen "foo")))
  (is (= 0 (cmd/llen "newlist")))
  (is (= 3 (cmd/llen "list"))))

(deftest lrange
  (is (thrown? Exception (cmd/lrange "foo" 0 1)))
  (is (= [] (cmd/lrange "newlist" 0 42)))
  (is (= ["one"] (cmd/lrange "list" 0 0)))
  (is (= ["three"] (cmd/lrange "list" -1 -1)))
  (is (= ["one" "two"] (cmd/lrange "list" 0 1)))
  (is (= ["one" "two" "three"] (cmd/lrange "list" 0 2)))
  (is (= ["one" "two" "three"] (cmd/lrange "list" 0 42)))
  (is (= [] (cmd/lrange "list" 42 0))))

(deftest ltrim
  (is (thrown? Exception (cmd/ltrim "foo" 0 0)))
  (cmd/ltrim "list" 0 1)
  (is (= ["one" "two"] (cmd/lrange "list" 0  99)))
  (cmd/ltrim "list" 1 99)
  (is (= ["two"] (cmd/lrange "list" 0  99))))

(deftest lindex
  (is (thrown? Exception (cmd/lindex "foo" 0)))
  (is (= nil (cmd/lindex "list" 42)))
  (is (= nil (cmd/lindex "list" -4)))
  (is (= "one" (cmd/lindex "list" 0)))
  (is (= "three" (cmd/lindex "list" 2)))
  (is (= "three" (cmd/lindex "list" -1))))

(deftest lset
  (is (thrown? Exception (cmd/lset "foo" 0 "bar")))
  (is (thrown? Exception (cmd/lset "list" 42 "value")))
  (cmd/lset "list" 0 "test")
  (is (= "test" (cmd/lindex "list" 0)))
  (cmd/lset "list" 2 "test2")
  (is (= "test2" (cmd/lindex "list" 2)))
  (cmd/lset "list" -1 "test3")
  (is (= "test3" (cmd/lindex "list" 2))))

(deftest lrem
  (is (thrown? Exception (cmd/lrem "foo" 0 "bar")))
  (is (= 0 (cmd/lrem "newlist" 0 "")))
  (is (= 1 (cmd/lrem "list" 1 "two")))
  (is (= 1 (cmd/lrem "list" 42 "three")))
  (is (= 1 (cmd/llen "list"))))

(deftest lpop
  (is (thrown? Exception (cmd/lpop "foo")))
  (is (= nil (cmd/lpop "newlist")))
  (is (= "one" (cmd/lpop "list")))
  (is (= 2 (cmd/llen "list"))))

(deftest rpop
  (is (thrown? Exception (cmd/rpop "foo")))
  (is (= nil (cmd/rpop "newlist")))
  (is (= "three" (cmd/rpop "list")))
  (is (= 2 (cmd/llen "list"))))

(deftest blpop)

(deftest brpop)

(deftest rpoplpush
  (cmd/rpush "src" "a")
  (cmd/rpush "src" "b")
  (cmd/rpush "src" "c")
  (cmd/rpush "dest" "foo")
  (cmd/rpush "dest" "bar")
  (is (= "c" (cmd/rpoplpush "src" "dest")))
  (is (= ["a" "b"] (cmd/lrange "src" 0 -1)))
  (is (= ["c" "foo" "bar" (cmd/lrange "dest" 0 -1)])))

;;
;; Set commands
;;
(deftest sadd
  (is (thrown? Exception (cmd/sadd "foo" "bar")))
  (is (= true (cmd/sadd "newset" "member")))
  (is (= true (cmd/sismember "newset" "member")))
  (is (= false (cmd/sadd "set" "two")))
  (is (= true (cmd/sadd "set" "four")))
  (is (= true (cmd/sismember "set" "four"))))

(deftest srem
  (is (thrown? Exception (cmd/srem "foo" "bar")))
  (is (= false (cmd/srem "newset" "member")))
  (is (= true (cmd/srem "set" "two")))
  (is (= false (cmd/sismember "set" "two")))
  (is (= false (cmd/srem "set" "blahonga"))))

(deftest spop
  (is (thrown? Exception (cmd/spop "foo" "bar")))
  (is (= nil (cmd/spop "newset")))
  (is (contains? #{"one" "two" "three"} (cmd/spop "set"))))

(deftest smove
  (is (thrown? Exception (cmd/smove "foo" "set" "one")))
  (is (thrown? Exception (cmd/smove "set" "foo" "one")))
  (cmd/sadd "set1" "two")
  (is (= false (cmd/smove "set" "set1" "four")))
  (is (= #{"two"} (cmd/smembers "set1")))
  (is (= true (cmd/smove "set" "set1" "one")))
  (is (= #{"one" "two"} (cmd/smembers "set1"))))

(deftest scard
  (is (thrown? Exception (cmd/scard "foo")))
  (is (= 3 (cmd/scard "set"))))

(deftest sismember
  (is (thrown? Exception (cmd/sismember "foo" "bar")))
  (is (= false (cmd/sismember "set" "blahonga")))
  (is (= true (cmd/sismember "set" "two"))))

(deftest sinter
  (is (thrown? Exception (cmd/sinter "foo" "set")))
  (is (= #{} (cmd/sinter "nonexistent")))
  (cmd/sadd "set1" "one")
  (cmd/sadd "set1" "four")
  (is (= #{"one" "two" "three"} (cmd/sinter "set")))
  (is (= #{"one"} (cmd/sinter "set" "set1")))
  (is (= #{} (cmd/sinter "set" "set1" "nonexistent"))))

(deftest sinterstore
  (cmd/sinterstore "foo" "set")
  (is (= #{"one" "two" "three"} (cmd/smembers "foo")))
  (cmd/sadd "set1" "one")
  (cmd/sadd "set1" "four")
  (cmd/sinterstore "newset" "set" "set1")
  (is (= #{"one"} (cmd/smembers "newset"))))

(deftest sunion
  (is (thrown? Exception (cmd/sunion "foo" "set")))
  (is (= #{} (cmd/sunion "nonexistent")))
  (cmd/sadd "set1" "one")
  (cmd/sadd "set1" "four")
  (is (= #{"one" "two" "three"} (cmd/sunion "set")))
  (is (= #{"one" "two" "three" "four"} (cmd/sunion "set" "set1")))
  (is (= #{"one" "two" "three" "four"} (cmd/sunion "set" "set1" "nonexistent"))))

(deftest sunionstore
  (cmd/sunionstore "foo" "set")
  (is (= #{"one" "two" "three"} (cmd/smembers "foo")))
  (cmd/sadd "set1" "one")
  (cmd/sadd "set1" "four")
  (cmd/sunionstore "newset" "set" "set1")
  (is (= #{"one" "two" "three" "four"} (cmd/smembers "newset"))))

(deftest sdiff
  (is (thrown? Exception (cmd/sdiff "foo" "set")))
  (is (= #{} (cmd/sdiff "nonexistent")))
  (cmd/sadd "set1" "one")
  (cmd/sadd "set1" "four")
  (is (= #{"one" "two" "three"} (cmd/sdiff "set")))
  (is (= #{"two" "three"} (cmd/sdiff "set" "set1")))
  (is (= #{"two" "three"} (cmd/sdiff "set" "set1" "nonexistent"))))

(deftest sdiffstore
  (cmd/sdiffstore "foo" "set")
  (is (= #{"one" "two" "three"} (cmd/smembers "foo")))
  (cmd/sadd "set1" "one")
  (cmd/sadd "set1" "four")
  (cmd/sdiffstore "newset" "set" "set1")
  (is (= #{"two" "three"} (cmd/smembers "newset"))))

(deftest smembers
  (is (thrown? Exception (cmd/smembers "foo")))
  (is (= #{"one" "two" "three"} (cmd/smembers "set"))))

(deftest srandmember
  (is (contains? #{"one" "two" "three"} (cmd/srandmember "set"))))

;;
;; ZSet commands
;;
(deftest zadd
  (is (thrown? Exception (cmd/zadd "foo" 1 "bar")))
  (is (= true (cmd/zadd "zset" 3.141592 "foo")))
  (is (= true (cmd/zadd "zset" -42 "bar")))
  (is (= true (cmd/zadd "zset" 2393845792384752345239485723984534589739284579348.349857983457398457934857 "baz")))
  (is (= ["bar" "foo"] (cmd/zrange "zset" 0 1))))

(deftest zrem
  (is (thrown? Exception (cmd/zrem "foo" "bar")))
  (is (= false (cmd/zrem "zset" "foobar")))
  (cmd/zadd "zset" 1.0 "one")
  (cmd/zadd "zset" 2.0 "two")
  (cmd/zadd "zset" 3.0 "three")
  (is (= true (cmd/zrem "zset" "two")))
  (is (= ["one" "three"] (cmd/zrange "zset" 0 1))))

(deftest zincrby
  (is (thrown? Exception (cmd/zincrby "foo")))
  (is (= 3.141592 (cmd/zincrby "zset" 3.141592 "value")))
  (is (= 42.141593) (cmd/zincrby "zset" 42.00001 "value"))
  (is (= 3.141592) (cmd/zincrby "zset" -42.00001 "value")))

(deftest zrank)

(deftest zrevrank)

(deftest zrange
  (is (thrown? Exception (cmd/zrange "foo")))
  (is (= [] (cmd/zrange "zset" 0 99)))
  (cmd/zadd "zset" 12349809.23873948579348750 "one")
  (cmd/zadd "zset" -42 "two")
  (cmd/zadd "zset" 3.141592 "three")
  (is (= [] (cmd/zrange "zset" -1 -2)))
  (is (= ["two" "three" "one"] (cmd/zrange "zset" 0 2)))
  (is (= ["three" "one"] (cmd/zrange "zset" 1 2))))

(deftest zrevrange
  (is (thrown? Exception (cmd/zrevrange "foo")))
  (is (= [] (cmd/zrevrange "zset" 0 99)))
  (cmd/zadd "zset" 12349809.23873948579348750 "one")
  (cmd/zadd "zset" -42 "two")
  (cmd/zadd "zset" 3.141592 "three")
  (is (= [] (cmd/zrevrange "zset" -1 -2)))
  (is (= ["one" "three" "two"] (cmd/zrevrange "zset" 0 2)))
  (is (= ["three" "two"] (cmd/zrevrange "zset" 1 2))))

(deftest zrangebyscore
  (is (thrown? Exception (cmd/zrangebyscore "foo")))
  (is (= [] (cmd/zrangebyscore "zset" 0 99)))
  (cmd/zadd "zset" 1.0 "one")
  (cmd/zadd "zset" 2.0 "two")
  (cmd/zadd "zset" 3.0 "three")
  (is (= [] (cmd/zrangebyscore "zset" -42 0.99)))
  (is (= ["two"] (cmd/zrangebyscore "zset" 1.1 2.9)))
  (is (= ["two" "three"] (cmd/zrangebyscore "zset" 1.0000001 3.00001))))

(deftest zremrangebyrank)

(deftest zremrangebyscore
  (is (thrown? Exception (cmd/zremrangebyscore "foo")))
  (is (= 0 (cmd/zremrangebyscore "zset" 0 42.4)))
  (cmd/zadd "zset" 1.0 "one")
  (cmd/zadd "zset" 2.0 "two")
  (cmd/zadd "zset" 3.0 "three")
  (is (= 1 (cmd/zremrangebyscore "zset" 2.0 2.999999))))

(deftest zcard
  (is (thrown? Exception (cmd/zcard "foo")))
  (is (= 0 (cmd/zcard "zset")))
  (cmd/zadd "zset" 1.0 "one")
  (is (= 1 (cmd/zcard "zset"))))

(deftest zscore
  (is (thrown? Exception (cmd/zscore "foo")))
  (cmd/zadd "zset" 3.141592 "pi")
  (is (= 3.141592 (cmd/zscore "zset" "pi")))
  (cmd/zadd "zset" -42 "neg")
  (is (= -42.0 (cmd/zscore "zset" "neg"))))


;;
;; Hash commands
;;
(deftest hset
  (is (thrown? Exception (cmd/hset "foo" "baz" "poe")))
  (cmd/hset "bar" "foo" "hoge")
  (is (= "hoge" (cmd/hget "bar" "foo")))

(deftest hget
  (is (= nil (cmd/hget "bar" "baz")))
  (is (= "bar" (cmd/hget "hash" "two"))))

(deftest hsetnx)

(deftest hmset
  (is (thrown? Exception (cmd/hmset "key1" "field1"))) 
  (is (thrown? Exception (cmd/hmset "key" "field" "value" "feild1")))
  (cmd/hmset "key1" "field1" "value1" "field2" "value2" "field3" "value3")
  (is (= ["value1" "value2" "value3"] (cmd/hvals "key1"))))

(deftest hmget
  (is (= ["foo"] (cmd/hmget "hash" "one")))
  (is (= ["bar" "baz"] (cmd/hmget "hash" "two" "three"))))

(deftest hincrby
  (is (= 42 (cmd/hincrby "non-exist-key" "non-exist-field" 42)))
  (is (thrown? Exception (cmd/hincrby "foo" "bar" 0)))
  (is (= 0 (cmd/hincrby "key1" "field1" 0))))
  (is (= 5 (cmd/hincrby "key1" "field1" 5))))

(deftest hexists
  (is (= true (cmd/hexists "hash" "one")))
  (is (= false (cmd/hexists "non-exist-key" "non-exist-field"))))

(deftest hdel
  (is (= false (cmd/hdel "non-exist-key" "non-exist-field")))
  (is (= true (cmd/hdel "hash" "three")))
  (is (= nil  (cmd/hget "hash" "three"))))

(deftest hlen
  (is (thrown? Exception (cmd/hlen "foo")))
  (is (= 0 (cmd/hlen "newhash")))
  (is (= 3 (cmd/hlen "hash"))))

(deftest hkeys
  (is (= [] (cmd/hkeys "noexistent")))
  (is (= ["one" "two" "three"] (cmd/hkeys "hash")))
  (cmd/hset "hash" "four" "hoge")
  (is (= 4 (count (cmd/hkeys "hash")))))

(deftest hvals
  (is (= [] (cmd/hvals "noexistent")))
  (is (= ["foo" "bar" "baz"] (cmd/hvals "hash")))
  (cmd/hdel "hash" "two")
  (is (= ["foo" "baz"] (cmd/hvals "hash"))))

(deftest hgetall
  (is (= {} (cmd/hgetall "noexistent")))
  (is (= {"one" "foo"
          "two" "bar"
          "three" "baz"}
         (cmd/hgetall "hash")))
  (cmd/hdel "hash" "one")
  (is (= {"two" "bar"
          "three" "baz"}
         (cmd/hgetall "hash"))))

;;
;; Redis Transactions: MULTI/EXEC/DISCARD/WATCH/UNWATCH
;;
(deftest multi-exec
  (cmd/set "key" "value")
  (cmd/multi)
  (is (= "QUEUED" (cmd/set "key" "blahonga")))
  (cmd/exec)
  (is (= "blahonga" (cmd/get "key"))))

(deftest multi-discard
  (cmd/set "key" "value")
  (cmd/multi)
  (is (= "QUEUED" (cmd/set "key" "blahonga")))
  (cmd/discard)
  (is (= "value" (cmd/get "key"))))

(deftest atomically
  (cmd/set "key" "value")
  (is (= ["OK" "OK" "blahong"]
       (cmd/atomically
        (cmd/set "key" "blahonga")
        (cmd/set "key2" "blahong")
        (cmd/get "key2"))))
  (is (= "blahonga" (cmd/get "key"))))

(deftest atomically-with-exception
  (cmd/set "key" "value")
  (is (thrown? Exception 
               (cmd/atomically
                (cmd/set "key" "blahonga")
                (throw (Exception. "Fail"))
                (cmd/set "key2" "blahong"))))
  (is (= "value" (cmd/get "key"))))

;; No tests for WATCH/UNWATCH yet. Waiting for stable Redis 2.1 release.

;;
;; Sorting
;;
(deftest sort
  (cmd/lpush "ids" 1)
  (cmd/lpush "ids" 4)
  (cmd/lpush "ids" 2)
  (cmd/lpush "ids" 3)
  (cmd/set "object_1" "one")
  (cmd/set "object_2" "two")
  (cmd/set "object_3" "three")
  (cmd/set "object_4" "four")
  (cmd/set "name_1" "Derek")
  (cmd/set "name_2" "Charlie")
  (cmd/set "name_3" "Bob")
  (cmd/set "name_4" "Alice")

  (is (= ["one" "two" "three"]
         (cmd/sort "list")))
  (is (= ["one" "three" "two"]
         (cmd/sort "list" :alpha)))
  (is (= ["1" "2" "3" "4"]
         (cmd/sort "ids")))
  (is (= ["1" "2" "3" "4"]
         (cmd/sort "ids" :asc)))
  (is (= ["4" "3" "2" "1"]
         (cmd/sort "ids" :desc)))
  (is (= ["1" "2"]
         (cmd/sort "ids" :asc :limit 0 2)))
  (is (= ["4" "3"]
         (cmd/sort "ids" :desc :limit 0 2)))
  (is (= ["4" "3" "2" "1"]
         (cmd/sort "ids" :by "name_*" :alpha)))
  (is (= ["one" "two" "three" "four"]
         (cmd/sort "ids" :get "object_*")))
  (is (= ["one" "two"]
           (cmd/sort "ids"
                       :by "name_*"
                       :alpha
                       :limit 0 2
                       :desc
                       :get "object_*")))
  (cmd/sort "ids"
              :by "name_*"
              :alpha
              :limit 0 2
              :desc
              :get "object_*"
              :store "result")
  (is (= ["one" "two"] (cmd/lrange "result" 0 -1))))





;; ;;
;; ;; Persistence commands
;; ;;
;; (deftest save
;;   (cmd/save))

;; (deftest bgsave
;;   (cmd/bgsave))

;; (deftest bgrewriteaof
;;   (cmd/bgrewriteaof))

;; (deftest lastsave
;;   (let [ages-ago (new java.util.Date (long 1))]
;;     (is (.before ages-ago (cmd/lastsave)))))




