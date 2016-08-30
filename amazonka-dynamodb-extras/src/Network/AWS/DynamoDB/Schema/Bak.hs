































-- data Credentials = Credentials
--     { _name     :: Text
--     , _version  :: Integer
--     , _revision :: ByteString
--     , _contents :: ByteString
--     }

-- instance DynamoItem Credentials where
--     -- toItem Credentials{..} =
--     --     serialize example mempty _name _version _revision _contents

--     -- fromItem = fmap unpack . deserialize example
--     --    where
--     --      -- This pattern match on ':*:' only exists because of the
--     --      -- current lack of a more familiar 'Applicative' interface:
--     --      unpack ( _name
--     --           :*: _version
--     --           :*: _revision
--     --           :*: _contents
--     --             ) = Credentials{..}


-- -- Q: how to build a de/serializer for an index?
-- -- Q: how to build scan/query/etc based on the schema/index/projections?
-- -- Q: specifying index projections
-- -- Q: additional stream specification configuration parameters in the schema?
-- -- Q: Field type is only used to add additional parameters to de/serializers - how to make it work with getKeys?
-- -- Q: Non-unique indexes are currently filtered, and are not a compile time error.

-- -- A: Type-class invariants are only checked at the top-level instance, ie. for 'Table'.

class DynamoSerializer a where
    type Serializer   a
    type Deserializer a

    serialize :: Proxy a
              -> HashMap Text Value
              -> Serializer a

    deserialize :: Proxy a
                -> HashMap Text Value
                -> Either ItemError (Deserializer a)

instance DynamoSerializer s => DynamoSerializer (Table n s o) where
    type Serializer   (Table n s o) = Serializer   s
    type Deserializer (Table n s o) = Deserializer s

    serialize   _ = serialize   (Proxy :: Proxy s)
    deserialize _ = deserialize (Proxy :: Proxy s)

instance ( DynamoSerializer s
         , KnownSymbol      n
         , DynamoValue      h
         ) => DynamoSerializer (PartitionKey n h :# s) where
    type Serializer   (PartitionKey n h :# s) = Serializer   (Attribute n h :# s)
    type Deserializer (PartitionKey n h :# s) = Deserializer (Attribute n h :# s)

    serialize   _ m = serialize   (Proxy :: Proxy (Attribute n h :# s)) m
    deserialize _ m = deserialize (Proxy :: Proxy (Attribute n h :# s)) m

instance ( KnownSymbol n
         , DynamoValue h
         ) => DynamoSerializer (PartitionKey n h) where
    type Serializer   (PartitionKey n h) = Serializer   (Attribute n h)
    type Deserializer (PartitionKey n h) = Deserializer (Attribute n h)

    serialize   _ m = serialize   (Proxy :: Proxy (Attribute n h)) m
    deserialize _ m = deserialize (Proxy :: Proxy (Attribute n h)) m

instance ( DynamoSerializer s
         , KnownSymbol      n
         , DynamoValue      r
         ) => DynamoSerializer (SortKey n r :# s) where
    type Serializer   (SortKey n r :# s) = Serializer   (Attribute n r :# s)
    type Deserializer (SortKey n r :# s) = Deserializer (Attribute n r :# s)

    serialize   _ m = serialize   (Proxy :: Proxy (Attribute n r :# s)) m
    deserialize _ m = deserialize (Proxy :: Proxy (Attribute n r :# s)) m

instance ( KnownSymbol n
         , DynamoValue r
         ) => DynamoSerializer (SortKey n r) where
    type Serializer   (SortKey n r) = Serializer   (Attribute n r)
    type Deserializer (SortKey n r) = Deserializer (Attribute n r)

    serialize   _ m = serialize   (Proxy :: Proxy (Attribute n r)) m
    deserialize _ m = deserialize (Proxy :: Proxy (Attribute n r)) m

instance ( DynamoSerializer s
         , KnownSymbol      n
         , DynamoValue      v
         ) => DynamoSerializer (Attribute n v :# s) where
    type Serializer   (Attribute n v :# s) = v ->  Serializer   s
    type Deserializer (Attribute n v :# s) = v :*: Deserializer s

    serialize _ m =
          serialize (Proxy :: Proxy s)
        . serialize (Proxy :: Proxy (Attribute n v)) m

    deserialize _ m =
        (:*:) <$> deserialize (Proxy :: Proxy (Attribute n v)) m
              <*> deserialize (Proxy :: Proxy s) m

instance ( KnownSymbol n
         , DynamoValue v
         ) => DynamoSerializer (Attribute n v) where
    type Serializer   (Attribute n v) = v -> HashMap Text Value
    type Deserializer (Attribute n v) = v

    serialize _ m v =
        uncurry HashMap.insert (attr (symbolText (Proxy :: Proxy n)) v) m

    deserialize _ =
        parse (symbolText (Proxy :: Proxy n))
