module Network.AWS.DynamoDB.Mapper.Value.Unsafe
    ( Value (..)
    ) where

import Network.AWS.DynamoDB (AttributeValue)

newtype Value = Value { getValue :: AttributeValue }
    deriving (Eq, Show)
