{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Mapper.Attributes where

import Data.ByteString    (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy         (Proxy (..))
import Data.Semigroup     ((<>))
import Data.Text          (Text)

import GHC.Exts     (Constraint)
import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Mapper.Types

class DynamoScalarType a where
    getScalarType :: Proxy a -> ScalarAttributeType

instance DynamoScalarType Text       where getScalarType = const S
instance DynamoScalarType Integer    where getScalarType = const N
instance DynamoScalarType ByteString where getScalarType = const B

class DynamoAttributes a where
    getAttributes :: Proxy a -> NonEmpty AttributeDefinition

instance DynamoAttributes s => DynamoAttributes (Table n s o) where
    getAttributes _ = getAttributes (Proxy :: Proxy s)

instance ( DynamoAttributes a
         , DynamoAttributes b
         ) => DynamoAttributes (a :# b) where
    getAttributes _ =
           getAttributes (Proxy :: Proxy a)
        <> getAttributes (Proxy :: Proxy b)

instance ( KnownSymbol      n
         , DynamoScalarType v
         ) => DynamoAttributes (Attribute n v) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy v))

type family HasAttributes a b where
    HasAttributes a b =
        MissingAttributesError (NonIntersecting (GetNames a) (GetNames b)) b

type family MissingAttributesError e a :: Constraint where
    MissingAttributesError '[]  a = ()
    MissingAttributesError '[e] a =
           (TypeError
               ('Text "An attribute matching the name: "
                   ':<>: 'ShowType e
                   ':$$: 'Text "is not defined in the subsequent schema: "
                   ':<>: 'ShowType a))
    MissingAttributesError  es  a =
           (TypeError
               ('Text "Attributes matching the names: "
                   ':<>: 'ShowType es
                   ':$$: 'Text "are not defined in the subsequent schema: "
                   ':<>: 'ShowType a))

type family GetNames a :: [Symbol] where
    GetNames (Key       n k) = '[n]
    GetNames (Attribute n v) = '[n]
    GetNames (a :# b)        = GetNames a ++ GetNames b
    GetNames _               = '[]
