{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- Module      : Network.AWS.S3.Internal
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Internal
    ( module Network.AWS.S3.Internal
    , Region
    ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.String
import           Network.AWS.Data.XML
import           Network.AWS.Prelude

default (ByteString)

newtype BucketName = BucketName Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

newtype ObjectVersionId = ObjectVersionId Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

-- FIXME: Add the difference between weak + strong ETags and their respective
-- equalities if necessary, see: https://github.com/brendanhay/amazonka/issues/76
newtype ETag = ETag ByteString
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

type Delimiter = Char

data ObjectKey
    = DecodedKey !Char [ByteString]
    | EncodedKey ByteString
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance IsString ObjectKey where
    fromString = objectKey '/' . fromString

instance FromText ObjectKey where
    parser = EncodedKey . toBS <$> takeText

instance FromXML ObjectKey where
    parseXML = parseXMLText "ObjectKey"

instance ToByteString ObjectKey where toBS    = encoded
instance ToText       ObjectKey where toText  = toText  . encoded
instance ToQuery      ObjectKey where toQuery = toQuery . encoded
instance ToBuilder    ObjectKey where build   = build   . encoded
instance ToXML        ObjectKey where toXML   = toXMLText

objectKey :: Char -> ByteString -> ObjectKey
objectKey c = DecodedKey c . BS8.split c

encoded :: ObjectKey -> ByteString
encoded = \case
    EncodedKey bs   -> bs
    DecodedKey c xs ->
        BS8.intercalate (BS8.singleton c) $
            map (urlEncode False) xs

decoded :: ObjectKey -> Either (Char -> ByteString) ByteString
decoded = \case
    DecodedKey c xs -> Right $ BS8.intercalate (BS8.singleton c) xs
    EncodedKey bs   -> Left  $ \c ->
        let w = toEnum (fromEnum c)
         in BS.intercalate (BS.singleton w) $
                map (urlDecode False) (BS.split w bs)
