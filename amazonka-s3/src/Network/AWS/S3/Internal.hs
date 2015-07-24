{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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

import           Data.String
import           Network.AWS.Data.XML
import           Network.AWS.Prelude
import           Network.AWS.Request  (contentMD5)

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

newtype ObjectKey = ObjectKey Text
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

-- Taken from: https://github.com/aws/aws-sdk-ruby/blob/17508581a6887fb170e608242165b1ee18ccd9a9/aws-sdk-core/lib/aws-sdk-core/plugins/s3_md5s.rb#L15
checksumMD5 :: Request a -> Request a
checksumMD5 rq =
    case _rqOperation rq of
        "DeleteObjects"      -> contentMD5 rq
        "PutBucketCORS"      -> contentMD5 rq
        "PutBucketLifecycle" -> contentMD5 rq
        "PutBucketPolicy"    -> contentMD5 rq
        "PutBucketTagging"   -> contentMD5 rq
        _                    -> rq
