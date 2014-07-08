{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
module Network.AWS.S3.V2006_03_01.PutBucketVersioning where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable PutBucketVersioning request.
putBucketVersioning :: VersioningConfiguration -- ^ 'pbvrVersioningConfiguration'
                    -> BucketName -- ^ 'pbvrBucket'
                    -> PutBucketVersioning
putBucketVersioning p1 p2 = PutBucketVersioning
    { pbvrVersioningConfiguration = p1
    , pbvrBucket = p2
    , pbvrContentMD5 = Nothing
    , pbvrMFA = Nothing
    }

data PutBucketVersioning = PutBucketVersioning
    { pbvrVersioningConfiguration :: VersioningConfiguration
    , pbvrBucket :: BucketName
    , pbvrContentMD5 :: Maybe Text
    , pbvrMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = mconcat
        [ "/"
        , toBS pbvrBucket
        ]

instance ToQuery PutBucketVersioning

instance ToHeaders PutBucketVersioning where
    toHeaders PutBucketVersioning{..} = concat
        [ "Content-MD5" =: pbvrContentMD5
        , "x-amz-mfa" =: pbvrMFA
        ]

instance ToBody PutBucketVersioning where
    toBody = undefined -- toBody . pbvrVersioningConfiguration

instance AWSRequest PutBucketVersioning where
    type Sv PutBucketVersioning = S3

    request  = put
fromList [("payload",Null),("name",String "PutBucketVersioningResponse"),("shape",Object fromList [("streaming",Bool False),("location",String "body"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",Null),("documentation",Null),("common",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",Null),("documentation",Null),("location_name",Null),("xml_name",Null)]),("location_name",Null),("type",String "Text"),("xml_name",Null)]),("fields",Array (fromList []))]

data instance Rs PutBucketVersioning = PutBucketVersioningResponse
    deriving (Eq, Show, Generic)
