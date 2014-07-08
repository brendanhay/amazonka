{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the logging parameters for a bucket and to specify permissions for who
-- can view and modify the logging parameters. To set the logging status of a
-- bucket, you must be the bucket owner.
module Network.AWS.S3.V2006_03_01.PutBucketLogging where

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
-- specify the minimum viable PutBucketLogging request.
putBucketLogging :: BucketLoggingStatus -- ^ 'pblrBucketLoggingStatus'
                 -> BucketName -- ^ 'pblrBucket'
                 -> PutBucketLogging
putBucketLogging p1 p2 = PutBucketLogging
    { pblrBucketLoggingStatus = p1
    , pblrBucket = p2
    , pblrContentMD5 = Nothing
    }

data PutBucketLogging = PutBucketLogging
    { pblrBucketLoggingStatus :: BucketLoggingStatus
    , pblrBucket :: BucketName
    , pblrContentMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = mconcat
        [ "/"
        , toBS pblrBucket
        ]

instance ToQuery PutBucketLogging

instance ToHeaders PutBucketLogging where
    toHeaders PutBucketLogging{..} = concat
        [ "Content-MD5" =: pblrContentMD5
        ]

instance ToBody PutBucketLogging where
    toBody = undefined -- toBody . pblrBucketLoggingStatus

instance AWSRequest PutBucketLogging where
    type Sv PutBucketLogging = S3

    request  = put
fromList [("payload",Null),("name",String "PutBucketLoggingResponse"),("shape",Object fromList [("streaming",Bool False),("location",String "body"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",Null),("documentation",Null),("common",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",Null),("documentation",Null),("location_name",Null),("xml_name",Null)]),("location_name",Null),("type",String "Text"),("xml_name",Null)]),("fields",Array (fromList []))]

data instance Rs PutBucketLogging = PutBucketLoggingResponse
    deriving (Eq, Show, Generic)
