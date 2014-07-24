{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the tags for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketTagging where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Default PutBucketTagging request.
putBucketTagging :: Tagging -- ^ '_pbtrTagging'
                 -> BucketName -- ^ '_pbtrBucket'
                 -> PutBucketTagging
putBucketTagging p1 p2 = PutBucketTagging
    { _pbtrTagging = p1
    , _pbtrBucket = p2
    , _pbtrContentMD5 = Nothing
    }

data PutBucketTagging = PutBucketTagging
    { _pbtrTagging :: Tagging
    , _pbtrBucket :: BucketName
    , _pbtrContentMD5 :: Maybe Text
    } deriving (Generic)

instance ToPath PutBucketTagging where
    toPath PutBucketTagging{..} = mconcat
        [ "/"
        , toBS _pbtrBucket
        ]

instance ToQuery PutBucketTagging

instance ToHeaders PutBucketTagging where
    toHeaders PutBucketTagging{..} = concat
        [ "Content-MD5" =: _pbtrContentMD5
        ]

instance ToBody PutBucketTagging where
    toBody = toBody . encodeXML . _pbtrTagging

instance AWSRequest PutBucketTagging where
    type Sv PutBucketTagging = S3
    type Rs PutBucketTagging = PutBucketTaggingResponse

    request = put

    response _ _ = return (Right PutBucketTaggingResponse)

data PutBucketTaggingResponse = PutBucketTaggingResponse
    deriving (Eq, Show, Generic)
