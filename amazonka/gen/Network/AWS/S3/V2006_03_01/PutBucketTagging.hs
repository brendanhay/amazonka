{-# LANGUAGE DeriveGeneric               #-}
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


-- | Default PutBucketTagging request.
putBucketTagging :: Tagging -- ^ 'pbtrTagging'
                 -> BucketName -- ^ 'pbtrBucket'
                 -> PutBucketTagging
putBucketTagging p1 p2 = PutBucketTagging
    { pbtrTagging = p1
    , pbtrBucket = p2
    , pbtrContentMD5 = Nothing
    }

data PutBucketTagging = PutBucketTagging
    { pbtrTagging :: Tagging
    , pbtrBucket :: BucketName
    , pbtrContentMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketTagging where
    toPath PutBucketTagging{..} = mconcat
        [ "/"
        , toBS pbtrBucket
        ]

instance ToQuery PutBucketTagging

instance ToHeaders PutBucketTagging where
    toHeaders PutBucketTagging{..} = concat
        [ "Content-MD5" =: pbtrContentMD5
        ]

instance ToBody PutBucketTagging where
    toBody = undefined -- toBody . pbtrTagging

instance AWSRequest PutBucketTagging where
    type Sv PutBucketTagging = S3

    request  = put
    response = headerResposne $ const PutBucketTaggingResponse

data instance Rs PutBucketTagging = PutBucketTaggingResponse
    deriving (Eq, Show, Generic)
