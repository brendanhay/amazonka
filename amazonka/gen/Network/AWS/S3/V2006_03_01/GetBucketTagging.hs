{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketTagging where

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
-- specify the minimum viable GetBucketTagging request.
getBucketTagging :: BucketName -- ^ 'gbtrBucket'
                 -> GetBucketTagging
getBucketTagging p1 = GetBucketTagging
    { gbtrBucket = p1
    }

data GetBucketTagging = GetBucketTagging
    { gbtrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = mconcat
        [ "/"
        , toBS gbtrBucket
        ]

instance ToQuery GetBucketTagging

instance ToHeaders GetBucketTagging

instance ToBody GetBucketTagging

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3

    request  = get
    response = response' undefined

data instance Rs GetBucketTagging = GetBucketTaggingResponse
    { gbtoTagSet :: [Tag]
    } deriving (Eq, Show, Generic)
