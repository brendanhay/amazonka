{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the website configuration for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketWebsite where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable PutBucketWebsite request.
putBucketWebsite :: WebsiteConfiguration -- ^ 'pbwrWebsiteConfiguration'
                 -> BucketName -- ^ 'pbwrBucket'
                 -> PutBucketWebsite
putBucketWebsite p1 p2 = PutBucketWebsite
    { pbwrWebsiteConfiguration = p1
    , pbwrBucket = p2
    , pbwrContentMD5 = Nothing
    }

data PutBucketWebsite = PutBucketWebsite
    { pbwrWebsiteConfiguration :: WebsiteConfiguration
    , pbwrBucket :: BucketName
    , pbwrContentMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketWebsite where
    toPath PutBucketWebsite{..} = mconcat
        [ "/"
        , toBS pbwrBucket
        ]

instance ToQuery PutBucketWebsite

instance ToHeaders PutBucketWebsite where
    toHeaders PutBucketWebsite{..} = concat
        [ "Content-MD5" =: pbwrContentMD5
        ]

instance ToBody PutBucketWebsite where
    toBody = undefined -- toBody . pbwrWebsiteConfiguration

instance AWSRequest PutBucketWebsite where
    type Sv PutBucketWebsite = S3

    request  = put
    response = undefined

data instance Rs PutBucketWebsite = PutBucketWebsiteResponse
    deriving (Eq, Show, Generic)
