{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the website configuration for a bucket.
module Network.AWS.S3.V2006_03_01.GetBucketWebsite where

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
-- specify the minimum viable GetBucketWebsite request.
getBucketWebsite :: BucketName -- ^ 'gbwrBucket'
                 -> GetBucketWebsite
getBucketWebsite p1 = GetBucketWebsite
    { gbwrBucket = p1
    }

data GetBucketWebsite = GetBucketWebsite
    { gbwrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketWebsite where
    toPath GetBucketWebsite{..} = mconcat
        [ "/"
        , toBS gbwrBucket
        ]

instance ToQuery GetBucketWebsite

instance ToHeaders GetBucketWebsite

instance ToBody GetBucketWebsite

instance AWSRequest GetBucketWebsite where
    type Sv GetBucketWebsite = S3

    request  = get
    response = undefined

data instance Rs GetBucketWebsite = GetBucketWebsiteResponse
    { gbwoErrorDocument :: Maybe ErrorDocument
    , gbwoIndexDocument :: Maybe IndexDocument
    , gbwoRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , gbwoRoutingRules :: [RoutingRule]
    } deriving (Eq, Show, Generic)
