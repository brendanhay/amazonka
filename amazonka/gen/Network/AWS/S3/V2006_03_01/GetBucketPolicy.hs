{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the policy of a specified bucket.
module Network.AWS.S3.V2006_03_01.GetBucketPolicy where

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


-- | Default GetBucketPolicy request.
getBucketPolicy :: BucketName -- ^ 'gbprBucket'
                -> GetBucketPolicy
getBucketPolicy p1 = GetBucketPolicy
    { gbprBucket = p1
    }

data GetBucketPolicy = GetBucketPolicy
    { gbprBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketPolicy where
    toPath GetBucketPolicy{..} = mconcat
        [ "/"
        , toBS gbprBucket
        ]

instance ToQuery GetBucketPolicy

instance ToHeaders GetBucketPolicy

instance ToBody GetBucketPolicy

instance AWSRequest GetBucketPolicy where
    type Sv GetBucketPolicy = S3

    request  = get
    response = response' $ \

data instance Rs GetBucketPolicy = GetBucketPolicyResponse
    { gbpoPolicy :: Maybe Text
      -- ^ The bucket policy as a JSON document.
    } deriving (Eq, Show, Generic)
