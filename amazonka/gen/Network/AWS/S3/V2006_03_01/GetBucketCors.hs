{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the cors configuration for the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketCors where

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
-- specify the minimum viable GetBucketCors request.
getBucketCors :: BucketName -- ^ 'gbcrBucket'
              -> GetBucketCors
getBucketCors p1 = GetBucketCors
    { gbcrBucket = p1
    }

data GetBucketCors = GetBucketCors
    { gbcrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = mconcat
        [ "/"
        , toBS gbcrBucket
        ]

instance ToQuery GetBucketCors

instance ToHeaders GetBucketCors

instance ToBody GetBucketCors

instance AWSRequest GetBucketCors where
    type Sv GetBucketCors = S3

    request  = get
    response = undefined

data instance Rs GetBucketCors = GetBucketCorsResponse
    { gbcoCORSRules :: [CORSRule]
    } deriving (Eq, Show, Generic)
