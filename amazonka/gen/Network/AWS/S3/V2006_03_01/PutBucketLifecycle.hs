{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets lifecycle configuration for your bucket. If a lifecycle configuration
-- exists, it replaces it.
module Network.AWS.S3.V2006_03_01.PutBucketLifecycle where

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


-- | Default PutBucketLifecycle request.
putBucketLifecycle :: BucketName -- ^ 'pblrBucket'
                   -> LifecycleConfiguration -- ^ 'pblrLifecycleConfiguration'
                   -> PutBucketLifecycle
putBucketLifecycle p1 p2 = PutBucketLifecycle
    { pblrBucket = p1
    , pblrLifecycleConfiguration = p2
    , pblrContentMD5 = Nothing
    }

data PutBucketLifecycle = PutBucketLifecycle
    { pblrBucket :: BucketName
    , pblrLifecycleConfiguration :: LifecycleConfiguration
    , pblrContentMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = mconcat
        [ "/"
        , toBS pblrBucket
        ]

instance ToQuery PutBucketLifecycle

instance ToHeaders PutBucketLifecycle where
    toHeaders PutBucketLifecycle{..} = concat
        [ "Content-MD5" =: pblrContentMD5
        ]

instance ToBody PutBucketLifecycle where
    toBody = undefined -- toBody . pblrLifecycleConfiguration

instance AWSRequest PutBucketLifecycle where
    type Sv PutBucketLifecycle = S3

    request  = put
    response = headerResposne $ const PutBucketLifecycleResponse

data instance Rs PutBucketLifecycle = PutBucketLifecycleResponse
    deriving (Eq, Show, Generic)
