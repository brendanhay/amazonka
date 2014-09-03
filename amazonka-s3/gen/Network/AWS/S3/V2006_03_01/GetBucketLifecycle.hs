{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the lifecycle configuration information set on the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketLifecycle
    (
    -- * Request
      GetBucketLifecycle
    -- ** Request constructor
    , getBucketLifecycle
    -- ** Request lenses
    , gblrBucket

    -- * Response
    , GetBucketLifecycleResponse
    -- ** Response lenses
    , gbloRules
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetBucketLifecycle' request.
getBucketLifecycle :: BucketName -- ^ 'gblrBucket'
                   -> GetBucketLifecycle
getBucketLifecycle p1 = GetBucketLifecycle
    { _gblrBucket = p1
    }

data GetBucketLifecycle = GetBucketLifecycle
    { _gblrBucket :: BucketName
    } deriving (Show, Generic)

gblrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> GetBucketLifecycle
    -> f GetBucketLifecycle
gblrBucket f x =
    (\y -> x { _gblrBucket = y })
       <$> f (_gblrBucket x)
{-# INLINE gblrBucket #-}

instance ToPath GetBucketLifecycle where
    toPath GetBucketLifecycle{..} = mconcat
        [ "/"
        , toBS _gblrBucket
        ]

instance ToQuery GetBucketLifecycle where
    toQuery GetBucketLifecycle{..} = mconcat
        [ "lifecycle"
        ]

instance ToHeaders GetBucketLifecycle

instance ToBody GetBucketLifecycle

data GetBucketLifecycleResponse = GetBucketLifecycleResponse
    { _gbloRules :: [Rule]
    } deriving (Show, Generic)

gbloRules
    :: Functor f
    => ([Rule]
    -> f ([Rule]))
    -> GetBucketLifecycleResponse
    -> f GetBucketLifecycleResponse
gbloRules f x =
    (\y -> x { _gbloRules = y })
       <$> f (_gbloRules x)
{-# INLINE gbloRules #-}

instance FromXML GetBucketLifecycleResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketLifecycle where
    type Sv GetBucketLifecycle = S3
    type Rs GetBucketLifecycle = GetBucketLifecycleResponse

    request = get
    response _ = xmlResponse
