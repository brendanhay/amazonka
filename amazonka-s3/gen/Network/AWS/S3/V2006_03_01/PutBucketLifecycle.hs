{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.PutBucketLifecycle
    (
    -- * Request
      PutBucketLifecycle
    -- ** Request constructor
    , putBucketLifecycle
    -- ** Request lenses
    , pblrBucket
    , pblrLifecycleConfiguration
    , pblrContentMD5

    -- * Response
    , PutBucketLifecycleResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketLifecycle' request.
putBucketLifecycle :: BucketName -- ^ 'pblrBucket'
                   -> PutBucketLifecycle
putBucketLifecycle p1 = PutBucketLifecycle
    { _pblrBucket = p1
    , _pblrLifecycleConfiguration = Nothing
    , _pblrContentMD5 = Nothing
    }

data PutBucketLifecycle = PutBucketLifecycle
    { _pblrBucket :: BucketName
    , _pblrLifecycleConfiguration :: Maybe LifecycleConfiguration
    , _pblrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pblrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> PutBucketLifecycle
    -> f PutBucketLifecycle
pblrBucket f x =
    (\y -> x { _pblrBucket = y })
       <$> f (_pblrBucket x)
{-# INLINE pblrBucket #-}

pblrLifecycleConfiguration
    :: Functor f
    => (Maybe LifecycleConfiguration
    -> f (Maybe LifecycleConfiguration))
    -> PutBucketLifecycle
    -> f PutBucketLifecycle
pblrLifecycleConfiguration f x =
    (\y -> x { _pblrLifecycleConfiguration = y })
       <$> f (_pblrLifecycleConfiguration x)
{-# INLINE pblrLifecycleConfiguration #-}

pblrContentMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutBucketLifecycle
    -> f PutBucketLifecycle
pblrContentMD5 f x =
    (\y -> x { _pblrContentMD5 = y })
       <$> f (_pblrContentMD5 x)
{-# INLINE pblrContentMD5 #-}

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = mconcat
        [ "/"
        , toBS _pblrBucket
        ]

instance ToQuery PutBucketLifecycle where
    toQuery PutBucketLifecycle{..} = mconcat
        [ "lifecycle"
        ]

instance ToHeaders PutBucketLifecycle where
    toHeaders PutBucketLifecycle{..} = concat
        [ "Content-MD5" =: _pblrContentMD5
        ]

instance ToBody PutBucketLifecycle where
    toBody = toBody . encodeXML . _pblrLifecycleConfiguration

data PutBucketLifecycleResponse = PutBucketLifecycleResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketLifecycle where
    type Sv PutBucketLifecycle = S3
    type Rs PutBucketLifecycle = PutBucketLifecycleResponse

    request = put
    response _ = nullaryResponse PutBucketLifecycleResponse
