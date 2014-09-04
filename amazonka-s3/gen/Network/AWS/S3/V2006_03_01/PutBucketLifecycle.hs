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
    , mkPutBucketLifecycleRequest
    -- ** Request lenses
    , pblrBucket
    , pblrContentMD5
    , pblrLifecycleConfiguration

    -- * Response
    , PutBucketLifecycleResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketLifecycle' request.
mkPutBucketLifecycleRequest :: BucketName -- ^ 'pblrBucket'
                            -> PutBucketLifecycle
mkPutBucketLifecycleRequest p1 = PutBucketLifecycle
    { _pblrBucket = p1
    , _pblrContentMD5 = Nothing
    , _pblrLifecycleConfiguration = Nothing
    }
{-# INLINE mkPutBucketLifecycleRequest #-}

data PutBucketLifecycle = PutBucketLifecycle
    { _pblrBucket :: BucketName
    , _pblrContentMD5 :: Maybe Text
    , _pblrLifecycleConfiguration :: Maybe LifecycleConfiguration
    } deriving (Show, Generic)

pblrBucket :: Lens' PutBucketLifecycle (BucketName)
pblrBucket = lens _pblrBucket (\s a -> s { _pblrBucket = a })
{-# INLINE pblrBucket #-}

pblrContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
pblrContentMD5 = lens _pblrContentMD5 (\s a -> s { _pblrContentMD5 = a })
{-# INLINE pblrContentMD5 #-}

pblrLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
pblrLifecycleConfiguration = lens _pblrLifecycleConfiguration (\s a -> s { _pblrLifecycleConfiguration = a })
{-# INLINE pblrLifecycleConfiguration #-}

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = mconcat
        [ "/"
        , toBS _pblrBucket
        ]

instance ToQuery PutBucketLifecycle where
    toQuery PutBucketLifecycle{..} = mconcat
        [ "lifecycle"
        ]

instance ToHeaders PutBucketLifecycle

instance ToBody PutBucketLifecycle

data PutBucketLifecycleResponse = PutBucketLifecycleResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketLifecycle where
    type Sv PutBucketLifecycle = S3
    type Rs PutBucketLifecycle = PutBucketLifecycleResponse

    request = put
    response _ = nullaryResponse PutBucketLifecycleResponse
