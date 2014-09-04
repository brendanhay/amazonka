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
{-# INLINE putBucketLifecycle #-}

data PutBucketLifecycle = PutBucketLifecycle
    { _pblrBucket :: BucketName
    , _pblrLifecycleConfiguration :: Maybe LifecycleConfiguration
    , _pblrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pblrBucket :: Lens' PutBucketLifecycle (BucketName)
pblrBucket f x =
    f (_pblrBucket x)
        <&> \y -> x { _pblrBucket = y }
{-# INLINE pblrBucket #-}

pblrLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
pblrLifecycleConfiguration f x =
    f (_pblrLifecycleConfiguration x)
        <&> \y -> x { _pblrLifecycleConfiguration = y }
{-# INLINE pblrLifecycleConfiguration #-}

pblrContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
pblrContentMD5 f x =
    f (_pblrContentMD5 x)
        <&> \y -> x { _pblrContentMD5 = y }
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
