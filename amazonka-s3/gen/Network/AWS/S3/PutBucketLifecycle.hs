{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketLifecycle
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
module Network.AWS.S3.PutBucketLifecycle
    (
    -- * Request
      PutBucketLifecycle
    -- ** Request constructor
    , putBucketLifecycle
    -- ** Request lenses
    , pblrBucket
    , pblrContentMD5
    , pblrLifecycleConfiguration

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

data PutBucketLifecycle = PutBucketLifecycle
    { _pblrBucket                 :: BucketName
    , _pblrContentMD5             :: Maybe Text
    , _pblrLifecycleConfiguration :: Maybe LifecycleConfiguration
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketLifecycle' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pblrBucket' @::@ 'BucketName'
--
-- * 'pblrContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pblrLifecycleConfiguration' @::@ 'Maybe' 'LifecycleConfiguration'
--
putBucketLifecycle :: BucketName -- ^ 'pblrBucket'
                   -> PutBucketLifecycle
putBucketLifecycle p1 = PutBucketLifecycle
    { _pblrBucket                 = p1
    , _pblrContentMD5             = Nothing
    , _pblrLifecycleConfiguration = Nothing
    }

pblrBucket :: Lens' PutBucketLifecycle BucketName
pblrBucket = lens _pblrBucket (\s a -> s { _pblrBucket = a })

pblrContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
pblrContentMD5 = lens _pblrContentMD5 (\s a -> s { _pblrContentMD5 = a })

pblrLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
pblrLifecycleConfiguration =
    lens _pblrLifecycleConfiguration
        (\s a -> s { _pblrLifecycleConfiguration = a })

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = mconcat
        [ "/"
        , toText _pblrBucket
        ]

instance ToQuery PutBucketLifecycle where
    toQuery = const "lifecycle"

instance ToHeaders PutBucketLifecycle where
    toHeaders PutBucketLifecycle{..} = mconcat
        [ "Content-MD5" =: _pblrContentMD5
        ]

instance AWSRequest PutBucketLifecycle where
    type Sv PutBucketLifecycle = S3
    type Rs PutBucketLifecycle = Empty

    request  = put
    response = const (nullaryResponse Empty)
