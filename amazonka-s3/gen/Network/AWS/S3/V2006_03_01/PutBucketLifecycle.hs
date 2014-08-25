{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketLifecycle' request.
putBucketLifecycle :: BucketName -- ^ '_pblsBucket'
                   -> PutBucketLifecycle
putBucketLifecycle p1 = PutBucketLifecycle
    { _pblsBucket = p1
    , _pblsLifecycleConfiguration = Nothing
    , _pblsContentMD5 = Nothing
    }

data PutBucketLifecycle = PutBucketLifecycle
    { _pblsBucket :: BucketName
    , _pblsLifecycleConfiguration :: Maybe LifecycleConfiguration
    , _pblsContentMD5 :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''PutBucketLifecycle

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = mconcat
        [ "/"
        , toBS _pblsBucket
        ]

instance ToQuery PutBucketLifecycle where
    toQuery PutBucketLifecycle{..} = mconcat
        [ "lifecycle"
        ]

instance ToHeaders PutBucketLifecycle where
    toHeaders PutBucketLifecycle{..} = concat
        [ "Content-MD5" =: _pblsContentMD5
        ]

instance ToBody PutBucketLifecycle where
    toBody = toBody . encodeXML . _pblsLifecycleConfiguration

data PutBucketLifecycleResponse = PutBucketLifecycleResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutBucketLifecycleResponse

instance AWSRequest PutBucketLifecycle where
    type Sv PutBucketLifecycle = S3
    type Rs PutBucketLifecycle = PutBucketLifecycleResponse

    request = put
    response _ = nullaryResponse PutBucketLifecycleResponse
