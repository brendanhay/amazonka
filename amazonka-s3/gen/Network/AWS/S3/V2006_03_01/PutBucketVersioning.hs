{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
module Network.AWS.S3.V2006_03_01.PutBucketVersioning
    (
    -- * Request
      PutBucketVersioning
    -- ** Request constructor
    , mkPutBucketVersioningRequest
    -- ** Request lenses
    , pbvrBucket
    , pbvrContentMD5
    , pbvrMFA
    , pbvrVersioningConfiguration

    -- * Response
    , PutBucketVersioningResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketVersioning' request.
mkPutBucketVersioningRequest :: BucketName -- ^ 'pbvrBucket'
                             -> VersioningConfiguration -- ^ 'pbvrVersioningConfiguration'
                             -> PutBucketVersioning
mkPutBucketVersioningRequest p1 p2 = PutBucketVersioning
    { _pbvrBucket = p1
    , _pbvrContentMD5 = Nothing
    , _pbvrMFA = Nothing
    , _pbvrVersioningConfiguration = p4
    }
{-# INLINE mkPutBucketVersioningRequest #-}

data PutBucketVersioning = PutBucketVersioning
    { _pbvrBucket :: BucketName
    , _pbvrContentMD5 :: Maybe Text
    , _pbvrMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    , _pbvrVersioningConfiguration :: VersioningConfiguration
    } deriving (Show, Generic)

pbvrBucket :: Lens' PutBucketVersioning (BucketName)
pbvrBucket = lens _pbvrBucket (\s a -> s { _pbvrBucket = a })
{-# INLINE pbvrBucket #-}

pbvrContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvrContentMD5 = lens _pbvrContentMD5 (\s a -> s { _pbvrContentMD5 = a })
{-# INLINE pbvrContentMD5 #-}

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
pbvrMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvrMFA = lens _pbvrMFA (\s a -> s { _pbvrMFA = a })
{-# INLINE pbvrMFA #-}

pbvrVersioningConfiguration :: Lens' PutBucketVersioning (VersioningConfiguration)
pbvrVersioningConfiguration = lens _pbvrVersioningConfiguration (\s a -> s { _pbvrVersioningConfiguration = a })
{-# INLINE pbvrVersioningConfiguration #-}

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = mconcat
        [ "/"
        , toBS _pbvrBucket
        ]

instance ToQuery PutBucketVersioning where
    toQuery PutBucketVersioning{..} = mconcat
        [ "versioning"
        ]

instance ToHeaders PutBucketVersioning

instance ToBody PutBucketVersioning

data PutBucketVersioningResponse = PutBucketVersioningResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketVersioning where
    type Sv PutBucketVersioning = S3
    type Rs PutBucketVersioning = PutBucketVersioningResponse

    request = put
    response _ = nullaryResponse PutBucketVersioningResponse
