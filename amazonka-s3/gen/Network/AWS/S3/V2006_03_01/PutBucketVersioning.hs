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
    , putBucketVersioning
    -- ** Request lenses
    , pbvrVersioningConfiguration
    , pbvrBucket
    , pbvrContentMD5
    , pbvrMFA

    -- * Response
    , PutBucketVersioningResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketVersioning' request.
putBucketVersioning :: VersioningConfiguration -- ^ 'pbvrVersioningConfiguration'
                    -> BucketName -- ^ 'pbvrBucket'
                    -> PutBucketVersioning
putBucketVersioning p1 p2 = PutBucketVersioning
    { _pbvrVersioningConfiguration = p1
    , _pbvrBucket = p2
    , _pbvrContentMD5 = Nothing
    , _pbvrMFA = Nothing
    }
{-# INLINE putBucketVersioning #-}

data PutBucketVersioning = PutBucketVersioning
    { _pbvrVersioningConfiguration :: VersioningConfiguration
    , _pbvrBucket :: BucketName
    , _pbvrContentMD5 :: Maybe Text
    , _pbvrMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    } deriving (Show, Generic)

pbvrVersioningConfiguration :: Lens' PutBucketVersioning (VersioningConfiguration)
pbvrVersioningConfiguration f x =
    f (_pbvrVersioningConfiguration x)
        <&> \y -> x { _pbvrVersioningConfiguration = y }
{-# INLINE pbvrVersioningConfiguration #-}

pbvrBucket :: Lens' PutBucketVersioning (BucketName)
pbvrBucket f x =
    f (_pbvrBucket x)
        <&> \y -> x { _pbvrBucket = y }
{-# INLINE pbvrBucket #-}

pbvrContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvrContentMD5 f x =
    f (_pbvrContentMD5 x)
        <&> \y -> x { _pbvrContentMD5 = y }
{-# INLINE pbvrContentMD5 #-}

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
pbvrMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvrMFA f x =
    f (_pbvrMFA x)
        <&> \y -> x { _pbvrMFA = y }
{-# INLINE pbvrMFA #-}

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = mconcat
        [ "/"
        , toBS _pbvrBucket
        ]

instance ToQuery PutBucketVersioning where
    toQuery PutBucketVersioning{..} = mconcat
        [ "versioning"
        ]

instance ToHeaders PutBucketVersioning where
    toHeaders PutBucketVersioning{..} = concat
        [ "Content-MD5" =: _pbvrContentMD5
        , "x-amz-mfa" =: _pbvrMFA
        ]

instance ToBody PutBucketVersioning where
    toBody = toBody . encodeXML . _pbvrVersioningConfiguration

data PutBucketVersioningResponse = PutBucketVersioningResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketVersioning where
    type Sv PutBucketVersioning = S3
    type Rs PutBucketVersioning = PutBucketVersioningResponse

    request = put
    response _ = nullaryResponse PutBucketVersioningResponse
