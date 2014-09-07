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
    , mkPutBucketVersioning
    -- ** Request lenses
    , pbvBucket
    , pbvContentMD5
    , pbvMFA
    , pbvVersioningConfiguration

    -- * Response
    , PutBucketVersioningResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutBucketVersioning = PutBucketVersioning
    { _pbvBucket :: BucketName
    , _pbvContentMD5 :: Maybe Text
    , _pbvMFA :: Maybe Text
    , _pbvVersioningConfiguration :: VersioningConfiguration
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketVersioning' request.
mkPutBucketVersioning :: BucketName -- ^ 'pbvBucket'
                      -> VersioningConfiguration -- ^ 'pbvVersioningConfiguration'
                      -> PutBucketVersioning
mkPutBucketVersioning p1 p4 = PutBucketVersioning
    { _pbvBucket = p1
    , _pbvContentMD5 = Nothing
    , _pbvMFA = Nothing
    , _pbvVersioningConfiguration = p4
    }

pbvBucket :: Lens' PutBucketVersioning BucketName
pbvBucket = lens _pbvBucket (\s a -> s { _pbvBucket = a })

pbvContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvContentMD5 = lens _pbvContentMD5 (\s a -> s { _pbvContentMD5 = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
pbvMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvMFA = lens _pbvMFA (\s a -> s { _pbvMFA = a })

pbvVersioningConfiguration :: Lens' PutBucketVersioning VersioningConfiguration
pbvVersioningConfiguration =
    lens _pbvVersioningConfiguration
         (\s a -> s { _pbvVersioningConfiguration = a })

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = mconcat
        [ "/"
        , toBS _pbvBucket
        ]

instance ToQuery PutBucketVersioning where
    toQuery PutBucketVersioning{..} = mconcat
        [ "versioning"
        ]

instance ToHeaders PutBucketVersioning where
    toHeaders PutBucketVersioning{..} = concat
        [ "Content-MD5" =: _pbvContentMD5
        , "x-amz-mfa" =: _pbvMFA
        ]

instance ToBody PutBucketVersioning where
    toBody = toBody . encodeXML . _pbvVersioningConfiguration

data PutBucketVersioningResponse = PutBucketVersioningResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketVersioning where
    type Sv PutBucketVersioning = S3
    type Rs PutBucketVersioning = PutBucketVersioningResponse

    request = put
    response _ = nullaryResponse PutBucketVersioningResponse
