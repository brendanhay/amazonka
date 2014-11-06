{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketVersioning
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
module Network.AWS.S3.PutBucketVersioning
    (
    -- * Request
      PutBucketVersioning
    -- ** Request constructor
    , putBucketVersioning
    -- ** Request lenses
    , pbvrBucket
    , pbvrContentMD5
    , pbvrMFA
    , pbvrVersioningConfiguration

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data PutBucketVersioning = PutBucketVersioning
    { _pbvrBucket                  :: BucketName
    , _pbvrContentMD5              :: Maybe Text
    , _pbvrMFA                     :: Maybe Text
    , _pbvrVersioningConfiguration :: VersioningConfiguration
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketVersioning' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbvrBucket' @::@ 'BucketName'
--
-- * 'pbvrContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbvrMFA' @::@ 'Maybe' 'Text'
--
-- * 'pbvrVersioningConfiguration' @::@ 'VersioningConfiguration'
--
putBucketVersioning :: BucketName -- ^ 'pbvrBucket'
                    -> VersioningConfiguration -- ^ 'pbvrVersioningConfiguration'
                    -> PutBucketVersioning
putBucketVersioning p1 p2 = PutBucketVersioning
    { _pbvrBucket                  = p1
    , _pbvrVersioningConfiguration = p2
    , _pbvrContentMD5              = Nothing
    , _pbvrMFA                     = Nothing
    }

pbvrBucket :: Lens' PutBucketVersioning BucketName
pbvrBucket = lens _pbvrBucket (\s a -> s { _pbvrBucket = a })

pbvrContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvrContentMD5 = lens _pbvrContentMD5 (\s a -> s { _pbvrContentMD5 = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
pbvrMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvrMFA = lens _pbvrMFA (\s a -> s { _pbvrMFA = a })

pbvrVersioningConfiguration :: Lens' PutBucketVersioning VersioningConfiguration
pbvrVersioningConfiguration =
    lens _pbvrVersioningConfiguration
        (\s a -> s { _pbvrVersioningConfiguration = a })

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = mconcat
        [ "/"
        , toText _pbvrBucket
        ]

instance ToQuery PutBucketVersioning where
    toQuery = const "versioning"

instance ToHeaders PutBucketVersioning where
    toHeaders PutBucketVersioning{..} = mconcat
        [ "Content-MD5" =: _pbvrContentMD5
        , "x-amz-mfa"   =: _pbvrMFA
        ]

instance ToBody PutBucketVersioning where
    toBody = toBody . encodeXML . _pbvrVersioningConfiguration

instance AWSRequest PutBucketVersioning where
    type Sv PutBucketVersioning = S3
    type Rs PutBucketVersioning = Empty

    request  = put
    response = const (nullaryResponse Empty)
