{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the versioning state of an existing bucket. To set the versioning state,
-- you must be the bucket owner.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketVersioning.html>
module Network.AWS.S3.PutBucketVersioning
    (
    -- * Request
      PutBucketVersioning
    -- ** Request constructor
    , putBucketVersioning
    -- ** Request lenses
    , pbvBucket
    , pbvContentMD5
    , pbvMFA
    , pbvVersioningConfiguration

    -- * Response
    , PutBucketVersioningResponse
    -- ** Response constructor
    , putBucketVersioningResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketVersioning = PutBucketVersioning
    { _pbvBucket                  :: Text
    , _pbvContentMD5              :: Maybe Text
    , _pbvMFA                     :: Maybe Text
    , _pbvVersioningConfiguration :: VersioningConfiguration
    } deriving (Eq, Show)

-- | 'PutBucketVersioning' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbvBucket' @::@ 'Text'
--
-- * 'pbvContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbvMFA' @::@ 'Maybe' 'Text'
--
-- * 'pbvVersioningConfiguration' @::@ 'VersioningConfiguration'
--
putBucketVersioning :: Text -- ^ 'pbvBucket'
                    -> VersioningConfiguration -- ^ 'pbvVersioningConfiguration'
                    -> PutBucketVersioning
putBucketVersioning p1 p2 = PutBucketVersioning
    { _pbvBucket                  = p1
    , _pbvVersioningConfiguration = p2
    , _pbvContentMD5              = Nothing
    , _pbvMFA                     = Nothing
    }

pbvBucket :: Lens' PutBucketVersioning Text
pbvBucket = lens _pbvBucket (\s a -> s { _pbvBucket = a })

pbvContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvContentMD5 = lens _pbvContentMD5 (\s a -> s { _pbvContentMD5 = a })

-- | The concatenation of the authentication device's serial number, a space, and
-- the value that is displayed on your authentication device.
pbvMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvMFA = lens _pbvMFA (\s a -> s { _pbvMFA = a })

pbvVersioningConfiguration :: Lens' PutBucketVersioning VersioningConfiguration
pbvVersioningConfiguration =
    lens _pbvVersioningConfiguration
        (\s a -> s { _pbvVersioningConfiguration = a })

data PutBucketVersioningResponse = PutBucketVersioningResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketVersioningResponse' constructor.
putBucketVersioningResponse :: PutBucketVersioningResponse
putBucketVersioningResponse = PutBucketVersioningResponse

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = mconcat
        [ "/"
        , toText _pbvBucket
        ]

instance ToQuery PutBucketVersioning where
    toQuery = const "versioning"

instance ToHeaders PutBucketVersioning where
    toHeaders PutBucketVersioning{..} = mconcat
        [ "Content-MD5" =: _pbvContentMD5
        , "x-amz-mfa"   =: _pbvMFA
        ]

instance ToXMLRoot PutBucketVersioning where
    toXMLRoot PutBucketVersioning{..} = namespaced ns "PutBucketVersioning"
        [ "VersioningConfiguration" =@ _pbvVersioningConfiguration
        ]

instance ToXML PutBucketVersioning

instance AWSRequest PutBucketVersioning where
    type Sv PutBucketVersioning = S3
    type Rs PutBucketVersioning = PutBucketVersioningResponse

    request  = put
    response = nullResponse PutBucketVersioningResponse
