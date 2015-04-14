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

-- Module      : Network.AWS.S3.GetBucketReplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketReplication.html>
module Network.AWS.S3.GetBucketReplication
    (
    -- * Request
      GetBucketReplication
    -- ** Request constructor
    , getBucketReplication
    -- ** Request lenses
    , gbrBucket

    -- * Response
    , GetBucketReplicationResponse
    -- ** Response constructor
    , getBucketReplicationResponse
    -- ** Response lenses
    , gbrrReplicationConfiguration
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketReplication = GetBucketReplication
    { _gbrBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetBucketReplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrBucket' @::@ 'Text'
--
getBucketReplication :: Text -- ^ 'gbrBucket'
                     -> GetBucketReplication
getBucketReplication p1 = GetBucketReplication
    { _gbrBucket = p1
    }

gbrBucket :: Lens' GetBucketReplication Text
gbrBucket = lens _gbrBucket (\s a -> s { _gbrBucket = a })

newtype GetBucketReplicationResponse = GetBucketReplicationResponse
    { _gbrrReplicationConfiguration :: Maybe ReplicationConfiguration
    } deriving (Eq, Read, Show)

-- | 'GetBucketReplicationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrrReplicationConfiguration' @::@ 'Maybe' 'ReplicationConfiguration'
--
getBucketReplicationResponse :: GetBucketReplicationResponse
getBucketReplicationResponse = GetBucketReplicationResponse
    { _gbrrReplicationConfiguration = Nothing
    }

gbrrReplicationConfiguration :: Lens' GetBucketReplicationResponse (Maybe ReplicationConfiguration)
gbrrReplicationConfiguration =
    lens _gbrrReplicationConfiguration
        (\s a -> s { _gbrrReplicationConfiguration = a })

instance ToPath GetBucketReplication where
    toPath GetBucketReplication{..} = mconcat
        [ "/"
        , toText _gbrBucket
        ]

instance ToQuery GetBucketReplication where
    toQuery = const "replication"

instance ToHeaders GetBucketReplication

instance ToXMLRoot GetBucketReplication where
    toXMLRoot = const (namespaced ns "GetBucketReplication" [])

instance ToXML GetBucketReplication

instance AWSRequest GetBucketReplication where
    type Sv GetBucketReplication = S3
    type Rs GetBucketReplication = GetBucketReplicationResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketReplicationResponse where
    parseXML x = GetBucketReplicationResponse
        <$> x .@? "ReplicationConfiguration"
