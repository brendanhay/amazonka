{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.GetBucketReplication
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | FIXME: Undocumented operation.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketReplication.html>
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.S3.Types

-- | /See:/ 'getBucketReplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrBucket'
newtype GetBucketReplication = GetBucketReplication'{_gbrBucket :: BucketName} deriving (Eq, Read, Show)

-- | 'GetBucketReplication' smart constructor.
getBucketReplication :: BucketName -> GetBucketReplication
getBucketReplication pBucket = GetBucketReplication'{_gbrBucket = pBucket};

-- | FIXME: Undocumented member.
gbrBucket :: Lens' GetBucketReplication BucketName
gbrBucket = lens _gbrBucket (\ s a -> s{_gbrBucket = a});

instance AWSRequest GetBucketReplication where
        type Sv GetBucketReplication = S3
        type Rs GetBucketReplication =
             GetBucketReplicationResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketReplicationResponse' <$>
                   x .@? "ReplicationConfiguration")

instance ToHeaders GetBucketReplication where
        toHeaders = const mempty

instance ToPath GetBucketReplication where
        toPath GetBucketReplication'{..}
          = mconcat ["/", toText _gbrBucket]

instance ToQuery GetBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'getBucketReplicationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrrReplicationConfiguration'
newtype GetBucketReplicationResponse = GetBucketReplicationResponse'{_gbrrReplicationConfiguration :: Maybe ReplicationConfiguration} deriving (Eq, Read, Show)

-- | 'GetBucketReplicationResponse' smart constructor.
getBucketReplicationResponse :: GetBucketReplicationResponse
getBucketReplicationResponse = GetBucketReplicationResponse'{_gbrrReplicationConfiguration = Nothing};

-- | FIXME: Undocumented member.
gbrrReplicationConfiguration :: Lens' GetBucketReplicationResponse (Maybe ReplicationConfiguration)
gbrrReplicationConfiguration = lens _gbrrReplicationConfiguration (\ s a -> s{_gbrrReplicationConfiguration = a});
