{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketReplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- FIXME: Undocumented operation.
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
    , gbrrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketReplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrBucket'
newtype GetBucketReplication = GetBucketReplication'
    { _gbrBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketReplication' smart constructor.
getBucketReplication :: BucketName -> GetBucketReplication
getBucketReplication pBucket =
    GetBucketReplication'
    { _gbrBucket = pBucket
    }

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
                   (parseXML x) <*> (pure (fromEnum s)))

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
--
-- * 'gbrrStatus'
data GetBucketReplicationResponse = GetBucketReplicationResponse'
    { _gbrrReplicationConfiguration :: !(Maybe ReplicationConfiguration)
    , _gbrrStatus                   :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketReplicationResponse' smart constructor.
getBucketReplicationResponse :: Int -> GetBucketReplicationResponse
getBucketReplicationResponse pStatus =
    GetBucketReplicationResponse'
    { _gbrrReplicationConfiguration = Nothing
    , _gbrrStatus = pStatus
    }

-- | FIXME: Undocumented member.
gbrrReplicationConfiguration :: Lens' GetBucketReplicationResponse (Maybe ReplicationConfiguration)
gbrrReplicationConfiguration = lens _gbrrReplicationConfiguration (\ s a -> s{_gbrrReplicationConfiguration = a});

-- | FIXME: Undocumented member.
gbrrStatus :: Lens' GetBucketReplicationResponse Int
gbrrStatus = lens _gbrrStatus (\ s a -> s{_gbrrStatus = a});
