{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , gbrrqBucket

    -- * Response
    , GetBucketReplicationResponse
    -- ** Response constructor
    , getBucketReplicationResponse
    -- ** Response lenses
    , gbrrsReplicationConfiguration
    , gbrrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketReplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrrqBucket'
newtype GetBucketReplication = GetBucketReplication'
    { _gbrrqBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketReplication' smart constructor.
getBucketReplication :: BucketName -> GetBucketReplication
getBucketReplication pBucket =
    GetBucketReplication'
    { _gbrrqBucket = pBucket
    }

-- | FIXME: Undocumented member.
gbrrqBucket :: Lens' GetBucketReplication BucketName
gbrrqBucket = lens _gbrrqBucket (\ s a -> s{_gbrrqBucket = a});

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
          = mconcat ["/", toText _gbrrqBucket]

instance ToQuery GetBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'getBucketReplicationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbrrsReplicationConfiguration'
--
-- * 'gbrrsStatus'
data GetBucketReplicationResponse = GetBucketReplicationResponse'
    { _gbrrsReplicationConfiguration :: !(Maybe ReplicationConfiguration)
    , _gbrrsStatus                   :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketReplicationResponse' smart constructor.
getBucketReplicationResponse :: Int -> GetBucketReplicationResponse
getBucketReplicationResponse pStatus =
    GetBucketReplicationResponse'
    { _gbrrsReplicationConfiguration = Nothing
    , _gbrrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
gbrrsReplicationConfiguration :: Lens' GetBucketReplicationResponse (Maybe ReplicationConfiguration)
gbrrsReplicationConfiguration = lens _gbrrsReplicationConfiguration (\ s a -> s{_gbrrsReplicationConfiguration = a});

-- | FIXME: Undocumented member.
gbrrsStatus :: Lens' GetBucketReplicationResponse Int
gbrrsStatus = lens _gbrrsStatus (\ s a -> s{_gbrrsStatus = a});
