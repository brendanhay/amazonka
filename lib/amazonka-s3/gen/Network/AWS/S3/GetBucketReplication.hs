{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketReplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the replication configuration of a bucket.
module Network.AWS.S3.GetBucketReplication
    (
    -- * Creating a Request
      getBucketReplication
    , GetBucketReplication
    -- * Request Lenses
    , gbrBucket

    -- * Destructuring the Response
    , getBucketReplicationResponse
    , GetBucketReplicationResponse
    -- * Response Lenses
    , gbrrsReplicationConfiguration
    , gbrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketReplication' smart constructor.
newtype GetBucketReplication = GetBucketReplication'
  { _gbrBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrBucket' - Undocumented member.
getBucketReplication
    :: BucketName -- ^ 'gbrBucket'
    -> GetBucketReplication
getBucketReplication pBucket_ = GetBucketReplication' {_gbrBucket = pBucket_}


-- | Undocumented member.
gbrBucket :: Lens' GetBucketReplication BucketName
gbrBucket = lens _gbrBucket (\ s a -> s{_gbrBucket = a})

instance AWSRequest GetBucketReplication where
        type Rs GetBucketReplication =
             GetBucketReplicationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketReplicationResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetBucketReplication where

instance NFData GetBucketReplication where

instance ToHeaders GetBucketReplication where
        toHeaders = const mempty

instance ToPath GetBucketReplication where
        toPath GetBucketReplication'{..}
          = mconcat ["/", toBS _gbrBucket]

instance ToQuery GetBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'getBucketReplicationResponse' smart constructor.
data GetBucketReplicationResponse = GetBucketReplicationResponse'
  { _gbrrsReplicationConfiguration :: !(Maybe ReplicationConfiguration)
  , _gbrrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketReplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrrsReplicationConfiguration' - Undocumented member.
--
-- * 'gbrrsResponseStatus' - -- | The response status code.
getBucketReplicationResponse
    :: Int -- ^ 'gbrrsResponseStatus'
    -> GetBucketReplicationResponse
getBucketReplicationResponse pResponseStatus_ =
  GetBucketReplicationResponse'
    { _gbrrsReplicationConfiguration = Nothing
    , _gbrrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gbrrsReplicationConfiguration :: Lens' GetBucketReplicationResponse (Maybe ReplicationConfiguration)
gbrrsReplicationConfiguration = lens _gbrrsReplicationConfiguration (\ s a -> s{_gbrrsReplicationConfiguration = a})

-- | -- | The response status code.
gbrrsResponseStatus :: Lens' GetBucketReplicationResponse Int
gbrrsResponseStatus = lens _gbrrsResponseStatus (\ s a -> s{_gbrrsResponseStatus = a})

instance NFData GetBucketReplicationResponse where
