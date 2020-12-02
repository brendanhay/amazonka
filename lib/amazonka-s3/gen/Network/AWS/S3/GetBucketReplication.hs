{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the replication configuration of a bucket.
--
--
-- For information about replication configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- This operation requires permissions for the @s3:GetReplicationConfiguration@ action. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies> .
--
-- If you include the @Filter@ element in a replication configuration, you must also include the @DeleteMarkerReplication@ and @Priority@ elements. The response also returns those elements.
--
-- For information about @GetBucketReplication@ errors, see <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ReplicationErrorCodeList List of replication-related error codes>
--
-- The following operations are related to @GetBucketReplication@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketReplication.html PutBucketReplication>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketReplication.html DeleteBucketReplication>
module Network.AWS.S3.GetBucketReplication
  ( -- * Creating a Request
    getBucketReplication,
    GetBucketReplication,

    -- * Request Lenses
    gbrExpectedBucketOwner,
    gbrBucket,

    -- * Destructuring the Response
    getBucketReplicationResponse,
    GetBucketReplicationResponse,

    -- * Response Lenses
    gbrrsReplicationConfiguration,
    gbrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketReplication' smart constructor.
data GetBucketReplication = GetBucketReplication'
  { _gbrExpectedBucketOwner ::
      !(Maybe Text),
    _gbrBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbrBucket' - The bucket name for which to get the replication information.
getBucketReplication ::
  -- | 'gbrBucket'
  BucketName ->
  GetBucketReplication
getBucketReplication pBucket_ =
  GetBucketReplication'
    { _gbrExpectedBucketOwner = Nothing,
      _gbrBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbrExpectedBucketOwner :: Lens' GetBucketReplication (Maybe Text)
gbrExpectedBucketOwner = lens _gbrExpectedBucketOwner (\s a -> s {_gbrExpectedBucketOwner = a})

-- | The bucket name for which to get the replication information.
gbrBucket :: Lens' GetBucketReplication BucketName
gbrBucket = lens _gbrBucket (\s a -> s {_gbrBucket = a})

instance AWSRequest GetBucketReplication where
  type Rs GetBucketReplication = GetBucketReplicationResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketReplicationResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketReplication

instance NFData GetBucketReplication

instance ToHeaders GetBucketReplication where
  toHeaders GetBucketReplication' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbrExpectedBucketOwner]

instance ToPath GetBucketReplication where
  toPath GetBucketReplication' {..} = mconcat ["/", toBS _gbrBucket]

instance ToQuery GetBucketReplication where
  toQuery = const (mconcat ["replication"])

-- | /See:/ 'getBucketReplicationResponse' smart constructor.
data GetBucketReplicationResponse = GetBucketReplicationResponse'
  { _gbrrsReplicationConfiguration ::
      !(Maybe ReplicationConfiguration),
    _gbrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketReplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrrsReplicationConfiguration' - Undocumented member.
--
-- * 'gbrrsResponseStatus' - -- | The response status code.
getBucketReplicationResponse ::
  -- | 'gbrrsResponseStatus'
  Int ->
  GetBucketReplicationResponse
getBucketReplicationResponse pResponseStatus_ =
  GetBucketReplicationResponse'
    { _gbrrsReplicationConfiguration =
        Nothing,
      _gbrrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gbrrsReplicationConfiguration :: Lens' GetBucketReplicationResponse (Maybe ReplicationConfiguration)
gbrrsReplicationConfiguration = lens _gbrrsReplicationConfiguration (\s a -> s {_gbrrsReplicationConfiguration = a})

-- | -- | The response status code.
gbrrsResponseStatus :: Lens' GetBucketReplicationResponse Int
gbrrsResponseStatus = lens _gbrrsResponseStatus (\s a -> s {_gbrrsResponseStatus = a})

instance NFData GetBucketReplicationResponse
