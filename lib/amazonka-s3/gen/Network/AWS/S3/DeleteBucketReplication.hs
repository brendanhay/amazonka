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
-- Module      : Network.AWS.S3.DeleteBucketReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the replication configuration from the bucket.
--
--
-- To use this operation, you must have permissions to perform the @s3:PutReplicationConfiguration@ action. The bucket owner has these permissions by default and can grant it to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about replication configuration, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon S3 Developer Guide/ .
--
-- The following operations are related to @DeleteBucketReplication@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketReplication.html PutBucketReplication>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketReplication.html GetBucketReplication>
module Network.AWS.S3.DeleteBucketReplication
  ( -- * Creating a Request
    deleteBucketReplication,
    DeleteBucketReplication,

    -- * Request Lenses
    dbrExpectedBucketOwner,
    dbrBucket,

    -- * Destructuring the Response
    deleteBucketReplicationResponse,
    DeleteBucketReplicationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketReplication' smart constructor.
data DeleteBucketReplication = DeleteBucketReplication'
  { _dbrExpectedBucketOwner ::
      !(Maybe Text),
    _dbrBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbrBucket' - The bucket name.
deleteBucketReplication ::
  -- | 'dbrBucket'
  BucketName ->
  DeleteBucketReplication
deleteBucketReplication pBucket_ =
  DeleteBucketReplication'
    { _dbrExpectedBucketOwner = Nothing,
      _dbrBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbrExpectedBucketOwner :: Lens' DeleteBucketReplication (Maybe Text)
dbrExpectedBucketOwner = lens _dbrExpectedBucketOwner (\s a -> s {_dbrExpectedBucketOwner = a})

-- | The bucket name.
dbrBucket :: Lens' DeleteBucketReplication BucketName
dbrBucket = lens _dbrBucket (\s a -> s {_dbrBucket = a})

instance AWSRequest DeleteBucketReplication where
  type Rs DeleteBucketReplication = DeleteBucketReplicationResponse
  request = delete s3
  response = receiveNull DeleteBucketReplicationResponse'

instance Hashable DeleteBucketReplication

instance NFData DeleteBucketReplication

instance ToHeaders DeleteBucketReplication where
  toHeaders DeleteBucketReplication' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbrExpectedBucketOwner]

instance ToPath DeleteBucketReplication where
  toPath DeleteBucketReplication' {..} =
    mconcat ["/", toBS _dbrBucket]

instance ToQuery DeleteBucketReplication where
  toQuery = const (mconcat ["replication"])

-- | /See:/ 'deleteBucketReplicationResponse' smart constructor.
data DeleteBucketReplicationResponse = DeleteBucketReplicationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketReplicationResponse' with the minimum fields required to make a request.
deleteBucketReplicationResponse ::
  DeleteBucketReplicationResponse
deleteBucketReplicationResponse = DeleteBucketReplicationResponse'

instance NFData DeleteBucketReplicationResponse
