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
-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 bucket. All objects (including all object versions and delete markers) in the bucket must be deleted before the bucket itself can be deleted.
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.DeleteBucket
  ( -- * Creating a Request
    deleteBucket,
    DeleteBucket,

    -- * Request Lenses
    dbExpectedBucketOwner,
    dbBucket,

    -- * Destructuring the Response
    deleteBucketResponse,
    DeleteBucketResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucket' smart constructor.
data DeleteBucket = DeleteBucket'
  { _dbExpectedBucketOwner ::
      !(Maybe Text),
    _dbBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbBucket' - Specifies the bucket being deleted.
deleteBucket ::
  -- | 'dbBucket'
  BucketName ->
  DeleteBucket
deleteBucket pBucket_ =
  DeleteBucket'
    { _dbExpectedBucketOwner = Nothing,
      _dbBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbExpectedBucketOwner :: Lens' DeleteBucket (Maybe Text)
dbExpectedBucketOwner = lens _dbExpectedBucketOwner (\s a -> s {_dbExpectedBucketOwner = a})

-- | Specifies the bucket being deleted.
dbBucket :: Lens' DeleteBucket BucketName
dbBucket = lens _dbBucket (\s a -> s {_dbBucket = a})

instance AWSRequest DeleteBucket where
  type Rs DeleteBucket = DeleteBucketResponse
  request = delete s3
  response = receiveNull DeleteBucketResponse'

instance Hashable DeleteBucket

instance NFData DeleteBucket

instance ToHeaders DeleteBucket where
  toHeaders DeleteBucket' {..} =
    mconcat ["x-amz-expected-bucket-owner" =# _dbExpectedBucketOwner]

instance ToPath DeleteBucket where
  toPath DeleteBucket' {..} = mconcat ["/", toBS _dbBucket]

instance ToQuery DeleteBucket where
  toQuery = const mempty

-- | /See:/ 'deleteBucketResponse' smart constructor.
data DeleteBucketResponse = DeleteBucketResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketResponse' with the minimum fields required to make a request.
deleteBucketResponse ::
  DeleteBucketResponse
deleteBucketResponse = DeleteBucketResponse'

instance NFData DeleteBucketResponse
