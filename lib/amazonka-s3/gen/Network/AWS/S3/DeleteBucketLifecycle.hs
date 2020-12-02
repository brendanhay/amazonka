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
-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle configuration from the specified bucket. Amazon S3 removes all the lifecycle configuration rules in the lifecycle subresource associated with the bucket. Your objects never expire, and Amazon S3 no longer automatically deletes any objects on the basis of rules contained in the deleted lifecycle configuration.
--
--
-- To use this operation, you must have permission to perform the @s3:PutLifecycleConfiguration@ action. By default, the bucket owner has this permission and the bucket owner can grant this permission to others.
--
-- There is usually some time lag before lifecycle configuration deletion is fully propagated to all the Amazon S3 systems.
--
-- For more information about the object expiration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#intro-lifecycle-rules-actions Elements to Describe Lifecycle Actions> .
--
-- Related actions include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
module Network.AWS.S3.DeleteBucketLifecycle
  ( -- * Creating a Request
    deleteBucketLifecycle,
    DeleteBucketLifecycle,

    -- * Request Lenses
    dblExpectedBucketOwner,
    dblBucket,

    -- * Destructuring the Response
    deleteBucketLifecycleResponse,
    DeleteBucketLifecycleResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketLifecycle' smart constructor.
data DeleteBucketLifecycle = DeleteBucketLifecycle'
  { _dblExpectedBucketOwner ::
      !(Maybe Text),
    _dblBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dblExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dblBucket' - The bucket name of the lifecycle to delete.
deleteBucketLifecycle ::
  -- | 'dblBucket'
  BucketName ->
  DeleteBucketLifecycle
deleteBucketLifecycle pBucket_ =
  DeleteBucketLifecycle'
    { _dblExpectedBucketOwner = Nothing,
      _dblBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dblExpectedBucketOwner :: Lens' DeleteBucketLifecycle (Maybe Text)
dblExpectedBucketOwner = lens _dblExpectedBucketOwner (\s a -> s {_dblExpectedBucketOwner = a})

-- | The bucket name of the lifecycle to delete.
dblBucket :: Lens' DeleteBucketLifecycle BucketName
dblBucket = lens _dblBucket (\s a -> s {_dblBucket = a})

instance AWSRequest DeleteBucketLifecycle where
  type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse
  request = delete s3
  response = receiveNull DeleteBucketLifecycleResponse'

instance Hashable DeleteBucketLifecycle

instance NFData DeleteBucketLifecycle

instance ToHeaders DeleteBucketLifecycle where
  toHeaders DeleteBucketLifecycle' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dblExpectedBucketOwner]

instance ToPath DeleteBucketLifecycle where
  toPath DeleteBucketLifecycle' {..} = mconcat ["/", toBS _dblBucket]

instance ToQuery DeleteBucketLifecycle where
  toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'deleteBucketLifecycleResponse' smart constructor.
data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketLifecycleResponse' with the minimum fields required to make a request.
deleteBucketLifecycleResponse ::
  DeleteBucketLifecycleResponse
deleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'

instance NFData DeleteBucketLifecycleResponse
