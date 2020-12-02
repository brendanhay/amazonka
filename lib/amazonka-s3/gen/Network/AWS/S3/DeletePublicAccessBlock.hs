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
-- Module      : Network.AWS.S3.DeletePublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketPublicAccessBlock@ permission. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
--
-- The following operations are related to @DeletePublicAccessBlock@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus>
module Network.AWS.S3.DeletePublicAccessBlock
  ( -- * Creating a Request
    deletePublicAccessBlock,
    DeletePublicAccessBlock,

    -- * Request Lenses
    dpabExpectedBucketOwner,
    dpabBucket,

    -- * Destructuring the Response
    deletePublicAccessBlockResponse,
    DeletePublicAccessBlockResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deletePublicAccessBlock' smart constructor.
data DeletePublicAccessBlock = DeletePublicAccessBlock'
  { _dpabExpectedBucketOwner ::
      !(Maybe Text),
    _dpabBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePublicAccessBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpabExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dpabBucket' - The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to delete.
deletePublicAccessBlock ::
  -- | 'dpabBucket'
  BucketName ->
  DeletePublicAccessBlock
deletePublicAccessBlock pBucket_ =
  DeletePublicAccessBlock'
    { _dpabExpectedBucketOwner = Nothing,
      _dpabBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dpabExpectedBucketOwner :: Lens' DeletePublicAccessBlock (Maybe Text)
dpabExpectedBucketOwner = lens _dpabExpectedBucketOwner (\s a -> s {_dpabExpectedBucketOwner = a})

-- | The Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to delete.
dpabBucket :: Lens' DeletePublicAccessBlock BucketName
dpabBucket = lens _dpabBucket (\s a -> s {_dpabBucket = a})

instance AWSRequest DeletePublicAccessBlock where
  type Rs DeletePublicAccessBlock = DeletePublicAccessBlockResponse
  request = delete s3
  response = receiveNull DeletePublicAccessBlockResponse'

instance Hashable DeletePublicAccessBlock

instance NFData DeletePublicAccessBlock

instance ToHeaders DeletePublicAccessBlock where
  toHeaders DeletePublicAccessBlock' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dpabExpectedBucketOwner]

instance ToPath DeletePublicAccessBlock where
  toPath DeletePublicAccessBlock' {..} =
    mconcat ["/", toBS _dpabBucket]

instance ToQuery DeletePublicAccessBlock where
  toQuery = const (mconcat ["publicAccessBlock"])

-- | /See:/ 'deletePublicAccessBlockResponse' smart constructor.
data DeletePublicAccessBlockResponse = DeletePublicAccessBlockResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePublicAccessBlockResponse' with the minimum fields required to make a request.
deletePublicAccessBlockResponse ::
  DeletePublicAccessBlockResponse
deletePublicAccessBlockResponse = DeletePublicAccessBlockResponse'

instance NFData DeletePublicAccessBlockResponse
