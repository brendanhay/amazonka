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
-- Module      : Network.AWS.S3.DeleteBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the DELETE operation removes default encryption from the bucket. For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
-- To use this operation, you must have permissions to perform the @s3:PutEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
module Network.AWS.S3.DeleteBucketEncryption
  ( -- * Creating a Request
    deleteBucketEncryption,
    DeleteBucketEncryption,

    -- * Request Lenses
    dbeExpectedBucketOwner,
    dbeBucket,

    -- * Destructuring the Response
    deleteBucketEncryptionResponse,
    DeleteBucketEncryptionResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketEncryption' smart constructor.
data DeleteBucketEncryption = DeleteBucketEncryption'
  { _dbeExpectedBucketOwner ::
      !(Maybe Text),
    _dbeBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbeExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbeBucket' - The name of the bucket containing the server-side encryption configuration to delete.
deleteBucketEncryption ::
  -- | 'dbeBucket'
  BucketName ->
  DeleteBucketEncryption
deleteBucketEncryption pBucket_ =
  DeleteBucketEncryption'
    { _dbeExpectedBucketOwner = Nothing,
      _dbeBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbeExpectedBucketOwner :: Lens' DeleteBucketEncryption (Maybe Text)
dbeExpectedBucketOwner = lens _dbeExpectedBucketOwner (\s a -> s {_dbeExpectedBucketOwner = a})

-- | The name of the bucket containing the server-side encryption configuration to delete.
dbeBucket :: Lens' DeleteBucketEncryption BucketName
dbeBucket = lens _dbeBucket (\s a -> s {_dbeBucket = a})

instance AWSRequest DeleteBucketEncryption where
  type Rs DeleteBucketEncryption = DeleteBucketEncryptionResponse
  request = delete s3
  response = receiveNull DeleteBucketEncryptionResponse'

instance Hashable DeleteBucketEncryption

instance NFData DeleteBucketEncryption

instance ToHeaders DeleteBucketEncryption where
  toHeaders DeleteBucketEncryption' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbeExpectedBucketOwner]

instance ToPath DeleteBucketEncryption where
  toPath DeleteBucketEncryption' {..} = mconcat ["/", toBS _dbeBucket]

instance ToQuery DeleteBucketEncryption where
  toQuery = const (mconcat ["encryption"])

-- | /See:/ 'deleteBucketEncryptionResponse' smart constructor.
data DeleteBucketEncryptionResponse = DeleteBucketEncryptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketEncryptionResponse' with the minimum fields required to make a request.
deleteBucketEncryptionResponse ::
  DeleteBucketEncryptionResponse
deleteBucketEncryptionResponse = DeleteBucketEncryptionResponse'

instance NFData DeleteBucketEncryptionResponse
