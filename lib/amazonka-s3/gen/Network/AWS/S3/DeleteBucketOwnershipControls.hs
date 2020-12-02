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
-- Module      : Network.AWS.S3.DeleteBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
--
-- The following operations are related to @DeleteBucketOwnershipControls@ :
--
--     * 'GetBucketOwnershipControls'
--
--     * 'PutBucketOwnershipControls'
module Network.AWS.S3.DeleteBucketOwnershipControls
  ( -- * Creating a Request
    deleteBucketOwnershipControls,
    DeleteBucketOwnershipControls,

    -- * Request Lenses
    dbocExpectedBucketOwner,
    dbocBucket,

    -- * Destructuring the Response
    deleteBucketOwnershipControlsResponse,
    DeleteBucketOwnershipControlsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketOwnershipControls' smart constructor.
data DeleteBucketOwnershipControls = DeleteBucketOwnershipControls'
  { _dbocExpectedBucketOwner ::
      !(Maybe Text),
    _dbocBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketOwnershipControls' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbocExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbocBucket' - The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
deleteBucketOwnershipControls ::
  -- | 'dbocBucket'
  BucketName ->
  DeleteBucketOwnershipControls
deleteBucketOwnershipControls pBucket_ =
  DeleteBucketOwnershipControls'
    { _dbocExpectedBucketOwner =
        Nothing,
      _dbocBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbocExpectedBucketOwner :: Lens' DeleteBucketOwnershipControls (Maybe Text)
dbocExpectedBucketOwner = lens _dbocExpectedBucketOwner (\s a -> s {_dbocExpectedBucketOwner = a})

-- | The Amazon S3 bucket whose @OwnershipControls@ you want to delete.
dbocBucket :: Lens' DeleteBucketOwnershipControls BucketName
dbocBucket = lens _dbocBucket (\s a -> s {_dbocBucket = a})

instance AWSRequest DeleteBucketOwnershipControls where
  type
    Rs DeleteBucketOwnershipControls =
      DeleteBucketOwnershipControlsResponse
  request = delete s3
  response = receiveNull DeleteBucketOwnershipControlsResponse'

instance Hashable DeleteBucketOwnershipControls

instance NFData DeleteBucketOwnershipControls

instance ToHeaders DeleteBucketOwnershipControls where
  toHeaders DeleteBucketOwnershipControls' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbocExpectedBucketOwner]

instance ToPath DeleteBucketOwnershipControls where
  toPath DeleteBucketOwnershipControls' {..} =
    mconcat ["/", toBS _dbocBucket]

instance ToQuery DeleteBucketOwnershipControls where
  toQuery = const (mconcat ["ownershipControls"])

-- | /See:/ 'deleteBucketOwnershipControlsResponse' smart constructor.
data DeleteBucketOwnershipControlsResponse = DeleteBucketOwnershipControlsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketOwnershipControlsResponse' with the minimum fields required to make a request.
deleteBucketOwnershipControlsResponse ::
  DeleteBucketOwnershipControlsResponse
deleteBucketOwnershipControlsResponse =
  DeleteBucketOwnershipControlsResponse'

instance NFData DeleteBucketOwnershipControlsResponse
