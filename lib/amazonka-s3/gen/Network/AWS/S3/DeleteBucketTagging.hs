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
-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the tags from the bucket.
--
--
-- To use this operation, you must have permission to perform the @s3:PutBucketTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
--
-- The following operations are related to @DeleteBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
module Network.AWS.S3.DeleteBucketTagging
  ( -- * Creating a Request
    deleteBucketTagging,
    DeleteBucketTagging,

    -- * Request Lenses
    dbtExpectedBucketOwner,
    dbtBucket,

    -- * Destructuring the Response
    deleteBucketTaggingResponse,
    DeleteBucketTaggingResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketTagging' smart constructor.
data DeleteBucketTagging = DeleteBucketTagging'
  { _dbtExpectedBucketOwner ::
      !(Maybe Text),
    _dbtBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbtExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbtBucket' - The bucket that has the tag set to be removed.
deleteBucketTagging ::
  -- | 'dbtBucket'
  BucketName ->
  DeleteBucketTagging
deleteBucketTagging pBucket_ =
  DeleteBucketTagging'
    { _dbtExpectedBucketOwner = Nothing,
      _dbtBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbtExpectedBucketOwner :: Lens' DeleteBucketTagging (Maybe Text)
dbtExpectedBucketOwner = lens _dbtExpectedBucketOwner (\s a -> s {_dbtExpectedBucketOwner = a})

-- | The bucket that has the tag set to be removed.
dbtBucket :: Lens' DeleteBucketTagging BucketName
dbtBucket = lens _dbtBucket (\s a -> s {_dbtBucket = a})

instance AWSRequest DeleteBucketTagging where
  type Rs DeleteBucketTagging = DeleteBucketTaggingResponse
  request = delete s3
  response = receiveNull DeleteBucketTaggingResponse'

instance Hashable DeleteBucketTagging

instance NFData DeleteBucketTagging

instance ToHeaders DeleteBucketTagging where
  toHeaders DeleteBucketTagging' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbtExpectedBucketOwner]

instance ToPath DeleteBucketTagging where
  toPath DeleteBucketTagging' {..} = mconcat ["/", toBS _dbtBucket]

instance ToQuery DeleteBucketTagging where
  toQuery = const (mconcat ["tagging"])

-- | /See:/ 'deleteBucketTaggingResponse' smart constructor.
data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketTaggingResponse' with the minimum fields required to make a request.
deleteBucketTaggingResponse ::
  DeleteBucketTaggingResponse
deleteBucketTaggingResponse = DeleteBucketTaggingResponse'

instance NFData DeleteBucketTaggingResponse
