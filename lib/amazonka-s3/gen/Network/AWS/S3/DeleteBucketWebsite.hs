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
-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the website configuration for a bucket. Amazon S3 returns a @200 OK@ response upon successfully deleting a website configuration on the specified bucket. You will get a @200 OK@ response if the website configuration you are trying to delete does not exist on the bucket. Amazon S3 returns a @404@ response if the bucket specified in the request does not exist.
--
--
-- This DELETE operation requires the @S3:DeleteBucketWebsite@ permission. By default, only the bucket owner can delete the website configuration attached to a bucket. However, bucket owners can grant other users permission to delete the website configuration by writing a bucket policy granting them the @S3:DeleteBucketWebsite@ permission.
--
-- For more information about hosting websites, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> .
--
-- The following operations are related to @DeleteBucketWebsite@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketWebsite.html GetBucketWebsite>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketWebsite.html PutBucketWebsite>
module Network.AWS.S3.DeleteBucketWebsite
  ( -- * Creating a Request
    deleteBucketWebsite,
    DeleteBucketWebsite,

    -- * Request Lenses
    dbwExpectedBucketOwner,
    dbwBucket,

    -- * Destructuring the Response
    deleteBucketWebsiteResponse,
    DeleteBucketWebsiteResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketWebsite' smart constructor.
data DeleteBucketWebsite = DeleteBucketWebsite'
  { _dbwExpectedBucketOwner ::
      !(Maybe Text),
    _dbwBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketWebsite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbwExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbwBucket' - The bucket name for which you want to remove the website configuration.
deleteBucketWebsite ::
  -- | 'dbwBucket'
  BucketName ->
  DeleteBucketWebsite
deleteBucketWebsite pBucket_ =
  DeleteBucketWebsite'
    { _dbwExpectedBucketOwner = Nothing,
      _dbwBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbwExpectedBucketOwner :: Lens' DeleteBucketWebsite (Maybe Text)
dbwExpectedBucketOwner = lens _dbwExpectedBucketOwner (\s a -> s {_dbwExpectedBucketOwner = a})

-- | The bucket name for which you want to remove the website configuration.
dbwBucket :: Lens' DeleteBucketWebsite BucketName
dbwBucket = lens _dbwBucket (\s a -> s {_dbwBucket = a})

instance AWSRequest DeleteBucketWebsite where
  type Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse
  request = delete s3
  response = receiveNull DeleteBucketWebsiteResponse'

instance Hashable DeleteBucketWebsite

instance NFData DeleteBucketWebsite

instance ToHeaders DeleteBucketWebsite where
  toHeaders DeleteBucketWebsite' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbwExpectedBucketOwner]

instance ToPath DeleteBucketWebsite where
  toPath DeleteBucketWebsite' {..} = mconcat ["/", toBS _dbwBucket]

instance ToQuery DeleteBucketWebsite where
  toQuery = const (mconcat ["website"])

-- | /See:/ 'deleteBucketWebsiteResponse' smart constructor.
data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketWebsiteResponse' with the minimum fields required to make a request.
deleteBucketWebsiteResponse ::
  DeleteBucketWebsiteResponse
deleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'

instance NFData DeleteBucketWebsiteResponse
