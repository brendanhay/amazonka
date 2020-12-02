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
-- Module      : Network.AWS.S3.GetBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:GetBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
--
-- The following operations are related to @GetBucketOwnershipControls@ :
--
--     * 'PutBucketOwnershipControls'
--
--     * 'DeleteBucketOwnershipControls'
module Network.AWS.S3.GetBucketOwnershipControls
  ( -- * Creating a Request
    getBucketOwnershipControls,
    GetBucketOwnershipControls,

    -- * Request Lenses
    gbocExpectedBucketOwner,
    gbocBucket,

    -- * Destructuring the Response
    getBucketOwnershipControlsResponse,
    GetBucketOwnershipControlsResponse,

    -- * Response Lenses
    gbocrsOwnershipControls,
    gbocrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketOwnershipControls' smart constructor.
data GetBucketOwnershipControls = GetBucketOwnershipControls'
  { _gbocExpectedBucketOwner ::
      !(Maybe Text),
    _gbocBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketOwnershipControls' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbocExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbocBucket' - The name of the Amazon S3 bucket whose @OwnershipControls@ you want to retrieve.
getBucketOwnershipControls ::
  -- | 'gbocBucket'
  BucketName ->
  GetBucketOwnershipControls
getBucketOwnershipControls pBucket_ =
  GetBucketOwnershipControls'
    { _gbocExpectedBucketOwner = Nothing,
      _gbocBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbocExpectedBucketOwner :: Lens' GetBucketOwnershipControls (Maybe Text)
gbocExpectedBucketOwner = lens _gbocExpectedBucketOwner (\s a -> s {_gbocExpectedBucketOwner = a})

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to retrieve.
gbocBucket :: Lens' GetBucketOwnershipControls BucketName
gbocBucket = lens _gbocBucket (\s a -> s {_gbocBucket = a})

instance AWSRequest GetBucketOwnershipControls where
  type
    Rs GetBucketOwnershipControls =
      GetBucketOwnershipControlsResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketOwnershipControlsResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketOwnershipControls

instance NFData GetBucketOwnershipControls

instance ToHeaders GetBucketOwnershipControls where
  toHeaders GetBucketOwnershipControls' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbocExpectedBucketOwner]

instance ToPath GetBucketOwnershipControls where
  toPath GetBucketOwnershipControls' {..} =
    mconcat ["/", toBS _gbocBucket]

instance ToQuery GetBucketOwnershipControls where
  toQuery = const (mconcat ["ownershipControls"])

-- | /See:/ 'getBucketOwnershipControlsResponse' smart constructor.
data GetBucketOwnershipControlsResponse = GetBucketOwnershipControlsResponse'
  { _gbocrsOwnershipControls ::
      !( Maybe
           OwnershipControls
       ),
    _gbocrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketOwnershipControlsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbocrsOwnershipControls' - The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) currently in effect for this Amazon S3 bucket.
--
-- * 'gbocrsResponseStatus' - -- | The response status code.
getBucketOwnershipControlsResponse ::
  -- | 'gbocrsResponseStatus'
  Int ->
  GetBucketOwnershipControlsResponse
getBucketOwnershipControlsResponse pResponseStatus_ =
  GetBucketOwnershipControlsResponse'
    { _gbocrsOwnershipControls =
        Nothing,
      _gbocrsResponseStatus = pResponseStatus_
    }

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) currently in effect for this Amazon S3 bucket.
gbocrsOwnershipControls :: Lens' GetBucketOwnershipControlsResponse (Maybe OwnershipControls)
gbocrsOwnershipControls = lens _gbocrsOwnershipControls (\s a -> s {_gbocrsOwnershipControls = a})

-- | -- | The response status code.
gbocrsResponseStatus :: Lens' GetBucketOwnershipControlsResponse Int
gbocrsResponseStatus = lens _gbocrsResponseStatus (\s a -> s {_gbocrsResponseStatus = a})

instance NFData GetBucketOwnershipControlsResponse
