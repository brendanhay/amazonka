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
-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Region the bucket resides in. You set the bucket's Region using the @LocationConstraint@ request parameter in a @CreateBucket@ request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> .
--
--
-- To use this implementation of the operation, you must be the bucket owner.
--
-- The following operations are related to @GetBucketLocation@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Network.AWS.S3.GetBucketLocation
  ( -- * Creating a Request
    getBucketLocation,
    GetBucketLocation,

    -- * Request Lenses
    gblExpectedBucketOwner,
    gblBucket,

    -- * Destructuring the Response
    getBucketLocationResponse,
    GetBucketLocationResponse,

    -- * Response Lenses
    gblbrsResponseStatus,
    gblbrsLocationConstraint,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketLocation' smart constructor.
data GetBucketLocation = GetBucketLocation'
  { _gblExpectedBucketOwner ::
      !(Maybe Text),
    _gblBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gblExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gblBucket' - The name of the bucket for which to get the location.
getBucketLocation ::
  -- | 'gblBucket'
  BucketName ->
  GetBucketLocation
getBucketLocation pBucket_ =
  GetBucketLocation'
    { _gblExpectedBucketOwner = Nothing,
      _gblBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gblExpectedBucketOwner :: Lens' GetBucketLocation (Maybe Text)
gblExpectedBucketOwner = lens _gblExpectedBucketOwner (\s a -> s {_gblExpectedBucketOwner = a})

-- | The name of the bucket for which to get the location.
gblBucket :: Lens' GetBucketLocation BucketName
gblBucket = lens _gblBucket (\s a -> s {_gblBucket = a})

instance AWSRequest GetBucketLocation where
  type Rs GetBucketLocation = GetBucketLocationResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketLocationResponse'
            <$> (pure (fromEnum s)) <*> (parseXML x)
      )

instance Hashable GetBucketLocation

instance NFData GetBucketLocation

instance ToHeaders GetBucketLocation where
  toHeaders GetBucketLocation' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gblExpectedBucketOwner]

instance ToPath GetBucketLocation where
  toPath GetBucketLocation' {..} = mconcat ["/", toBS _gblBucket]

instance ToQuery GetBucketLocation where
  toQuery = const (mconcat ["location"])

-- | /See:/ 'getBucketLocationResponse' smart constructor.
data GetBucketLocationResponse = GetBucketLocationResponse'
  { _gblbrsResponseStatus ::
      !Int,
    _gblbrsLocationConstraint ::
      !LocationConstraint
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketLocationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gblbrsResponseStatus' - -- | The response status code.
--
-- * 'gblbrsLocationConstraint' - Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
getBucketLocationResponse ::
  -- | 'gblbrsResponseStatus'
  Int ->
  -- | 'gblbrsLocationConstraint'
  LocationConstraint ->
  GetBucketLocationResponse
getBucketLocationResponse pResponseStatus_ pLocationConstraint_ =
  GetBucketLocationResponse'
    { _gblbrsResponseStatus =
        pResponseStatus_,
      _gblbrsLocationConstraint = pLocationConstraint_
    }

-- | -- | The response status code.
gblbrsResponseStatus :: Lens' GetBucketLocationResponse Int
gblbrsResponseStatus = lens _gblbrsResponseStatus (\s a -> s {_gblbrsResponseStatus = a})

-- | Specifies the Region where the bucket resides. For a list of all the Amazon S3 supported location constraints by Region, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints> . Buckets in Region @us-east-1@ have a LocationConstraint of @null@ .
gblbrsLocationConstraint :: Lens' GetBucketLocationResponse LocationConstraint
gblbrsLocationConstraint = lens _gblbrsLocationConstraint (\s a -> s {_gblbrsLocationConstraint = a})

instance NFData GetBucketLocationResponse
