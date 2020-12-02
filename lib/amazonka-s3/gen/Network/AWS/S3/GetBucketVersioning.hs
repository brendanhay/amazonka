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
-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the versioning state of a bucket.
--
--
-- To retrieve the versioning state of a bucket, you must be the bucket owner.
--
-- This implementation also returns the MFA Delete status of the versioning state. If the MFA Delete status is @enabled@ , the bucket owner must use an authentication device to change the versioning state of the bucket.
--
-- The following operations are related to @GetBucketVersioning@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.GetBucketVersioning
  ( -- * Creating a Request
    getBucketVersioning,
    GetBucketVersioning,

    -- * Request Lenses
    gbvExpectedBucketOwner,
    gbvBucket,

    -- * Destructuring the Response
    getBucketVersioningResponse,
    GetBucketVersioningResponse,

    -- * Response Lenses
    gbvrsStatus,
    gbvrsMFADelete,
    gbvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketVersioning' smart constructor.
data GetBucketVersioning = GetBucketVersioning'
  { _gbvExpectedBucketOwner ::
      !(Maybe Text),
    _gbvBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketVersioning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbvBucket' - The name of the bucket for which to get the versioning information.
getBucketVersioning ::
  -- | 'gbvBucket'
  BucketName ->
  GetBucketVersioning
getBucketVersioning pBucket_ =
  GetBucketVersioning'
    { _gbvExpectedBucketOwner = Nothing,
      _gbvBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbvExpectedBucketOwner :: Lens' GetBucketVersioning (Maybe Text)
gbvExpectedBucketOwner = lens _gbvExpectedBucketOwner (\s a -> s {_gbvExpectedBucketOwner = a})

-- | The name of the bucket for which to get the versioning information.
gbvBucket :: Lens' GetBucketVersioning BucketName
gbvBucket = lens _gbvBucket (\s a -> s {_gbvBucket = a})

instance AWSRequest GetBucketVersioning where
  type Rs GetBucketVersioning = GetBucketVersioningResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketVersioningResponse'
            <$> (x .@? "Status") <*> (x .@? "MfaDelete") <*> (pure (fromEnum s))
      )

instance Hashable GetBucketVersioning

instance NFData GetBucketVersioning

instance ToHeaders GetBucketVersioning where
  toHeaders GetBucketVersioning' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbvExpectedBucketOwner]

instance ToPath GetBucketVersioning where
  toPath GetBucketVersioning' {..} = mconcat ["/", toBS _gbvBucket]

instance ToQuery GetBucketVersioning where
  toQuery = const (mconcat ["versioning"])

-- | /See:/ 'getBucketVersioningResponse' smart constructor.
data GetBucketVersioningResponse = GetBucketVersioningResponse'
  { _gbvrsStatus ::
      !(Maybe BucketVersioningStatus),
    _gbvrsMFADelete ::
      !(Maybe MFADeleteStatus),
    _gbvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketVersioningResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvrsStatus' - The versioning state of the bucket.
--
-- * 'gbvrsMFADelete' - Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
--
-- * 'gbvrsResponseStatus' - -- | The response status code.
getBucketVersioningResponse ::
  -- | 'gbvrsResponseStatus'
  Int ->
  GetBucketVersioningResponse
getBucketVersioningResponse pResponseStatus_ =
  GetBucketVersioningResponse'
    { _gbvrsStatus = Nothing,
      _gbvrsMFADelete = Nothing,
      _gbvrsResponseStatus = pResponseStatus_
    }

-- | The versioning state of the bucket.
gbvrsStatus :: Lens' GetBucketVersioningResponse (Maybe BucketVersioningStatus)
gbvrsStatus = lens _gbvrsStatus (\s a -> s {_gbvrsStatus = a})

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
gbvrsMFADelete :: Lens' GetBucketVersioningResponse (Maybe MFADeleteStatus)
gbvrsMFADelete = lens _gbvrsMFADelete (\s a -> s {_gbvrsMFADelete = a})

-- | -- | The response status code.
gbvrsResponseStatus :: Lens' GetBucketVersioningResponse Int
gbvrsResponseStatus = lens _gbvrsResponseStatus (\s a -> s {_gbvrsResponseStatus = a})

instance NFData GetBucketVersioningResponse
