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
-- Module      : Network.AWS.S3.GetBucketLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the logging status of a bucket and the permissions users have to view and modify that status. To use GET, you must be the bucket owner.
--
--
-- The following operations are related to @GetBucketLogging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLogging.html PutBucketLogging>
module Network.AWS.S3.GetBucketLogging
  ( -- * Creating a Request
    getBucketLogging,
    GetBucketLogging,

    -- * Request Lenses
    gExpectedBucketOwner,
    gBucket,

    -- * Destructuring the Response
    getBucketLoggingResponse,
    GetBucketLoggingResponse,

    -- * Response Lenses
    gblrsLoggingEnabled,
    gblrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketLogging' smart constructor.
data GetBucketLogging = GetBucketLogging'
  { _gExpectedBucketOwner ::
      !(Maybe Text),
    _gBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gBucket' - The bucket name for which to get the logging information.
getBucketLogging ::
  -- | 'gBucket'
  BucketName ->
  GetBucketLogging
getBucketLogging pBucket_ =
  GetBucketLogging'
    { _gExpectedBucketOwner = Nothing,
      _gBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gExpectedBucketOwner :: Lens' GetBucketLogging (Maybe Text)
gExpectedBucketOwner = lens _gExpectedBucketOwner (\s a -> s {_gExpectedBucketOwner = a})

-- | The bucket name for which to get the logging information.
gBucket :: Lens' GetBucketLogging BucketName
gBucket = lens _gBucket (\s a -> s {_gBucket = a})

instance AWSRequest GetBucketLogging where
  type Rs GetBucketLogging = GetBucketLoggingResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketLoggingResponse'
            <$> (x .@? "LoggingEnabled") <*> (pure (fromEnum s))
      )

instance Hashable GetBucketLogging

instance NFData GetBucketLogging

instance ToHeaders GetBucketLogging where
  toHeaders GetBucketLogging' {..} =
    mconcat ["x-amz-expected-bucket-owner" =# _gExpectedBucketOwner]

instance ToPath GetBucketLogging where
  toPath GetBucketLogging' {..} = mconcat ["/", toBS _gBucket]

instance ToQuery GetBucketLogging where
  toQuery = const (mconcat ["logging"])

-- | /See:/ 'getBucketLoggingResponse' smart constructor.
data GetBucketLoggingResponse = GetBucketLoggingResponse'
  { _gblrsLoggingEnabled ::
      !(Maybe LoggingEnabled),
    _gblrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketLoggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gblrsLoggingEnabled' - Undocumented member.
--
-- * 'gblrsResponseStatus' - -- | The response status code.
getBucketLoggingResponse ::
  -- | 'gblrsResponseStatus'
  Int ->
  GetBucketLoggingResponse
getBucketLoggingResponse pResponseStatus_ =
  GetBucketLoggingResponse'
    { _gblrsLoggingEnabled = Nothing,
      _gblrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gblrsLoggingEnabled :: Lens' GetBucketLoggingResponse (Maybe LoggingEnabled)
gblrsLoggingEnabled = lens _gblrsLoggingEnabled (\s a -> s {_gblrsLoggingEnabled = a})

-- | -- | The response status code.
gblrsResponseStatus :: Lens' GetBucketLoggingResponse Int
gblrsResponseStatus = lens _gblrsResponseStatus (\s a -> s {_gblrsResponseStatus = a})

instance NFData GetBucketLoggingResponse
