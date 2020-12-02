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
-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag set associated with the bucket.
--
--
-- To use this operation, you must have permission to perform the @s3:GetBucketTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
--
-- @GetBucketTagging@ has the following special error:
--
--     * Error code: @NoSuchTagSetError@
--
--     * Description: There is no tag set associated with the bucket.
--
--
--
--
--
-- The following operations are related to @GetBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging>
module Network.AWS.S3.GetBucketTagging
  ( -- * Creating a Request
    getBucketTagging,
    GetBucketTagging,

    -- * Request Lenses
    gbtExpectedBucketOwner,
    gbtBucket,

    -- * Destructuring the Response
    getBucketTaggingResponse,
    GetBucketTaggingResponse,

    -- * Response Lenses
    gbtrsResponseStatus,
    gbtrsTagSet,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketTagging' smart constructor.
data GetBucketTagging = GetBucketTagging'
  { _gbtExpectedBucketOwner ::
      !(Maybe Text),
    _gbtBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbtExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbtBucket' - The name of the bucket for which to get the tagging information.
getBucketTagging ::
  -- | 'gbtBucket'
  BucketName ->
  GetBucketTagging
getBucketTagging pBucket_ =
  GetBucketTagging'
    { _gbtExpectedBucketOwner = Nothing,
      _gbtBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbtExpectedBucketOwner :: Lens' GetBucketTagging (Maybe Text)
gbtExpectedBucketOwner = lens _gbtExpectedBucketOwner (\s a -> s {_gbtExpectedBucketOwner = a})

-- | The name of the bucket for which to get the tagging information.
gbtBucket :: Lens' GetBucketTagging BucketName
gbtBucket = lens _gbtBucket (\s a -> s {_gbtBucket = a})

instance AWSRequest GetBucketTagging where
  type Rs GetBucketTagging = GetBucketTaggingResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketTaggingResponse'
            <$> (pure (fromEnum s))
            <*> (x .@? "TagSet" .!@ mempty >>= parseXMLList "Tag")
      )

instance Hashable GetBucketTagging

instance NFData GetBucketTagging

instance ToHeaders GetBucketTagging where
  toHeaders GetBucketTagging' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbtExpectedBucketOwner]

instance ToPath GetBucketTagging where
  toPath GetBucketTagging' {..} = mconcat ["/", toBS _gbtBucket]

instance ToQuery GetBucketTagging where
  toQuery = const (mconcat ["tagging"])

-- | /See:/ 'getBucketTaggingResponse' smart constructor.
data GetBucketTaggingResponse = GetBucketTaggingResponse'
  { _gbtrsResponseStatus ::
      !Int,
    _gbtrsTagSet :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbtrsResponseStatus' - -- | The response status code.
--
-- * 'gbtrsTagSet' - Contains the tag set.
getBucketTaggingResponse ::
  -- | 'gbtrsResponseStatus'
  Int ->
  GetBucketTaggingResponse
getBucketTaggingResponse pResponseStatus_ =
  GetBucketTaggingResponse'
    { _gbtrsResponseStatus =
        pResponseStatus_,
      _gbtrsTagSet = mempty
    }

-- | -- | The response status code.
gbtrsResponseStatus :: Lens' GetBucketTaggingResponse Int
gbtrsResponseStatus = lens _gbtrsResponseStatus (\s a -> s {_gbtrsResponseStatus = a})

-- | Contains the tag set.
gbtrsTagSet :: Lens' GetBucketTaggingResponse [Tag]
gbtrsTagSet = lens _gbtrsTagSet (\s a -> s {_gbtrsTagSet = a}) . _Coerce

instance NFData GetBucketTaggingResponse
