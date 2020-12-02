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
-- Module      : Network.AWS.S3.GetBucketCORS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cors configuration information set for the bucket.
--
--
-- To use this operation, you must have permission to perform the s3:GetBucketCORS action. By default, the bucket owner has this permission and can grant it to others.
--
-- For more information about cors, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> .
--
-- The following operations are related to @GetBucketCors@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors>
module Network.AWS.S3.GetBucketCORS
  ( -- * Creating a Request
    getBucketCORS,
    GetBucketCORS,

    -- * Request Lenses
    gbcExpectedBucketOwner,
    gbcBucket,

    -- * Destructuring the Response
    getBucketCORSResponse,
    GetBucketCORSResponse,

    -- * Response Lenses
    gbcrsCORSRules,
    gbcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketCORS' smart constructor.
data GetBucketCORS = GetBucketCORS'
  { _gbcExpectedBucketOwner ::
      !(Maybe Text),
    _gbcBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketCORS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbcExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbcBucket' - The bucket name for which to get the cors configuration.
getBucketCORS ::
  -- | 'gbcBucket'
  BucketName ->
  GetBucketCORS
getBucketCORS pBucket_ =
  GetBucketCORS'
    { _gbcExpectedBucketOwner = Nothing,
      _gbcBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbcExpectedBucketOwner :: Lens' GetBucketCORS (Maybe Text)
gbcExpectedBucketOwner = lens _gbcExpectedBucketOwner (\s a -> s {_gbcExpectedBucketOwner = a})

-- | The bucket name for which to get the cors configuration.
gbcBucket :: Lens' GetBucketCORS BucketName
gbcBucket = lens _gbcBucket (\s a -> s {_gbcBucket = a})

instance AWSRequest GetBucketCORS where
  type Rs GetBucketCORS = GetBucketCORSResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketCORSResponse'
            <$> (may (parseXMLList "CORSRule") x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketCORS

instance NFData GetBucketCORS

instance ToHeaders GetBucketCORS where
  toHeaders GetBucketCORS' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbcExpectedBucketOwner]

instance ToPath GetBucketCORS where
  toPath GetBucketCORS' {..} = mconcat ["/", toBS _gbcBucket]

instance ToQuery GetBucketCORS where
  toQuery = const (mconcat ["cors"])

-- | /See:/ 'getBucketCORSResponse' smart constructor.
data GetBucketCORSResponse = GetBucketCORSResponse'
  { _gbcrsCORSRules ::
      !(Maybe [CORSRule]),
    _gbcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketCORSResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbcrsCORSRules' - A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
--
-- * 'gbcrsResponseStatus' - -- | The response status code.
getBucketCORSResponse ::
  -- | 'gbcrsResponseStatus'
  Int ->
  GetBucketCORSResponse
getBucketCORSResponse pResponseStatus_ =
  GetBucketCORSResponse'
    { _gbcrsCORSRules = Nothing,
      _gbcrsResponseStatus = pResponseStatus_
    }

-- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
gbcrsCORSRules :: Lens' GetBucketCORSResponse [CORSRule]
gbcrsCORSRules = lens _gbcrsCORSRules (\s a -> s {_gbcrsCORSRules = a}) . _Default . _Coerce

-- | -- | The response status code.
gbcrsResponseStatus :: Lens' GetBucketCORSResponse Int
gbcrsResponseStatus = lens _gbcrsResponseStatus (\s a -> s {_gbcrsResponseStatus = a})

instance NFData GetBucketCORSResponse
