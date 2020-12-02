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
-- Module      : Network.AWS.S3.GetBucketACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @GET@ operation uses the @acl@ subresource to return the access control list (ACL) of a bucket. To use @GET@ to return the ACL of the bucket, you must have @READ_ACP@ access to the bucket. If @READ_ACP@ permission is granted to the anonymous user, you can return the ACL of the bucket without using an authorization header.
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>
module Network.AWS.S3.GetBucketACL
  ( -- * Creating a Request
    getBucketACL,
    GetBucketACL,

    -- * Request Lenses
    gbaExpectedBucketOwner,
    gbaBucket,

    -- * Destructuring the Response
    getBucketACLResponse,
    GetBucketACLResponse,

    -- * Response Lenses
    gbarsGrants,
    gbarsOwner,
    gbarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketACL' smart constructor.
data GetBucketACL = GetBucketACL'
  { _gbaExpectedBucketOwner ::
      !(Maybe Text),
    _gbaBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbaExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbaBucket' - Specifies the S3 bucket whose ACL is being requested.
getBucketACL ::
  -- | 'gbaBucket'
  BucketName ->
  GetBucketACL
getBucketACL pBucket_ =
  GetBucketACL'
    { _gbaExpectedBucketOwner = Nothing,
      _gbaBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbaExpectedBucketOwner :: Lens' GetBucketACL (Maybe Text)
gbaExpectedBucketOwner = lens _gbaExpectedBucketOwner (\s a -> s {_gbaExpectedBucketOwner = a})

-- | Specifies the S3 bucket whose ACL is being requested.
gbaBucket :: Lens' GetBucketACL BucketName
gbaBucket = lens _gbaBucket (\s a -> s {_gbaBucket = a})

instance AWSRequest GetBucketACL where
  type Rs GetBucketACL = GetBucketACLResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketACLResponse'
            <$> ( x .@? "AccessControlList" .!@ mempty
                    >>= may (parseXMLList "Grant")
                )
            <*> (x .@? "Owner")
            <*> (pure (fromEnum s))
      )

instance Hashable GetBucketACL

instance NFData GetBucketACL

instance ToHeaders GetBucketACL where
  toHeaders GetBucketACL' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbaExpectedBucketOwner]

instance ToPath GetBucketACL where
  toPath GetBucketACL' {..} = mconcat ["/", toBS _gbaBucket]

instance ToQuery GetBucketACL where
  toQuery = const (mconcat ["acl"])

-- | /See:/ 'getBucketACLResponse' smart constructor.
data GetBucketACLResponse = GetBucketACLResponse'
  { _gbarsGrants ::
      !(Maybe [Grant]),
    _gbarsOwner :: !(Maybe Owner),
    _gbarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbarsGrants' - A list of grants.
--
-- * 'gbarsOwner' - Container for the bucket owner's display name and ID.
--
-- * 'gbarsResponseStatus' - -- | The response status code.
getBucketACLResponse ::
  -- | 'gbarsResponseStatus'
  Int ->
  GetBucketACLResponse
getBucketACLResponse pResponseStatus_ =
  GetBucketACLResponse'
    { _gbarsGrants = Nothing,
      _gbarsOwner = Nothing,
      _gbarsResponseStatus = pResponseStatus_
    }

-- | A list of grants.
gbarsGrants :: Lens' GetBucketACLResponse [Grant]
gbarsGrants = lens _gbarsGrants (\s a -> s {_gbarsGrants = a}) . _Default . _Coerce

-- | Container for the bucket owner's display name and ID.
gbarsOwner :: Lens' GetBucketACLResponse (Maybe Owner)
gbarsOwner = lens _gbarsOwner (\s a -> s {_gbarsOwner = a})

-- | -- | The response status code.
gbarsResponseStatus :: Lens' GetBucketACLResponse Int
gbarsResponseStatus = lens _gbarsResponseStatus (\s a -> s {_gbarsResponseStatus = a})

instance NFData GetBucketACLResponse
