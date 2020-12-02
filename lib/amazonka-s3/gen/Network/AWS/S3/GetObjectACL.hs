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
-- Module      : Network.AWS.S3.GetObjectACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access control list (ACL) of an object. To use this operation, you must have @READ_ACP@ access to the object.
--
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- __Versioning__
--
-- By default, GET returns ACL information about the current version of an object. To return ACL information about a different version, use the versionId subresource.
--
-- The following operations are related to @GetObjectAcl@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
module Network.AWS.S3.GetObjectACL
  ( -- * Creating a Request
    getObjectACL,
    GetObjectACL,

    -- * Request Lenses
    goaVersionId,
    goaRequestPayer,
    goaExpectedBucketOwner,
    goaBucket,
    goaKey,

    -- * Destructuring the Response
    getObjectACLResponse,
    GetObjectACLResponse,

    -- * Response Lenses
    goarsRequestCharged,
    goarsGrants,
    goarsOwner,
    goarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getObjectACL' smart constructor.
data GetObjectACL = GetObjectACL'
  { _goaVersionId ::
      !(Maybe ObjectVersionId),
    _goaRequestPayer :: !(Maybe RequestPayer),
    _goaExpectedBucketOwner :: !(Maybe Text),
    _goaBucket :: !BucketName,
    _goaKey :: !ObjectKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goaVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'goaRequestPayer' - Undocumented member.
--
-- * 'goaExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'goaBucket' - The bucket name that contains the object for which to get the ACL information.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'goaKey' - The key of the object for which to get the ACL information.
getObjectACL ::
  -- | 'goaBucket'
  BucketName ->
  -- | 'goaKey'
  ObjectKey ->
  GetObjectACL
getObjectACL pBucket_ pKey_ =
  GetObjectACL'
    { _goaVersionId = Nothing,
      _goaRequestPayer = Nothing,
      _goaExpectedBucketOwner = Nothing,
      _goaBucket = pBucket_,
      _goaKey = pKey_
    }

-- | VersionId used to reference a specific version of the object.
goaVersionId :: Lens' GetObjectACL (Maybe ObjectVersionId)
goaVersionId = lens _goaVersionId (\s a -> s {_goaVersionId = a})

-- | Undocumented member.
goaRequestPayer :: Lens' GetObjectACL (Maybe RequestPayer)
goaRequestPayer = lens _goaRequestPayer (\s a -> s {_goaRequestPayer = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
goaExpectedBucketOwner :: Lens' GetObjectACL (Maybe Text)
goaExpectedBucketOwner = lens _goaExpectedBucketOwner (\s a -> s {_goaExpectedBucketOwner = a})

-- | The bucket name that contains the object for which to get the ACL information.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
goaBucket :: Lens' GetObjectACL BucketName
goaBucket = lens _goaBucket (\s a -> s {_goaBucket = a})

-- | The key of the object for which to get the ACL information.
goaKey :: Lens' GetObjectACL ObjectKey
goaKey = lens _goaKey (\s a -> s {_goaKey = a})

instance AWSRequest GetObjectACL where
  type Rs GetObjectACL = GetObjectACLResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetObjectACLResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> ( x .@? "AccessControlList" .!@ mempty
                    >>= may (parseXMLList "Grant")
                )
            <*> (x .@? "Owner")
            <*> (pure (fromEnum s))
      )

instance Hashable GetObjectACL

instance NFData GetObjectACL

instance ToHeaders GetObjectACL where
  toHeaders GetObjectACL' {..} =
    mconcat
      [ "x-amz-request-payer" =# _goaRequestPayer,
        "x-amz-expected-bucket-owner" =# _goaExpectedBucketOwner
      ]

instance ToPath GetObjectACL where
  toPath GetObjectACL' {..} =
    mconcat ["/", toBS _goaBucket, "/", toBS _goaKey]

instance ToQuery GetObjectACL where
  toQuery GetObjectACL' {..} =
    mconcat ["versionId" =: _goaVersionId, "acl"]

-- | /See:/ 'getObjectACLResponse' smart constructor.
data GetObjectACLResponse = GetObjectACLResponse'
  { _goarsRequestCharged ::
      !(Maybe RequestCharged),
    _goarsGrants :: !(Maybe [Grant]),
    _goarsOwner :: !(Maybe Owner),
    _goarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goarsRequestCharged' - Undocumented member.
--
-- * 'goarsGrants' - A list of grants.
--
-- * 'goarsOwner' - Container for the bucket owner's display name and ID.
--
-- * 'goarsResponseStatus' - -- | The response status code.
getObjectACLResponse ::
  -- | 'goarsResponseStatus'
  Int ->
  GetObjectACLResponse
getObjectACLResponse pResponseStatus_ =
  GetObjectACLResponse'
    { _goarsRequestCharged = Nothing,
      _goarsGrants = Nothing,
      _goarsOwner = Nothing,
      _goarsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
goarsRequestCharged :: Lens' GetObjectACLResponse (Maybe RequestCharged)
goarsRequestCharged = lens _goarsRequestCharged (\s a -> s {_goarsRequestCharged = a})

-- | A list of grants.
goarsGrants :: Lens' GetObjectACLResponse [Grant]
goarsGrants = lens _goarsGrants (\s a -> s {_goarsGrants = a}) . _Default . _Coerce

-- | Container for the bucket owner's display name and ID.
goarsOwner :: Lens' GetObjectACLResponse (Maybe Owner)
goarsOwner = lens _goarsOwner (\s a -> s {_goarsOwner = a})

-- | -- | The response status code.
goarsResponseStatus :: Lens' GetObjectACLResponse Int
goarsResponseStatus = lens _goarsResponseStatus (\s a -> s {_goarsResponseStatus = a})

instance NFData GetObjectACLResponse
