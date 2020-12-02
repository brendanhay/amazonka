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
-- Module      : Network.AWS.S3.PutObjectACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses the @acl@ subresource to set the access control list (ACL) permissions for a new or existing object in an S3 bucket. You must have @WRITE_ACP@ permission to set the ACL of an object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#permissions What permissions can I grant?> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- Depending on your application needs, you can choose to set the ACL on an object using either the request body or the headers. For example, if you have an existing application that updates a bucket ACL using the request body, you can continue to use that approach. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> in the /Amazon S3 Developer Guide/ .
--
-- __Access Permissions__
--
-- You can set access permissions using one of the following methods:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as canned ACLs. Each canned ACL has a predefined set of grantees and permissions. Specify the canned ACL name as the value of @x-amz-ac@ l. If you use this header, you cannot use other access control-specific headers in your request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. When using these headers, you specify explicit access permissions and grantees (AWS accounts or Amazon S3 groups) who will receive the permission. If you use these ACL-specific headers, you cannot use @x-amz-acl@ header to set a canned ACL. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
--
-- You specify each grantee as a type=value pair, where the type is one of the following:
--
--     * @id@ – if the value specified is the canonical user ID of an AWS account
--
--     * @uri@ – if you are granting permissions to a predefined group
--
--     * @emailAddress@ – if the value specified is the email address of an AWS account
--
--
--
-- For example, the following @x-amz-grant-read@ header grants list objects permission to the two AWS accounts identified by their email addresses.
--
-- @x-amz-grant-read: emailAddress="xyz@amazon.com", emailAddress="abc@amazon.com" @
--
--
--
-- You can use either a canned ACL or specify access permissions explicitly. You cannot do both.
--
-- __Grantee Values__
--
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:
--
--     * By the person's ID:
--
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID><>ID<></ID><DisplayName><>GranteesEmail<></DisplayName> </Grantee>@
--
-- DisplayName is optional and ignored in the request.
--
--     * By URI:
--
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group"><URI><>http://acs.amazonaws.com/groups/global/AuthenticatedUsers<></URI></Grantee>@
--
--     * By Email address:
--
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail"><EmailAddress><>Grantees@email.com<></EmailAddress>lt;/Grantee>@
--
-- The grantee is resolved to the CanonicalUser and, in a response to a GET Object acl request, appears as the CanonicalUser.
--
--
--
-- __Versioning__
--
-- The ACL of an object is set at the object version level. By default, PUT sets the ACL of the current version of an object. To set the ACL of a different version, use the @versionId@ subresource.
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.PutObjectACL
  ( -- * Creating a Request
    putObjectACL,
    PutObjectACL,

    -- * Request Lenses
    poaVersionId,
    poaGrantReadACP,
    poaRequestPayer,
    poaGrantWriteACP,
    poaGrantRead,
    poaGrantFullControl,
    poaContentMD5,
    poaAccessControlPolicy,
    poaGrantWrite,
    poaACL,
    poaExpectedBucketOwner,
    poaBucket,
    poaKey,

    -- * Destructuring the Response
    putObjectACLResponse,
    PutObjectACLResponse,

    -- * Response Lenses
    poarsRequestCharged,
    poarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putObjectACL' smart constructor.
data PutObjectACL = PutObjectACL'
  { _poaVersionId ::
      !(Maybe ObjectVersionId),
    _poaGrantReadACP :: !(Maybe Text),
    _poaRequestPayer :: !(Maybe RequestPayer),
    _poaGrantWriteACP :: !(Maybe Text),
    _poaGrantRead :: !(Maybe Text),
    _poaGrantFullControl :: !(Maybe Text),
    _poaContentMD5 :: !(Maybe Text),
    _poaAccessControlPolicy :: !(Maybe AccessControlPolicy),
    _poaGrantWrite :: !(Maybe Text),
    _poaACL :: !(Maybe ObjectCannedACL),
    _poaExpectedBucketOwner :: !(Maybe Text),
    _poaBucket :: !BucketName,
    _poaKey :: !ObjectKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutObjectACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poaVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'poaGrantReadACP' - Allows grantee to read the bucket ACL. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poaRequestPayer' - Undocumented member.
--
-- * 'poaGrantWriteACP' - Allows grantee to write the ACL for the applicable bucket. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poaGrantRead' - Allows grantee to list the objects in the bucket. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poaGrantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on the bucket. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poaContentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'poaAccessControlPolicy' - Contains the elements that set the ACL permissions for an object per grantee.
--
-- * 'poaGrantWrite' - Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- * 'poaACL' - The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
-- * 'poaExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'poaBucket' - The bucket name that contains the object to which you want to attach the ACL.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'poaKey' - Key for which the PUT operation was initiated. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
putObjectACL ::
  -- | 'poaBucket'
  BucketName ->
  -- | 'poaKey'
  ObjectKey ->
  PutObjectACL
putObjectACL pBucket_ pKey_ =
  PutObjectACL'
    { _poaVersionId = Nothing,
      _poaGrantReadACP = Nothing,
      _poaRequestPayer = Nothing,
      _poaGrantWriteACP = Nothing,
      _poaGrantRead = Nothing,
      _poaGrantFullControl = Nothing,
      _poaContentMD5 = Nothing,
      _poaAccessControlPolicy = Nothing,
      _poaGrantWrite = Nothing,
      _poaACL = Nothing,
      _poaExpectedBucketOwner = Nothing,
      _poaBucket = pBucket_,
      _poaKey = pKey_
    }

-- | VersionId used to reference a specific version of the object.
poaVersionId :: Lens' PutObjectACL (Maybe ObjectVersionId)
poaVersionId = lens _poaVersionId (\s a -> s {_poaVersionId = a})

-- | Allows grantee to read the bucket ACL. This action is not supported by Amazon S3 on Outposts.
poaGrantReadACP :: Lens' PutObjectACL (Maybe Text)
poaGrantReadACP = lens _poaGrantReadACP (\s a -> s {_poaGrantReadACP = a})

-- | Undocumented member.
poaRequestPayer :: Lens' PutObjectACL (Maybe RequestPayer)
poaRequestPayer = lens _poaRequestPayer (\s a -> s {_poaRequestPayer = a})

-- | Allows grantee to write the ACL for the applicable bucket. This action is not supported by Amazon S3 on Outposts.
poaGrantWriteACP :: Lens' PutObjectACL (Maybe Text)
poaGrantWriteACP = lens _poaGrantWriteACP (\s a -> s {_poaGrantWriteACP = a})

-- | Allows grantee to list the objects in the bucket. This action is not supported by Amazon S3 on Outposts.
poaGrantRead :: Lens' PutObjectACL (Maybe Text)
poaGrantRead = lens _poaGrantRead (\s a -> s {_poaGrantRead = a})

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket. This action is not supported by Amazon S3 on Outposts.
poaGrantFullControl :: Lens' PutObjectACL (Maybe Text)
poaGrantFullControl = lens _poaGrantFullControl (\s a -> s {_poaGrantFullControl = a})

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>>  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
poaContentMD5 :: Lens' PutObjectACL (Maybe Text)
poaContentMD5 = lens _poaContentMD5 (\s a -> s {_poaContentMD5 = a})

-- | Contains the elements that set the ACL permissions for an object per grantee.
poaAccessControlPolicy :: Lens' PutObjectACL (Maybe AccessControlPolicy)
poaAccessControlPolicy = lens _poaAccessControlPolicy (\s a -> s {_poaAccessControlPolicy = a})

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
poaGrantWrite :: Lens' PutObjectACL (Maybe Text)
poaGrantWrite = lens _poaGrantWrite (\s a -> s {_poaGrantWrite = a})

-- | The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
poaACL :: Lens' PutObjectACL (Maybe ObjectCannedACL)
poaACL = lens _poaACL (\s a -> s {_poaACL = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
poaExpectedBucketOwner :: Lens' PutObjectACL (Maybe Text)
poaExpectedBucketOwner = lens _poaExpectedBucketOwner (\s a -> s {_poaExpectedBucketOwner = a})

-- | The bucket name that contains the object to which you want to attach the ACL.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
poaBucket :: Lens' PutObjectACL BucketName
poaBucket = lens _poaBucket (\s a -> s {_poaBucket = a})

-- | Key for which the PUT operation was initiated. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
poaKey :: Lens' PutObjectACL ObjectKey
poaKey = lens _poaKey (\s a -> s {_poaKey = a})

instance AWSRequest PutObjectACL where
  type Rs PutObjectACL = PutObjectACLResponse
  request = putXML s3
  response =
    receiveEmpty
      ( \s h x ->
          PutObjectACLResponse'
            <$> (h .#? "x-amz-request-charged") <*> (pure (fromEnum s))
      )

instance Hashable PutObjectACL

instance NFData PutObjectACL

instance ToElement PutObjectACL where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
      . _poaAccessControlPolicy

instance ToHeaders PutObjectACL where
  toHeaders PutObjectACL' {..} =
    mconcat
      [ "x-amz-grant-read-acp" =# _poaGrantReadACP,
        "x-amz-request-payer" =# _poaRequestPayer,
        "x-amz-grant-write-acp" =# _poaGrantWriteACP,
        "x-amz-grant-read" =# _poaGrantRead,
        "x-amz-grant-full-control" =# _poaGrantFullControl,
        "Content-MD5" =# _poaContentMD5,
        "x-amz-grant-write" =# _poaGrantWrite,
        "x-amz-acl" =# _poaACL,
        "x-amz-expected-bucket-owner" =# _poaExpectedBucketOwner
      ]

instance ToPath PutObjectACL where
  toPath PutObjectACL' {..} =
    mconcat ["/", toBS _poaBucket, "/", toBS _poaKey]

instance ToQuery PutObjectACL where
  toQuery PutObjectACL' {..} =
    mconcat ["versionId" =: _poaVersionId, "acl"]

-- | /See:/ 'putObjectACLResponse' smart constructor.
data PutObjectACLResponse = PutObjectACLResponse'
  { _poarsRequestCharged ::
      !(Maybe RequestCharged),
    _poarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutObjectACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poarsRequestCharged' - Undocumented member.
--
-- * 'poarsResponseStatus' - -- | The response status code.
putObjectACLResponse ::
  -- | 'poarsResponseStatus'
  Int ->
  PutObjectACLResponse
putObjectACLResponse pResponseStatus_ =
  PutObjectACLResponse'
    { _poarsRequestCharged = Nothing,
      _poarsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
poarsRequestCharged :: Lens' PutObjectACLResponse (Maybe RequestCharged)
poarsRequestCharged = lens _poarsRequestCharged (\s a -> s {_poarsRequestCharged = a})

-- | -- | The response status code.
poarsResponseStatus :: Lens' PutObjectACLResponse Int
poarsResponseStatus = lens _poarsResponseStatus (\s a -> s {_poarsResponseStatus = a})

instance NFData PutObjectACLResponse
