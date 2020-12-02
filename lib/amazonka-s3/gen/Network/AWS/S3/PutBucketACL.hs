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
-- Module      : Network.AWS.S3.PutBucketACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permissions on an existing bucket using access control lists (ACL). For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs> . To set the ACL of a bucket, you must have @WRITE_ACP@ permission.
--
--
-- You can use one of the following two ways to set a bucket's permissions:
--
--     * Specify the ACL in the request body
--
--     * Specify permissions using request headers
--
--
--
-- Depending on your application needs, you may choose to set the ACL on a bucket using either the request body or the headers. For example, if you have an existing application that updates a bucket ACL using the request body, then you can continue to use that approach.
--
-- __Access Permissions__
--
-- You can set access permissions using one of the following methods:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. Specify the canned ACL name as the value of @x-amz-acl@ . If you use this header, you cannot use other access control-specific headers in your request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. When using these headers, you specify explicit access permissions and grantees (AWS accounts or Amazon S3 groups) who will receive the permission. If you use these ACL-specific headers, you cannot use the @x-amz-acl@ header to set a canned ACL. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
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
-- For example, the following @x-amz-grant-write@ header grants create, overwrite, and delete objects permission to LogDelivery group predefined by Amazon S3 and two AWS accounts identified by their email addresses.
--
-- @x-amz-grant-write: uri="http://acs.amazonaws.com/groups/s3/LogDelivery", id="111122223333", id="555566667777" @
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
-- DisplayName is optional and ignored in the request
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
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
module Network.AWS.S3.PutBucketACL
  ( -- * Creating a Request
    putBucketACL,
    PutBucketACL,

    -- * Request Lenses
    pbaGrantReadACP,
    pbaGrantWriteACP,
    pbaGrantRead,
    pbaGrantFullControl,
    pbaContentMD5,
    pbaAccessControlPolicy,
    pbaGrantWrite,
    pbaACL,
    pbaExpectedBucketOwner,
    pbaBucket,

    -- * Destructuring the Response
    putBucketACLResponse,
    PutBucketACLResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketACL' smart constructor.
data PutBucketACL = PutBucketACL'
  { _pbaGrantReadACP ::
      !(Maybe Text),
    _pbaGrantWriteACP :: !(Maybe Text),
    _pbaGrantRead :: !(Maybe Text),
    _pbaGrantFullControl :: !(Maybe Text),
    _pbaContentMD5 :: !(Maybe Text),
    _pbaAccessControlPolicy :: !(Maybe AccessControlPolicy),
    _pbaGrantWrite :: !(Maybe Text),
    _pbaACL :: !(Maybe BucketCannedACL),
    _pbaExpectedBucketOwner :: !(Maybe Text),
    _pbaBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbaGrantReadACP' - Allows grantee to read the bucket ACL.
--
-- * 'pbaGrantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- * 'pbaGrantRead' - Allows grantee to list the objects in the bucket.
--
-- * 'pbaGrantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- * 'pbaContentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbaAccessControlPolicy' - Contains the elements that set the ACL permissions for an object per grantee.
--
-- * 'pbaGrantWrite' - Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- * 'pbaACL' - The canned ACL to apply to the bucket.
--
-- * 'pbaExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbaBucket' - The bucket to which to apply the ACL.
putBucketACL ::
  -- | 'pbaBucket'
  BucketName ->
  PutBucketACL
putBucketACL pBucket_ =
  PutBucketACL'
    { _pbaGrantReadACP = Nothing,
      _pbaGrantWriteACP = Nothing,
      _pbaGrantRead = Nothing,
      _pbaGrantFullControl = Nothing,
      _pbaContentMD5 = Nothing,
      _pbaAccessControlPolicy = Nothing,
      _pbaGrantWrite = Nothing,
      _pbaACL = Nothing,
      _pbaExpectedBucketOwner = Nothing,
      _pbaBucket = pBucket_
    }

-- | Allows grantee to read the bucket ACL.
pbaGrantReadACP :: Lens' PutBucketACL (Maybe Text)
pbaGrantReadACP = lens _pbaGrantReadACP (\s a -> s {_pbaGrantReadACP = a})

-- | Allows grantee to write the ACL for the applicable bucket.
pbaGrantWriteACP :: Lens' PutBucketACL (Maybe Text)
pbaGrantWriteACP = lens _pbaGrantWriteACP (\s a -> s {_pbaGrantWriteACP = a})

-- | Allows grantee to list the objects in the bucket.
pbaGrantRead :: Lens' PutBucketACL (Maybe Text)
pbaGrantRead = lens _pbaGrantRead (\s a -> s {_pbaGrantRead = a})

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
pbaGrantFullControl :: Lens' PutBucketACL (Maybe Text)
pbaGrantFullControl = lens _pbaGrantFullControl (\s a -> s {_pbaGrantFullControl = a})

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>  For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbaContentMD5 :: Lens' PutBucketACL (Maybe Text)
pbaContentMD5 = lens _pbaContentMD5 (\s a -> s {_pbaContentMD5 = a})

-- | Contains the elements that set the ACL permissions for an object per grantee.
pbaAccessControlPolicy :: Lens' PutBucketACL (Maybe AccessControlPolicy)
pbaAccessControlPolicy = lens _pbaAccessControlPolicy (\s a -> s {_pbaAccessControlPolicy = a})

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
pbaGrantWrite :: Lens' PutBucketACL (Maybe Text)
pbaGrantWrite = lens _pbaGrantWrite (\s a -> s {_pbaGrantWrite = a})

-- | The canned ACL to apply to the bucket.
pbaACL :: Lens' PutBucketACL (Maybe BucketCannedACL)
pbaACL = lens _pbaACL (\s a -> s {_pbaACL = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbaExpectedBucketOwner :: Lens' PutBucketACL (Maybe Text)
pbaExpectedBucketOwner = lens _pbaExpectedBucketOwner (\s a -> s {_pbaExpectedBucketOwner = a})

-- | The bucket to which to apply the ACL.
pbaBucket :: Lens' PutBucketACL BucketName
pbaBucket = lens _pbaBucket (\s a -> s {_pbaBucket = a})

instance AWSRequest PutBucketACL where
  type Rs PutBucketACL = PutBucketACLResponse
  request = putXML s3
  response = receiveNull PutBucketACLResponse'

instance Hashable PutBucketACL

instance NFData PutBucketACL

instance ToElement PutBucketACL where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
      . _pbaAccessControlPolicy

instance ToHeaders PutBucketACL where
  toHeaders PutBucketACL' {..} =
    mconcat
      [ "x-amz-grant-read-acp" =# _pbaGrantReadACP,
        "x-amz-grant-write-acp" =# _pbaGrantWriteACP,
        "x-amz-grant-read" =# _pbaGrantRead,
        "x-amz-grant-full-control" =# _pbaGrantFullControl,
        "Content-MD5" =# _pbaContentMD5,
        "x-amz-grant-write" =# _pbaGrantWrite,
        "x-amz-acl" =# _pbaACL,
        "x-amz-expected-bucket-owner" =# _pbaExpectedBucketOwner
      ]

instance ToPath PutBucketACL where
  toPath PutBucketACL' {..} = mconcat ["/", toBS _pbaBucket]

instance ToQuery PutBucketACL where
  toQuery = const (mconcat ["acl"])

-- | /See:/ 'putBucketACLResponse' smart constructor.
data PutBucketACLResponse = PutBucketACLResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketACLResponse' with the minimum fields required to make a request.
putBucketACLResponse ::
  PutBucketACLResponse
putBucketACLResponse = PutBucketACLResponse'

instance NFData PutBucketACLResponse
