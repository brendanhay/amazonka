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
-- Module      : Network.AWS.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.
--
--
-- You can set the versioning state with one of the following values:
--
-- __Enabled__ —Enables versioning for the objects in the bucket. All objects added to the bucket receive a unique version ID.
--
-- __Suspended__ —Disables versioning for the objects in the bucket. All objects added to the bucket receive the version ID null.
--
-- If the versioning state has never been set on a bucket, it has no versioning state; a <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning> request does not return a versioning state value.
--
-- If the bucket owner enables MFA Delete in the bucket versioning configuration, the bucket owner must include the @x-amz-mfa request@ header and the @Status@ and the @MfaDelete@ request elements in a request to set the versioning state of the bucket.
--
-- /Important:/ If you have an object expiration lifecycle policy in your non-versioned bucket and you want to maintain the same permanent delete behavior when you enable versioning, you must add a noncurrent expiration policy. The noncurrent expiration lifecycle policy will manage the deletes of the noncurrent object versions in the version-enabled bucket. (A version-enabled bucket maintains one current and zero or more noncurrent object versions.) For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-and-other-bucket-config Lifecycle and Versioning> .
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning>
module Network.AWS.S3.PutBucketVersioning
  ( -- * Creating a Request
    putBucketVersioning,
    PutBucketVersioning,

    -- * Request Lenses
    pbvMFA,
    pbvContentMD5,
    pbvExpectedBucketOwner,
    pbvBucket,
    pbvVersioningConfiguration,

    -- * Destructuring the Response
    putBucketVersioningResponse,
    PutBucketVersioningResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketVersioning' smart constructor.
data PutBucketVersioning = PutBucketVersioning'
  { _pbvMFA ::
      !(Maybe Text),
    _pbvContentMD5 :: !(Maybe Text),
    _pbvExpectedBucketOwner :: !(Maybe Text),
    _pbvBucket :: !BucketName,
    _pbvVersioningConfiguration ::
      !VersioningConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketVersioning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbvMFA' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
--
-- * 'pbvContentMD5' - >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbvExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbvBucket' - The bucket name.
--
-- * 'pbvVersioningConfiguration' - Container for setting the versioning state.
putBucketVersioning ::
  -- | 'pbvBucket'
  BucketName ->
  -- | 'pbvVersioningConfiguration'
  VersioningConfiguration ->
  PutBucketVersioning
putBucketVersioning pBucket_ pVersioningConfiguration_ =
  PutBucketVersioning'
    { _pbvMFA = Nothing,
      _pbvContentMD5 = Nothing,
      _pbvExpectedBucketOwner = Nothing,
      _pbvBucket = pBucket_,
      _pbvVersioningConfiguration = pVersioningConfiguration_
    }

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
pbvMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvMFA = lens _pbvMFA (\s a -> s {_pbvMFA = a})

-- | >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbvContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvContentMD5 = lens _pbvContentMD5 (\s a -> s {_pbvContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbvExpectedBucketOwner :: Lens' PutBucketVersioning (Maybe Text)
pbvExpectedBucketOwner = lens _pbvExpectedBucketOwner (\s a -> s {_pbvExpectedBucketOwner = a})

-- | The bucket name.
pbvBucket :: Lens' PutBucketVersioning BucketName
pbvBucket = lens _pbvBucket (\s a -> s {_pbvBucket = a})

-- | Container for setting the versioning state.
pbvVersioningConfiguration :: Lens' PutBucketVersioning VersioningConfiguration
pbvVersioningConfiguration = lens _pbvVersioningConfiguration (\s a -> s {_pbvVersioningConfiguration = a})

instance AWSRequest PutBucketVersioning where
  type Rs PutBucketVersioning = PutBucketVersioningResponse
  request = putXML s3
  response = receiveNull PutBucketVersioningResponse'

instance Hashable PutBucketVersioning

instance NFData PutBucketVersioning

instance ToElement PutBucketVersioning where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}VersioningConfiguration"
      . _pbvVersioningConfiguration

instance ToHeaders PutBucketVersioning where
  toHeaders PutBucketVersioning' {..} =
    mconcat
      [ "x-amz-mfa" =# _pbvMFA,
        "Content-MD5" =# _pbvContentMD5,
        "x-amz-expected-bucket-owner" =# _pbvExpectedBucketOwner
      ]

instance ToPath PutBucketVersioning where
  toPath PutBucketVersioning' {..} = mconcat ["/", toBS _pbvBucket]

instance ToQuery PutBucketVersioning where
  toQuery = const (mconcat ["versioning"])

-- | /See:/ 'putBucketVersioningResponse' smart constructor.
data PutBucketVersioningResponse = PutBucketVersioningResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketVersioningResponse' with the minimum fields required to make a request.
putBucketVersioningResponse ::
  PutBucketVersioningResponse
putBucketVersioningResponse = PutBucketVersioningResponse'

instance NFData PutBucketVersioningResponse
