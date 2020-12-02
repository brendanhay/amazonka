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
-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the tags for a bucket.
--
--
-- Use tags to organize your AWS bill to reflect your own cost structure. To do this, sign up to get your AWS account bill with tag key values included. Then, to see the cost of combined resources, organize your billing information according to resources with the same tag key values. For example, you can tag several resources with a specific application name, and then organize your billing information to see the total cost of that application across several services. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Cost Allocation and Tagging> .
--
-- To use this operation, you must have permissions to perform the @s3:PutBucketTagging@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- @PutBucketTagging@ has the following special errors:
--
--     * Error code: @InvalidTagError@
--
--     * Description: The tag provided was not a valid tag. This error can occur if the tag did not pass input validation. For information about tag restrictions, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/aws-tag-restrictions.html AWS-Generated Cost Allocation Tag Restrictions> .
--
--
--
--     * Error code: @MalformedXMLError@
--
--     * Description: The XML provided does not match the schema.
--
--
--
--     * Error code: @OperationAbortedError @
--
--     * Description: A conflicting conditional operation is currently in progress against this resource. Please try again.
--
--
--
--     * Error code: @InternalError@
--
--     * Description: The service was unable to apply the provided tag to the bucket.
--
--
--
--
--
-- The following operations are related to @PutBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging>
module Network.AWS.S3.PutBucketTagging
  ( -- * Creating a Request
    putBucketTagging,
    PutBucketTagging,

    -- * Request Lenses
    pbtContentMD5,
    pbtExpectedBucketOwner,
    pbtBucket,
    pbtTagging,

    -- * Destructuring the Response
    putBucketTaggingResponse,
    PutBucketTaggingResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketTagging' smart constructor.
data PutBucketTagging = PutBucketTagging'
  { _pbtContentMD5 ::
      !(Maybe Text),
    _pbtExpectedBucketOwner :: !(Maybe Text),
    _pbtBucket :: !BucketName,
    _pbtTagging :: !Tagging
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbtContentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbtExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbtBucket' - The bucket name.
--
-- * 'pbtTagging' - Container for the @TagSet@ and @Tag@ elements.
putBucketTagging ::
  -- | 'pbtBucket'
  BucketName ->
  -- | 'pbtTagging'
  Tagging ->
  PutBucketTagging
putBucketTagging pBucket_ pTagging_ =
  PutBucketTagging'
    { _pbtContentMD5 = Nothing,
      _pbtExpectedBucketOwner = Nothing,
      _pbtBucket = pBucket_,
      _pbtTagging = pTagging_
    }

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbtContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtContentMD5 = lens _pbtContentMD5 (\s a -> s {_pbtContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbtExpectedBucketOwner :: Lens' PutBucketTagging (Maybe Text)
pbtExpectedBucketOwner = lens _pbtExpectedBucketOwner (\s a -> s {_pbtExpectedBucketOwner = a})

-- | The bucket name.
pbtBucket :: Lens' PutBucketTagging BucketName
pbtBucket = lens _pbtBucket (\s a -> s {_pbtBucket = a})

-- | Container for the @TagSet@ and @Tag@ elements.
pbtTagging :: Lens' PutBucketTagging Tagging
pbtTagging = lens _pbtTagging (\s a -> s {_pbtTagging = a})

instance AWSRequest PutBucketTagging where
  type Rs PutBucketTagging = PutBucketTaggingResponse
  request = contentMD5Header . putXML s3
  response = receiveNull PutBucketTaggingResponse'

instance Hashable PutBucketTagging

instance NFData PutBucketTagging

instance ToElement PutBucketTagging where
  toElement =
    mkElement "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      . _pbtTagging

instance ToHeaders PutBucketTagging where
  toHeaders PutBucketTagging' {..} =
    mconcat
      [ "Content-MD5" =# _pbtContentMD5,
        "x-amz-expected-bucket-owner" =# _pbtExpectedBucketOwner
      ]

instance ToPath PutBucketTagging where
  toPath PutBucketTagging' {..} = mconcat ["/", toBS _pbtBucket]

instance ToQuery PutBucketTagging where
  toQuery = const (mconcat ["tagging"])

-- | /See:/ 'putBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse = PutBucketTaggingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketTaggingResponse' with the minimum fields required to make a request.
putBucketTaggingResponse ::
  PutBucketTaggingResponse
putBucketTaggingResponse = PutBucketTaggingResponse'

instance NFData PutBucketTaggingResponse
