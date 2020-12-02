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
-- Module      : Network.AWS.S3.PutObjectTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the supplied tag-set to an object that already exists in a bucket.
--
--
-- A tag is a key-value pair. You can associate tags with an object by sending a PUT request against the tagging subresource that is associated with the object. You can retrieve tags by sending a GET request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging> .
--
-- For tagging-related restrictions related to characters and encodings, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html Tag Restrictions> . Note that Amazon S3 limits the maximum number of tags to 10 tags per object.
--
-- To use this operation, you must have permission to perform the @s3:PutObjectTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
--
-- To put tags of any other version, use the @versionId@ query parameter. You also need permission for the @s3:PutObjectVersionTagging@ action.
--
-- For information about the Amazon S3 object tagging feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
--
-- __Special Errors__
--
--     *     * /Code: InvalidTagError /
--
--     * /Cause: The tag provided was not a valid tag. This error can occur if the tag did not pass input validation. For more information, see <https:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/object-tagging.html Object Tagging> ./
--
--
--
--     *     * /Code: MalformedXMLError /
--
--     * /Cause: The XML provided does not match the schema./
--
--
--
--     *     * /Code: OperationAbortedError /
--
--     * /Cause: A conflicting conditional operation is currently in progress against this resource. Please try again./
--
--
--
--     *     * /Code: InternalError/
--
--     * /Cause: The service was unable to apply the provided tag to the object./
--
--
--
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>
module Network.AWS.S3.PutObjectTagging
  ( -- * Creating a Request
    putObjectTagging,
    PutObjectTagging,

    -- * Request Lenses
    potVersionId,
    potContentMD5,
    potExpectedBucketOwner,
    potBucket,
    potKey,
    potTagging,

    -- * Destructuring the Response
    putObjectTaggingResponse,
    PutObjectTaggingResponse,

    -- * Response Lenses
    potrsVersionId,
    potrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putObjectTagging' smart constructor.
data PutObjectTagging = PutObjectTagging'
  { _potVersionId ::
      !(Maybe ObjectVersionId),
    _potContentMD5 :: !(Maybe Text),
    _potExpectedBucketOwner :: !(Maybe Text),
    _potBucket :: !BucketName,
    _potKey :: !ObjectKey,
    _potTagging :: !Tagging
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutObjectTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'potVersionId' - The versionId of the object that the tag-set will be added to.
--
-- * 'potContentMD5' - The MD5 hash for the request body. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'potExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'potBucket' - The bucket name containing the object.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'potKey' - Name of the object key.
--
-- * 'potTagging' - Container for the @TagSet@ and @Tag@ elements
putObjectTagging ::
  -- | 'potBucket'
  BucketName ->
  -- | 'potKey'
  ObjectKey ->
  -- | 'potTagging'
  Tagging ->
  PutObjectTagging
putObjectTagging pBucket_ pKey_ pTagging_ =
  PutObjectTagging'
    { _potVersionId = Nothing,
      _potContentMD5 = Nothing,
      _potExpectedBucketOwner = Nothing,
      _potBucket = pBucket_,
      _potKey = pKey_,
      _potTagging = pTagging_
    }

-- | The versionId of the object that the tag-set will be added to.
potVersionId :: Lens' PutObjectTagging (Maybe ObjectVersionId)
potVersionId = lens _potVersionId (\s a -> s {_potVersionId = a})

-- | The MD5 hash for the request body. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
potContentMD5 :: Lens' PutObjectTagging (Maybe Text)
potContentMD5 = lens _potContentMD5 (\s a -> s {_potContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
potExpectedBucketOwner :: Lens' PutObjectTagging (Maybe Text)
potExpectedBucketOwner = lens _potExpectedBucketOwner (\s a -> s {_potExpectedBucketOwner = a})

-- | The bucket name containing the object.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
potBucket :: Lens' PutObjectTagging BucketName
potBucket = lens _potBucket (\s a -> s {_potBucket = a})

-- | Name of the object key.
potKey :: Lens' PutObjectTagging ObjectKey
potKey = lens _potKey (\s a -> s {_potKey = a})

-- | Container for the @TagSet@ and @Tag@ elements
potTagging :: Lens' PutObjectTagging Tagging
potTagging = lens _potTagging (\s a -> s {_potTagging = a})

instance AWSRequest PutObjectTagging where
  type Rs PutObjectTagging = PutObjectTaggingResponse
  request = putXML s3
  response =
    receiveEmpty
      ( \s h x ->
          PutObjectTaggingResponse'
            <$> (h .#? "x-amz-version-id") <*> (pure (fromEnum s))
      )

instance Hashable PutObjectTagging

instance NFData PutObjectTagging

instance ToElement PutObjectTagging where
  toElement =
    mkElement "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      . _potTagging

instance ToHeaders PutObjectTagging where
  toHeaders PutObjectTagging' {..} =
    mconcat
      [ "Content-MD5" =# _potContentMD5,
        "x-amz-expected-bucket-owner" =# _potExpectedBucketOwner
      ]

instance ToPath PutObjectTagging where
  toPath PutObjectTagging' {..} =
    mconcat ["/", toBS _potBucket, "/", toBS _potKey]

instance ToQuery PutObjectTagging where
  toQuery PutObjectTagging' {..} =
    mconcat ["versionId" =: _potVersionId, "tagging"]

-- | /See:/ 'putObjectTaggingResponse' smart constructor.
data PutObjectTaggingResponse = PutObjectTaggingResponse'
  { _potrsVersionId ::
      !(Maybe ObjectVersionId),
    _potrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutObjectTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'potrsVersionId' - The versionId of the object the tag-set was added to.
--
-- * 'potrsResponseStatus' - -- | The response status code.
putObjectTaggingResponse ::
  -- | 'potrsResponseStatus'
  Int ->
  PutObjectTaggingResponse
putObjectTaggingResponse pResponseStatus_ =
  PutObjectTaggingResponse'
    { _potrsVersionId = Nothing,
      _potrsResponseStatus = pResponseStatus_
    }

-- | The versionId of the object the tag-set was added to.
potrsVersionId :: Lens' PutObjectTaggingResponse (Maybe ObjectVersionId)
potrsVersionId = lens _potrsVersionId (\s a -> s {_potrsVersionId = a})

-- | -- | The response status code.
potrsResponseStatus :: Lens' PutObjectTaggingResponse Int
potrsResponseStatus = lens _potrsResponseStatus (\s a -> s {_potrsResponseStatus = a})

instance NFData PutObjectTaggingResponse
