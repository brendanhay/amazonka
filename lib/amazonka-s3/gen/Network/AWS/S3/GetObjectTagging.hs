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
-- Module      : Network.AWS.S3.GetObjectTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag-set of an object. You send the GET request against the tagging subresource associated with the object.
--
--
-- To use this operation, you must have permission to perform the @s3:GetObjectTagging@ action. By default, the GET operation returns information about current version of an object. For a versioned bucket, you can have multiple versions of an object in your bucket. To retrieve tags of any other version, use the versionId query parameter. You also need permission for the @s3:GetObjectVersionTagging@ action.
--
-- By default, the bucket owner has this permission and can grant this permission to others.
--
-- For information about the Amazon S3 object tagging feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
--
-- The following operation is related to @GetObjectTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html PutObjectTagging>
module Network.AWS.S3.GetObjectTagging
  ( -- * Creating a Request
    getObjectTagging,
    GetObjectTagging,

    -- * Request Lenses
    gotoVersionId,
    gotoExpectedBucketOwner,
    gotoBucket,
    gotoKey,

    -- * Destructuring the Response
    getObjectTaggingResponse,
    GetObjectTaggingResponse,

    -- * Response Lenses
    gotrsVersionId,
    gotrsResponseStatus,
    gotrsTagSet,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getObjectTagging' smart constructor.
data GetObjectTagging = GetObjectTagging'
  { _gotoVersionId ::
      !(Maybe ObjectVersionId),
    _gotoExpectedBucketOwner :: !(Maybe Text),
    _gotoBucket :: !BucketName,
    _gotoKey :: !ObjectKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotoVersionId' - The versionId of the object for which to get the tagging information.
--
-- * 'gotoExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gotoBucket' - The bucket name containing the object for which to get the tagging information.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'gotoKey' - Object key for which to get the tagging information.
getObjectTagging ::
  -- | 'gotoBucket'
  BucketName ->
  -- | 'gotoKey'
  ObjectKey ->
  GetObjectTagging
getObjectTagging pBucket_ pKey_ =
  GetObjectTagging'
    { _gotoVersionId = Nothing,
      _gotoExpectedBucketOwner = Nothing,
      _gotoBucket = pBucket_,
      _gotoKey = pKey_
    }

-- | The versionId of the object for which to get the tagging information.
gotoVersionId :: Lens' GetObjectTagging (Maybe ObjectVersionId)
gotoVersionId = lens _gotoVersionId (\s a -> s {_gotoVersionId = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gotoExpectedBucketOwner :: Lens' GetObjectTagging (Maybe Text)
gotoExpectedBucketOwner = lens _gotoExpectedBucketOwner (\s a -> s {_gotoExpectedBucketOwner = a})

-- | The bucket name containing the object for which to get the tagging information.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
gotoBucket :: Lens' GetObjectTagging BucketName
gotoBucket = lens _gotoBucket (\s a -> s {_gotoBucket = a})

-- | Object key for which to get the tagging information.
gotoKey :: Lens' GetObjectTagging ObjectKey
gotoKey = lens _gotoKey (\s a -> s {_gotoKey = a})

instance AWSRequest GetObjectTagging where
  type Rs GetObjectTagging = GetObjectTaggingResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetObjectTaggingResponse'
            <$> (h .#? "x-amz-version-id")
            <*> (pure (fromEnum s))
            <*> (x .@? "TagSet" .!@ mempty >>= parseXMLList "Tag")
      )

instance Hashable GetObjectTagging

instance NFData GetObjectTagging

instance ToHeaders GetObjectTagging where
  toHeaders GetObjectTagging' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gotoExpectedBucketOwner]

instance ToPath GetObjectTagging where
  toPath GetObjectTagging' {..} =
    mconcat ["/", toBS _gotoBucket, "/", toBS _gotoKey]

instance ToQuery GetObjectTagging where
  toQuery GetObjectTagging' {..} =
    mconcat ["versionId" =: _gotoVersionId, "tagging"]

-- | /See:/ 'getObjectTaggingResponse' smart constructor.
data GetObjectTaggingResponse = GetObjectTaggingResponse'
  { _gotrsVersionId ::
      !(Maybe ObjectVersionId),
    _gotrsResponseStatus :: !Int,
    _gotrsTagSet :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotrsVersionId' - The versionId of the object for which you got the tagging information.
--
-- * 'gotrsResponseStatus' - -- | The response status code.
--
-- * 'gotrsTagSet' - Contains the tag set.
getObjectTaggingResponse ::
  -- | 'gotrsResponseStatus'
  Int ->
  GetObjectTaggingResponse
getObjectTaggingResponse pResponseStatus_ =
  GetObjectTaggingResponse'
    { _gotrsVersionId = Nothing,
      _gotrsResponseStatus = pResponseStatus_,
      _gotrsTagSet = mempty
    }

-- | The versionId of the object for which you got the tagging information.
gotrsVersionId :: Lens' GetObjectTaggingResponse (Maybe ObjectVersionId)
gotrsVersionId = lens _gotrsVersionId (\s a -> s {_gotrsVersionId = a})

-- | -- | The response status code.
gotrsResponseStatus :: Lens' GetObjectTaggingResponse Int
gotrsResponseStatus = lens _gotrsResponseStatus (\s a -> s {_gotrsResponseStatus = a})

-- | Contains the tag set.
gotrsTagSet :: Lens' GetObjectTaggingResponse [Tag]
gotrsTagSet = lens _gotrsTagSet (\s a -> s {_gotrsTagSet = a}) . _Coerce

instance NFData GetObjectTaggingResponse
