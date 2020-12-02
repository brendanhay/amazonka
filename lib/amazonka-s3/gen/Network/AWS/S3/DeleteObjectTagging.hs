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
-- Module      : Network.AWS.S3.DeleteObjectTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the entire tag set from the specified object. For more information about managing object tags, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
--
--
-- To use this operation, you must have permission to perform the @s3:DeleteObjectTagging@ action.
--
-- To delete tags of a specific object version, add the @versionId@ query parameter in the request. You will need permission for the @s3:DeleteObjectVersionTagging@ action.
--
-- The following operations are related to @DeleteBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html PutObjectTagging>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>
module Network.AWS.S3.DeleteObjectTagging
  ( -- * Creating a Request
    deleteObjectTagging,
    DeleteObjectTagging,

    -- * Request Lenses
    dotVersionId,
    dotExpectedBucketOwner,
    dotBucket,
    dotKey,

    -- * Destructuring the Response
    deleteObjectTaggingResponse,
    DeleteObjectTaggingResponse,

    -- * Response Lenses
    dotrsVersionId,
    dotrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteObjectTagging' smart constructor.
data DeleteObjectTagging = DeleteObjectTagging'
  { _dotVersionId ::
      !(Maybe ObjectVersionId),
    _dotExpectedBucketOwner :: !(Maybe Text),
    _dotBucket :: !BucketName,
    _dotKey :: !ObjectKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteObjectTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotVersionId' - The versionId of the object that the tag-set will be removed from.
--
-- * 'dotExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dotBucket' - The bucket name containing the objects from which to remove the tags.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'dotKey' - Name of the object key.
deleteObjectTagging ::
  -- | 'dotBucket'
  BucketName ->
  -- | 'dotKey'
  ObjectKey ->
  DeleteObjectTagging
deleteObjectTagging pBucket_ pKey_ =
  DeleteObjectTagging'
    { _dotVersionId = Nothing,
      _dotExpectedBucketOwner = Nothing,
      _dotBucket = pBucket_,
      _dotKey = pKey_
    }

-- | The versionId of the object that the tag-set will be removed from.
dotVersionId :: Lens' DeleteObjectTagging (Maybe ObjectVersionId)
dotVersionId = lens _dotVersionId (\s a -> s {_dotVersionId = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dotExpectedBucketOwner :: Lens' DeleteObjectTagging (Maybe Text)
dotExpectedBucketOwner = lens _dotExpectedBucketOwner (\s a -> s {_dotExpectedBucketOwner = a})

-- | The bucket name containing the objects from which to remove the tags.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
dotBucket :: Lens' DeleteObjectTagging BucketName
dotBucket = lens _dotBucket (\s a -> s {_dotBucket = a})

-- | Name of the object key.
dotKey :: Lens' DeleteObjectTagging ObjectKey
dotKey = lens _dotKey (\s a -> s {_dotKey = a})

instance AWSRequest DeleteObjectTagging where
  type Rs DeleteObjectTagging = DeleteObjectTaggingResponse
  request = delete s3
  response =
    receiveEmpty
      ( \s h x ->
          DeleteObjectTaggingResponse'
            <$> (h .#? "x-amz-version-id") <*> (pure (fromEnum s))
      )

instance Hashable DeleteObjectTagging

instance NFData DeleteObjectTagging

instance ToHeaders DeleteObjectTagging where
  toHeaders DeleteObjectTagging' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dotExpectedBucketOwner]

instance ToPath DeleteObjectTagging where
  toPath DeleteObjectTagging' {..} =
    mconcat ["/", toBS _dotBucket, "/", toBS _dotKey]

instance ToQuery DeleteObjectTagging where
  toQuery DeleteObjectTagging' {..} =
    mconcat ["versionId" =: _dotVersionId, "tagging"]

-- | /See:/ 'deleteObjectTaggingResponse' smart constructor.
data DeleteObjectTaggingResponse = DeleteObjectTaggingResponse'
  { _dotrsVersionId ::
      !(Maybe ObjectVersionId),
    _dotrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteObjectTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dotrsVersionId' - The versionId of the object the tag-set was removed from.
--
-- * 'dotrsResponseStatus' - -- | The response status code.
deleteObjectTaggingResponse ::
  -- | 'dotrsResponseStatus'
  Int ->
  DeleteObjectTaggingResponse
deleteObjectTaggingResponse pResponseStatus_ =
  DeleteObjectTaggingResponse'
    { _dotrsVersionId = Nothing,
      _dotrsResponseStatus = pResponseStatus_
    }

-- | The versionId of the object the tag-set was removed from.
dotrsVersionId :: Lens' DeleteObjectTaggingResponse (Maybe ObjectVersionId)
dotrsVersionId = lens _dotrsVersionId (\s a -> s {_dotrsVersionId = a})

-- | -- | The response status code.
dotrsResponseStatus :: Lens' DeleteObjectTaggingResponse Int
dotrsResponseStatus = lens _dotrsResponseStatus (\s a -> s {_dotrsResponseStatus = a})

instance NFData DeleteObjectTaggingResponse
