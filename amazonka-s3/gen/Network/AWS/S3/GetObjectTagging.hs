{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectTagging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag-set of an object. You send the GET request against the
-- tagging subresource associated with the object.
--
-- To use this operation, you must have permission to perform the
-- @s3:GetObjectTagging@ action. By default, the GET operation returns
-- information about current version of an object. For a versioned bucket,
-- you can have multiple versions of an object in your bucket. To retrieve
-- tags of any other version, use the versionId query parameter. You also
-- need permission for the @s3:GetObjectVersionTagging@ action.
--
-- By default, the bucket owner has this permission and can grant this
-- permission to others.
--
-- For information about the Amazon S3 object tagging feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging>.
--
-- The following operation is related to @GetObjectTagging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html PutObjectTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObjectTagging.html DeleteObjectTagging>
module Network.AWS.S3.GetObjectTagging
  ( -- * Creating a Request
    GetObjectTagging (..),
    newGetObjectTagging,

    -- * Request Lenses
    getObjectTagging_expectedBucketOwner,
    getObjectTagging_versionId,
    getObjectTagging_requestPayer,
    getObjectTagging_bucket,
    getObjectTagging_key,

    -- * Destructuring the Response
    GetObjectTaggingResponse (..),
    newGetObjectTaggingResponse,

    -- * Response Lenses
    getObjectTaggingResponse_versionId,
    getObjectTaggingResponse_httpStatus,
    getObjectTaggingResponse_tagSet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetObjectTagging' smart constructor.
data GetObjectTagging = GetObjectTagging'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The versionId of the object for which to get the tagging information.
    versionId :: Prelude.Maybe ObjectVersionId,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The bucket name containing the object for which to get the tagging
    -- information.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- When using this API with Amazon S3 on Outposts, you must direct requests
    -- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
    -- form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this operation using S3 on Outposts through the AWS SDKs, you
    -- provide the Outposts bucket ARN in place of the bucket name. For more
    -- information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | Object key for which to get the tagging information.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetObjectTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectTagging_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'versionId', 'getObjectTagging_versionId' - The versionId of the object for which to get the tagging information.
--
-- 'requestPayer', 'getObjectTagging_requestPayer' - Undocumented member.
--
-- 'bucket', 'getObjectTagging_bucket' - The bucket name containing the object for which to get the tagging
-- information.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'key', 'getObjectTagging_key' - Object key for which to get the tagging information.
newGetObjectTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectTagging
newGetObjectTagging pBucket_ pKey_ =
  GetObjectTagging'
    { expectedBucketOwner =
        Prelude.Nothing,
      versionId = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getObjectTagging_expectedBucketOwner :: Lens.Lens' GetObjectTagging (Prelude.Maybe Prelude.Text)
getObjectTagging_expectedBucketOwner = Lens.lens (\GetObjectTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectTagging' {} a -> s {expectedBucketOwner = a} :: GetObjectTagging)

-- | The versionId of the object for which to get the tagging information.
getObjectTagging_versionId :: Lens.Lens' GetObjectTagging (Prelude.Maybe ObjectVersionId)
getObjectTagging_versionId = Lens.lens (\GetObjectTagging' {versionId} -> versionId) (\s@GetObjectTagging' {} a -> s {versionId = a} :: GetObjectTagging)

-- | Undocumented member.
getObjectTagging_requestPayer :: Lens.Lens' GetObjectTagging (Prelude.Maybe RequestPayer)
getObjectTagging_requestPayer = Lens.lens (\GetObjectTagging' {requestPayer} -> requestPayer) (\s@GetObjectTagging' {} a -> s {requestPayer = a} :: GetObjectTagging)

-- | The bucket name containing the object for which to get the tagging
-- information.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
getObjectTagging_bucket :: Lens.Lens' GetObjectTagging BucketName
getObjectTagging_bucket = Lens.lens (\GetObjectTagging' {bucket} -> bucket) (\s@GetObjectTagging' {} a -> s {bucket = a} :: GetObjectTagging)

-- | Object key for which to get the tagging information.
getObjectTagging_key :: Lens.Lens' GetObjectTagging ObjectKey
getObjectTagging_key = Lens.lens (\GetObjectTagging' {key} -> key) (\s@GetObjectTagging' {} a -> s {key = a} :: GetObjectTagging)

instance Prelude.AWSRequest GetObjectTagging where
  type Rs GetObjectTagging = GetObjectTaggingResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectTaggingResponse'
            Prelude.<$> (h Prelude..#? "x-amz-version-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..@? "TagSet" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "Tag"
                        )
      )

instance Prelude.Hashable GetObjectTagging

instance Prelude.NFData GetObjectTagging

instance Prelude.ToHeaders GetObjectTagging where
  toHeaders GetObjectTagging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "x-amz-request-payer" Prelude.=# requestPayer
      ]

instance Prelude.ToPath GetObjectTagging where
  toPath GetObjectTagging' {..} =
    Prelude.mconcat
      ["/", Prelude.toBS bucket, "/", Prelude.toBS key]

instance Prelude.ToQuery GetObjectTagging where
  toQuery GetObjectTagging' {..} =
    Prelude.mconcat
      ["versionId" Prelude.=: versionId, "tagging"]

-- | /See:/ 'newGetObjectTaggingResponse' smart constructor.
data GetObjectTaggingResponse = GetObjectTaggingResponse'
  { -- | The versionId of the object for which you got the tagging information.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains the tag set.
    tagSet :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetObjectTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'getObjectTaggingResponse_versionId' - The versionId of the object for which you got the tagging information.
--
-- 'httpStatus', 'getObjectTaggingResponse_httpStatus' - The response's http status code.
--
-- 'tagSet', 'getObjectTaggingResponse_tagSet' - Contains the tag set.
newGetObjectTaggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetObjectTaggingResponse
newGetObjectTaggingResponse pHttpStatus_ =
  GetObjectTaggingResponse'
    { versionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tagSet = Prelude.mempty
    }

-- | The versionId of the object for which you got the tagging information.
getObjectTaggingResponse_versionId :: Lens.Lens' GetObjectTaggingResponse (Prelude.Maybe ObjectVersionId)
getObjectTaggingResponse_versionId = Lens.lens (\GetObjectTaggingResponse' {versionId} -> versionId) (\s@GetObjectTaggingResponse' {} a -> s {versionId = a} :: GetObjectTaggingResponse)

-- | The response's http status code.
getObjectTaggingResponse_httpStatus :: Lens.Lens' GetObjectTaggingResponse Prelude.Int
getObjectTaggingResponse_httpStatus = Lens.lens (\GetObjectTaggingResponse' {httpStatus} -> httpStatus) (\s@GetObjectTaggingResponse' {} a -> s {httpStatus = a} :: GetObjectTaggingResponse)

-- | Contains the tag set.
getObjectTaggingResponse_tagSet :: Lens.Lens' GetObjectTaggingResponse [Tag]
getObjectTaggingResponse_tagSet = Lens.lens (\GetObjectTaggingResponse' {tagSet} -> tagSet) (\s@GetObjectTaggingResponse' {} a -> s {tagSet = a} :: GetObjectTaggingResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData GetObjectTaggingResponse
