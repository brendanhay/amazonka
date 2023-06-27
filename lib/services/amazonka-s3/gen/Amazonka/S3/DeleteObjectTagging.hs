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
-- Module      : Amazonka.S3.DeleteObjectTagging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the entire tag set from the specified object. For more
-- information about managing object tags, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging>.
--
-- To use this operation, you must have permission to perform the
-- @s3:DeleteObjectTagging@ action.
--
-- To delete tags of a specific object version, add the @versionId@ query
-- parameter in the request. You will need permission for the
-- @s3:DeleteObjectVersionTagging@ action.
--
-- The following operations are related to @DeleteObjectTagging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html PutObjectTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>
module Amazonka.S3.DeleteObjectTagging
  ( -- * Creating a Request
    DeleteObjectTagging (..),
    newDeleteObjectTagging,

    -- * Request Lenses
    deleteObjectTagging_expectedBucketOwner,
    deleteObjectTagging_versionId,
    deleteObjectTagging_bucket,
    deleteObjectTagging_key,

    -- * Destructuring the Response
    DeleteObjectTaggingResponse (..),
    newDeleteObjectTaggingResponse,

    -- * Response Lenses
    deleteObjectTaggingResponse_versionId,
    deleteObjectTaggingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteObjectTagging' smart constructor.
data DeleteObjectTagging = DeleteObjectTagging'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The versionId of the object that the tag-set will be removed from.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name containing the objects from which to remove the tags.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- When you use this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
    -- When you use this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts access point ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | The key that identifies the object in the bucket from which to remove
    -- all tags.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteObjectTagging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'versionId', 'deleteObjectTagging_versionId' - The versionId of the object that the tag-set will be removed from.
--
-- 'bucket', 'deleteObjectTagging_bucket' - The bucket name containing the objects from which to remove the tags.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When you use this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When you use this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts access point ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'key', 'deleteObjectTagging_key' - The key that identifies the object in the bucket from which to remove
-- all tags.
newDeleteObjectTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  DeleteObjectTagging
newDeleteObjectTagging pBucket_ pKey_ =
  DeleteObjectTagging'
    { expectedBucketOwner =
        Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
deleteObjectTagging_expectedBucketOwner :: Lens.Lens' DeleteObjectTagging (Prelude.Maybe Prelude.Text)
deleteObjectTagging_expectedBucketOwner = Lens.lens (\DeleteObjectTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteObjectTagging' {} a -> s {expectedBucketOwner = a} :: DeleteObjectTagging)

-- | The versionId of the object that the tag-set will be removed from.
deleteObjectTagging_versionId :: Lens.Lens' DeleteObjectTagging (Prelude.Maybe ObjectVersionId)
deleteObjectTagging_versionId = Lens.lens (\DeleteObjectTagging' {versionId} -> versionId) (\s@DeleteObjectTagging' {} a -> s {versionId = a} :: DeleteObjectTagging)

-- | The bucket name containing the objects from which to remove the tags.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When you use this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ @/@AccessPointName@/@-@/@AccountId@/@.@/@outpostID@/@.s3-outposts.@/@Region@/@.amazonaws.com@.
-- When you use this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts access point ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html What is S3 on Outposts>
-- in the /Amazon S3 User Guide/.
deleteObjectTagging_bucket :: Lens.Lens' DeleteObjectTagging BucketName
deleteObjectTagging_bucket = Lens.lens (\DeleteObjectTagging' {bucket} -> bucket) (\s@DeleteObjectTagging' {} a -> s {bucket = a} :: DeleteObjectTagging)

-- | The key that identifies the object in the bucket from which to remove
-- all tags.
deleteObjectTagging_key :: Lens.Lens' DeleteObjectTagging ObjectKey
deleteObjectTagging_key = Lens.lens (\DeleteObjectTagging' {key} -> key) (\s@DeleteObjectTagging' {} a -> s {key = a} :: DeleteObjectTagging)

instance Core.AWSRequest DeleteObjectTagging where
  type
    AWSResponse DeleteObjectTagging =
      DeleteObjectTaggingResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteObjectTaggingResponse'
            Prelude.<$> (h Data..#? "x-amz-version-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteObjectTagging where
  hashWithSalt _salt DeleteObjectTagging' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData DeleteObjectTagging where
  rnf DeleteObjectTagging' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders DeleteObjectTagging where
  toHeaders DeleteObjectTagging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath DeleteObjectTagging where
  toPath DeleteObjectTagging' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery DeleteObjectTagging where
  toQuery DeleteObjectTagging' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "tagging"]

-- | /See:/ 'newDeleteObjectTaggingResponse' smart constructor.
data DeleteObjectTaggingResponse = DeleteObjectTaggingResponse'
  { -- | The versionId of the object the tag-set was removed from.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'deleteObjectTaggingResponse_versionId' - The versionId of the object the tag-set was removed from.
--
-- 'httpStatus', 'deleteObjectTaggingResponse_httpStatus' - The response's http status code.
newDeleteObjectTaggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteObjectTaggingResponse
newDeleteObjectTaggingResponse pHttpStatus_ =
  DeleteObjectTaggingResponse'
    { versionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The versionId of the object the tag-set was removed from.
deleteObjectTaggingResponse_versionId :: Lens.Lens' DeleteObjectTaggingResponse (Prelude.Maybe ObjectVersionId)
deleteObjectTaggingResponse_versionId = Lens.lens (\DeleteObjectTaggingResponse' {versionId} -> versionId) (\s@DeleteObjectTaggingResponse' {} a -> s {versionId = a} :: DeleteObjectTaggingResponse)

-- | The response's http status code.
deleteObjectTaggingResponse_httpStatus :: Lens.Lens' DeleteObjectTaggingResponse Prelude.Int
deleteObjectTaggingResponse_httpStatus = Lens.lens (\DeleteObjectTaggingResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectTaggingResponse' {} a -> s {httpStatus = a} :: DeleteObjectTaggingResponse)

instance Prelude.NFData DeleteObjectTaggingResponse where
  rnf DeleteObjectTaggingResponse' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
