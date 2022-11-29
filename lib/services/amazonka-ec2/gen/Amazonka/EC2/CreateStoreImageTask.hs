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
-- Module      : Amazonka.EC2.CreateStoreImageTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores an AMI as a single object in an Amazon S3 bucket.
--
-- To use this API, you must have the required permissions. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-store-restore.html#ami-s3-permissions Permissions for storing and restoring AMIs using Amazon S3>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-store-restore.html Store and restore an AMI using Amazon S3>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateStoreImageTask
  ( -- * Creating a Request
    CreateStoreImageTask (..),
    newCreateStoreImageTask,

    -- * Request Lenses
    createStoreImageTask_dryRun,
    createStoreImageTask_s3ObjectTags,
    createStoreImageTask_imageId,
    createStoreImageTask_bucket,

    -- * Destructuring the Response
    CreateStoreImageTaskResponse (..),
    newCreateStoreImageTaskResponse,

    -- * Response Lenses
    createStoreImageTaskResponse_objectKey,
    createStoreImageTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStoreImageTask' smart constructor.
data CreateStoreImageTask = CreateStoreImageTask'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the AMI object that will be stored in the Amazon S3
    -- bucket.
    s3ObjectTags :: Prelude.Maybe [S3ObjectTag],
    -- | The ID of the AMI.
    imageId :: Prelude.Text,
    -- | The name of the Amazon S3 bucket in which the AMI object will be stored.
    -- The bucket must be in the Region in which the request is being made. The
    -- AMI object appears in the bucket only after the upload task has
    -- completed.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStoreImageTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createStoreImageTask_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 's3ObjectTags', 'createStoreImageTask_s3ObjectTags' - The tags to apply to the AMI object that will be stored in the Amazon S3
-- bucket.
--
-- 'imageId', 'createStoreImageTask_imageId' - The ID of the AMI.
--
-- 'bucket', 'createStoreImageTask_bucket' - The name of the Amazon S3 bucket in which the AMI object will be stored.
-- The bucket must be in the Region in which the request is being made. The
-- AMI object appears in the bucket only after the upload task has
-- completed.
newCreateStoreImageTask ::
  -- | 'imageId'
  Prelude.Text ->
  -- | 'bucket'
  Prelude.Text ->
  CreateStoreImageTask
newCreateStoreImageTask pImageId_ pBucket_ =
  CreateStoreImageTask'
    { dryRun = Prelude.Nothing,
      s3ObjectTags = Prelude.Nothing,
      imageId = pImageId_,
      bucket = pBucket_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createStoreImageTask_dryRun :: Lens.Lens' CreateStoreImageTask (Prelude.Maybe Prelude.Bool)
createStoreImageTask_dryRun = Lens.lens (\CreateStoreImageTask' {dryRun} -> dryRun) (\s@CreateStoreImageTask' {} a -> s {dryRun = a} :: CreateStoreImageTask)

-- | The tags to apply to the AMI object that will be stored in the Amazon S3
-- bucket.
createStoreImageTask_s3ObjectTags :: Lens.Lens' CreateStoreImageTask (Prelude.Maybe [S3ObjectTag])
createStoreImageTask_s3ObjectTags = Lens.lens (\CreateStoreImageTask' {s3ObjectTags} -> s3ObjectTags) (\s@CreateStoreImageTask' {} a -> s {s3ObjectTags = a} :: CreateStoreImageTask) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the AMI.
createStoreImageTask_imageId :: Lens.Lens' CreateStoreImageTask Prelude.Text
createStoreImageTask_imageId = Lens.lens (\CreateStoreImageTask' {imageId} -> imageId) (\s@CreateStoreImageTask' {} a -> s {imageId = a} :: CreateStoreImageTask)

-- | The name of the Amazon S3 bucket in which the AMI object will be stored.
-- The bucket must be in the Region in which the request is being made. The
-- AMI object appears in the bucket only after the upload task has
-- completed.
createStoreImageTask_bucket :: Lens.Lens' CreateStoreImageTask Prelude.Text
createStoreImageTask_bucket = Lens.lens (\CreateStoreImageTask' {bucket} -> bucket) (\s@CreateStoreImageTask' {} a -> s {bucket = a} :: CreateStoreImageTask)

instance Core.AWSRequest CreateStoreImageTask where
  type
    AWSResponse CreateStoreImageTask =
      CreateStoreImageTaskResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateStoreImageTaskResponse'
            Prelude.<$> (x Core..@? "objectKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStoreImageTask where
  hashWithSalt _salt CreateStoreImageTask' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` s3ObjectTags
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData CreateStoreImageTask where
  rnf CreateStoreImageTask' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf s3ObjectTags
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf bucket

instance Core.ToHeaders CreateStoreImageTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateStoreImageTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateStoreImageTask where
  toQuery CreateStoreImageTask' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateStoreImageTask" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "S3ObjectTag"
              Prelude.<$> s3ObjectTags
          ),
        "ImageId" Core.=: imageId,
        "Bucket" Core.=: bucket
      ]

-- | /See:/ 'newCreateStoreImageTaskResponse' smart constructor.
data CreateStoreImageTaskResponse = CreateStoreImageTaskResponse'
  { -- | The name of the stored AMI object in the S3 bucket.
    objectKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStoreImageTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectKey', 'createStoreImageTaskResponse_objectKey' - The name of the stored AMI object in the S3 bucket.
--
-- 'httpStatus', 'createStoreImageTaskResponse_httpStatus' - The response's http status code.
newCreateStoreImageTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStoreImageTaskResponse
newCreateStoreImageTaskResponse pHttpStatus_ =
  CreateStoreImageTaskResponse'
    { objectKey =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the stored AMI object in the S3 bucket.
createStoreImageTaskResponse_objectKey :: Lens.Lens' CreateStoreImageTaskResponse (Prelude.Maybe Prelude.Text)
createStoreImageTaskResponse_objectKey = Lens.lens (\CreateStoreImageTaskResponse' {objectKey} -> objectKey) (\s@CreateStoreImageTaskResponse' {} a -> s {objectKey = a} :: CreateStoreImageTaskResponse)

-- | The response's http status code.
createStoreImageTaskResponse_httpStatus :: Lens.Lens' CreateStoreImageTaskResponse Prelude.Int
createStoreImageTaskResponse_httpStatus = Lens.lens (\CreateStoreImageTaskResponse' {httpStatus} -> httpStatus) (\s@CreateStoreImageTaskResponse' {} a -> s {httpStatus = a} :: CreateStoreImageTaskResponse)

instance Prelude.NFData CreateStoreImageTaskResponse where
  rnf CreateStoreImageTaskResponse' {..} =
    Prelude.rnf objectKey
      `Prelude.seq` Prelude.rnf httpStatus
