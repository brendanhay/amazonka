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
-- Module      : Network.AWS.EC2.CreateRestoreImageTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task that restores an AMI from an Amazon S3 object that was
-- previously created by using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateStoreImageTask.html CreateStoreImageTask>.
--
-- To use this API, you must have the required permissions. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-store-restore.html#ami-s3-permissions Permissions for storing and restoring AMIs using Amazon S3>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-store-restore.html Store and restore an AMI using Amazon S3>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateRestoreImageTask
  ( -- * Creating a Request
    CreateRestoreImageTask (..),
    newCreateRestoreImageTask,

    -- * Request Lenses
    createRestoreImageTask_tagSpecifications,
    createRestoreImageTask_dryRun,
    createRestoreImageTask_name,
    createRestoreImageTask_bucket,
    createRestoreImageTask_objectKey,

    -- * Destructuring the Response
    CreateRestoreImageTaskResponse (..),
    newCreateRestoreImageTaskResponse,

    -- * Response Lenses
    createRestoreImageTaskResponse_imageId,
    createRestoreImageTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRestoreImageTask' smart constructor.
data CreateRestoreImageTask = CreateRestoreImageTask'
  { -- | The tags to apply to the AMI and snapshots on restoration. You can tag
    -- the AMI, the snapshots, or both.
    --
    -- -   To tag the AMI, the value for @ResourceType@ must be @image@.
    --
    -- -   To tag the snapshots, the value for @ResourceType@ must be
    --     @snapshot@. The same tag is applied to all of the snapshots that are
    --     created.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name for the restored AMI. The name must be unique for AMIs in the
    -- Region for this account. If you do not provide a name, the new AMI gets
    -- the same name as the original AMI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket that contains the stored AMI object.
    bucket :: Prelude.Text,
    -- | The name of the stored AMI object in the bucket.
    objectKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRestoreImageTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createRestoreImageTask_tagSpecifications' - The tags to apply to the AMI and snapshots on restoration. You can tag
-- the AMI, the snapshots, or both.
--
-- -   To tag the AMI, the value for @ResourceType@ must be @image@.
--
-- -   To tag the snapshots, the value for @ResourceType@ must be
--     @snapshot@. The same tag is applied to all of the snapshots that are
--     created.
--
-- 'dryRun', 'createRestoreImageTask_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'name', 'createRestoreImageTask_name' - The name for the restored AMI. The name must be unique for AMIs in the
-- Region for this account. If you do not provide a name, the new AMI gets
-- the same name as the original AMI.
--
-- 'bucket', 'createRestoreImageTask_bucket' - The name of the Amazon S3 bucket that contains the stored AMI object.
--
-- 'objectKey', 'createRestoreImageTask_objectKey' - The name of the stored AMI object in the bucket.
newCreateRestoreImageTask ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'objectKey'
  Prelude.Text ->
  CreateRestoreImageTask
newCreateRestoreImageTask pBucket_ pObjectKey_ =
  CreateRestoreImageTask'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      name = Prelude.Nothing,
      bucket = pBucket_,
      objectKey = pObjectKey_
    }

-- | The tags to apply to the AMI and snapshots on restoration. You can tag
-- the AMI, the snapshots, or both.
--
-- -   To tag the AMI, the value for @ResourceType@ must be @image@.
--
-- -   To tag the snapshots, the value for @ResourceType@ must be
--     @snapshot@. The same tag is applied to all of the snapshots that are
--     created.
createRestoreImageTask_tagSpecifications :: Lens.Lens' CreateRestoreImageTask (Prelude.Maybe [TagSpecification])
createRestoreImageTask_tagSpecifications = Lens.lens (\CreateRestoreImageTask' {tagSpecifications} -> tagSpecifications) (\s@CreateRestoreImageTask' {} a -> s {tagSpecifications = a} :: CreateRestoreImageTask) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createRestoreImageTask_dryRun :: Lens.Lens' CreateRestoreImageTask (Prelude.Maybe Prelude.Bool)
createRestoreImageTask_dryRun = Lens.lens (\CreateRestoreImageTask' {dryRun} -> dryRun) (\s@CreateRestoreImageTask' {} a -> s {dryRun = a} :: CreateRestoreImageTask)

-- | The name for the restored AMI. The name must be unique for AMIs in the
-- Region for this account. If you do not provide a name, the new AMI gets
-- the same name as the original AMI.
createRestoreImageTask_name :: Lens.Lens' CreateRestoreImageTask (Prelude.Maybe Prelude.Text)
createRestoreImageTask_name = Lens.lens (\CreateRestoreImageTask' {name} -> name) (\s@CreateRestoreImageTask' {} a -> s {name = a} :: CreateRestoreImageTask)

-- | The name of the Amazon S3 bucket that contains the stored AMI object.
createRestoreImageTask_bucket :: Lens.Lens' CreateRestoreImageTask Prelude.Text
createRestoreImageTask_bucket = Lens.lens (\CreateRestoreImageTask' {bucket} -> bucket) (\s@CreateRestoreImageTask' {} a -> s {bucket = a} :: CreateRestoreImageTask)

-- | The name of the stored AMI object in the bucket.
createRestoreImageTask_objectKey :: Lens.Lens' CreateRestoreImageTask Prelude.Text
createRestoreImageTask_objectKey = Lens.lens (\CreateRestoreImageTask' {objectKey} -> objectKey) (\s@CreateRestoreImageTask' {} a -> s {objectKey = a} :: CreateRestoreImageTask)

instance Core.AWSRequest CreateRestoreImageTask where
  type
    AWSResponse CreateRestoreImageTask =
      CreateRestoreImageTaskResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRestoreImageTaskResponse'
            Prelude.<$> (x Core..@? "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRestoreImageTask

instance Prelude.NFData CreateRestoreImageTask

instance Core.ToHeaders CreateRestoreImageTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateRestoreImageTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateRestoreImageTask where
  toQuery CreateRestoreImageTask' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateRestoreImageTask" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Name" Core.=: name,
        "Bucket" Core.=: bucket,
        "ObjectKey" Core.=: objectKey
      ]

-- | /See:/ 'newCreateRestoreImageTaskResponse' smart constructor.
data CreateRestoreImageTaskResponse = CreateRestoreImageTaskResponse'
  { -- | The AMI ID.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRestoreImageTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'createRestoreImageTaskResponse_imageId' - The AMI ID.
--
-- 'httpStatus', 'createRestoreImageTaskResponse_httpStatus' - The response's http status code.
newCreateRestoreImageTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRestoreImageTaskResponse
newCreateRestoreImageTaskResponse pHttpStatus_ =
  CreateRestoreImageTaskResponse'
    { imageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The AMI ID.
createRestoreImageTaskResponse_imageId :: Lens.Lens' CreateRestoreImageTaskResponse (Prelude.Maybe Prelude.Text)
createRestoreImageTaskResponse_imageId = Lens.lens (\CreateRestoreImageTaskResponse' {imageId} -> imageId) (\s@CreateRestoreImageTaskResponse' {} a -> s {imageId = a} :: CreateRestoreImageTaskResponse)

-- | The response's http status code.
createRestoreImageTaskResponse_httpStatus :: Lens.Lens' CreateRestoreImageTaskResponse Prelude.Int
createRestoreImageTaskResponse_httpStatus = Lens.lens (\CreateRestoreImageTaskResponse' {httpStatus} -> httpStatus) (\s@CreateRestoreImageTaskResponse' {} a -> s {httpStatus = a} :: CreateRestoreImageTaskResponse)

instance
  Prelude.NFData
    CreateRestoreImageTaskResponse
