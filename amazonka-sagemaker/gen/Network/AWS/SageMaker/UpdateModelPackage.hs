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
-- Module      : Network.AWS.SageMaker.UpdateModelPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a versioned model.
module Network.AWS.SageMaker.UpdateModelPackage
  ( -- * Creating a Request
    UpdateModelPackage (..),
    newUpdateModelPackage,

    -- * Request Lenses
    updateModelPackage_approvalDescription,
    updateModelPackage_modelPackageArn,
    updateModelPackage_modelApprovalStatus,

    -- * Destructuring the Response
    UpdateModelPackageResponse (..),
    newUpdateModelPackageResponse,

    -- * Response Lenses
    updateModelPackageResponse_httpStatus,
    updateModelPackageResponse_modelPackageArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateModelPackage' smart constructor.
data UpdateModelPackage = UpdateModelPackage'
  { -- | A description for the approval status of the model.
    approvalDescription :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the model.
    modelPackageArn :: Core.Text,
    -- | The approval status of the model.
    modelApprovalStatus :: ModelApprovalStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalDescription', 'updateModelPackage_approvalDescription' - A description for the approval status of the model.
--
-- 'modelPackageArn', 'updateModelPackage_modelPackageArn' - The Amazon Resource Name (ARN) of the model.
--
-- 'modelApprovalStatus', 'updateModelPackage_modelApprovalStatus' - The approval status of the model.
newUpdateModelPackage ::
  -- | 'modelPackageArn'
  Core.Text ->
  -- | 'modelApprovalStatus'
  ModelApprovalStatus ->
  UpdateModelPackage
newUpdateModelPackage
  pModelPackageArn_
  pModelApprovalStatus_ =
    UpdateModelPackage'
      { approvalDescription =
          Core.Nothing,
        modelPackageArn = pModelPackageArn_,
        modelApprovalStatus = pModelApprovalStatus_
      }

-- | A description for the approval status of the model.
updateModelPackage_approvalDescription :: Lens.Lens' UpdateModelPackage (Core.Maybe Core.Text)
updateModelPackage_approvalDescription = Lens.lens (\UpdateModelPackage' {approvalDescription} -> approvalDescription) (\s@UpdateModelPackage' {} a -> s {approvalDescription = a} :: UpdateModelPackage)

-- | The Amazon Resource Name (ARN) of the model.
updateModelPackage_modelPackageArn :: Lens.Lens' UpdateModelPackage Core.Text
updateModelPackage_modelPackageArn = Lens.lens (\UpdateModelPackage' {modelPackageArn} -> modelPackageArn) (\s@UpdateModelPackage' {} a -> s {modelPackageArn = a} :: UpdateModelPackage)

-- | The approval status of the model.
updateModelPackage_modelApprovalStatus :: Lens.Lens' UpdateModelPackage ModelApprovalStatus
updateModelPackage_modelApprovalStatus = Lens.lens (\UpdateModelPackage' {modelApprovalStatus} -> modelApprovalStatus) (\s@UpdateModelPackage' {} a -> s {modelApprovalStatus = a} :: UpdateModelPackage)

instance Core.AWSRequest UpdateModelPackage where
  type
    AWSResponse UpdateModelPackage =
      UpdateModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelPackageResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ModelPackageArn")
      )

instance Core.Hashable UpdateModelPackage

instance Core.NFData UpdateModelPackage

instance Core.ToHeaders UpdateModelPackage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateModelPackage" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateModelPackage where
  toJSON UpdateModelPackage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ApprovalDescription" Core..=)
              Core.<$> approvalDescription,
            Core.Just
              ("ModelPackageArn" Core..= modelPackageArn),
            Core.Just
              ("ModelApprovalStatus" Core..= modelApprovalStatus)
          ]
      )

instance Core.ToPath UpdateModelPackage where
  toPath = Core.const "/"

instance Core.ToQuery UpdateModelPackage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateModelPackageResponse' smart constructor.
data UpdateModelPackageResponse = UpdateModelPackageResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model.
    modelPackageArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateModelPackageResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageArn', 'updateModelPackageResponse_modelPackageArn' - The Amazon Resource Name (ARN) of the model.
newUpdateModelPackageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'modelPackageArn'
  Core.Text ->
  UpdateModelPackageResponse
newUpdateModelPackageResponse
  pHttpStatus_
  pModelPackageArn_ =
    UpdateModelPackageResponse'
      { httpStatus =
          pHttpStatus_,
        modelPackageArn = pModelPackageArn_
      }

-- | The response's http status code.
updateModelPackageResponse_httpStatus :: Lens.Lens' UpdateModelPackageResponse Core.Int
updateModelPackageResponse_httpStatus = Lens.lens (\UpdateModelPackageResponse' {httpStatus} -> httpStatus) (\s@UpdateModelPackageResponse' {} a -> s {httpStatus = a} :: UpdateModelPackageResponse)

-- | The Amazon Resource Name (ARN) of the model.
updateModelPackageResponse_modelPackageArn :: Lens.Lens' UpdateModelPackageResponse Core.Text
updateModelPackageResponse_modelPackageArn = Lens.lens (\UpdateModelPackageResponse' {modelPackageArn} -> modelPackageArn) (\s@UpdateModelPackageResponse' {} a -> s {modelPackageArn = a} :: UpdateModelPackageResponse)

instance Core.NFData UpdateModelPackageResponse
