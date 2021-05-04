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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateModelPackage' smart constructor.
data UpdateModelPackage = UpdateModelPackage'
  { -- | A description for the approval status of the model.
    approvalDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model.
    modelPackageArn :: Prelude.Text,
    -- | The approval status of the model.
    modelApprovalStatus :: ModelApprovalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'modelApprovalStatus'
  ModelApprovalStatus ->
  UpdateModelPackage
newUpdateModelPackage
  pModelPackageArn_
  pModelApprovalStatus_ =
    UpdateModelPackage'
      { approvalDescription =
          Prelude.Nothing,
        modelPackageArn = pModelPackageArn_,
        modelApprovalStatus = pModelApprovalStatus_
      }

-- | A description for the approval status of the model.
updateModelPackage_approvalDescription :: Lens.Lens' UpdateModelPackage (Prelude.Maybe Prelude.Text)
updateModelPackage_approvalDescription = Lens.lens (\UpdateModelPackage' {approvalDescription} -> approvalDescription) (\s@UpdateModelPackage' {} a -> s {approvalDescription = a} :: UpdateModelPackage)

-- | The Amazon Resource Name (ARN) of the model.
updateModelPackage_modelPackageArn :: Lens.Lens' UpdateModelPackage Prelude.Text
updateModelPackage_modelPackageArn = Lens.lens (\UpdateModelPackage' {modelPackageArn} -> modelPackageArn) (\s@UpdateModelPackage' {} a -> s {modelPackageArn = a} :: UpdateModelPackage)

-- | The approval status of the model.
updateModelPackage_modelApprovalStatus :: Lens.Lens' UpdateModelPackage ModelApprovalStatus
updateModelPackage_modelApprovalStatus = Lens.lens (\UpdateModelPackage' {modelApprovalStatus} -> modelApprovalStatus) (\s@UpdateModelPackage' {} a -> s {modelApprovalStatus = a} :: UpdateModelPackage)

instance Prelude.AWSRequest UpdateModelPackage where
  type
    Rs UpdateModelPackage =
      UpdateModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelPackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ModelPackageArn")
      )

instance Prelude.Hashable UpdateModelPackage

instance Prelude.NFData UpdateModelPackage

instance Prelude.ToHeaders UpdateModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.UpdateModelPackage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateModelPackage where
  toJSON UpdateModelPackage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ApprovalDescription" Prelude..=)
              Prelude.<$> approvalDescription,
            Prelude.Just
              ("ModelPackageArn" Prelude..= modelPackageArn),
            Prelude.Just
              ( "ModelApprovalStatus"
                  Prelude..= modelApprovalStatus
              )
          ]
      )

instance Prelude.ToPath UpdateModelPackage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelPackageResponse' smart constructor.
data UpdateModelPackageResponse = UpdateModelPackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model.
    modelPackageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'modelPackageArn'
  Prelude.Text ->
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
updateModelPackageResponse_httpStatus :: Lens.Lens' UpdateModelPackageResponse Prelude.Int
updateModelPackageResponse_httpStatus = Lens.lens (\UpdateModelPackageResponse' {httpStatus} -> httpStatus) (\s@UpdateModelPackageResponse' {} a -> s {httpStatus = a} :: UpdateModelPackageResponse)

-- | The Amazon Resource Name (ARN) of the model.
updateModelPackageResponse_modelPackageArn :: Lens.Lens' UpdateModelPackageResponse Prelude.Text
updateModelPackageResponse_modelPackageArn = Lens.lens (\UpdateModelPackageResponse' {modelPackageArn} -> modelPackageArn) (\s@UpdateModelPackageResponse' {} a -> s {modelPackageArn = a} :: UpdateModelPackageResponse)

instance Prelude.NFData UpdateModelPackageResponse
