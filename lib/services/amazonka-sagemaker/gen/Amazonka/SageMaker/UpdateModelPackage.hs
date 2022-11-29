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
-- Module      : Amazonka.SageMaker.UpdateModelPackage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a versioned model.
module Amazonka.SageMaker.UpdateModelPackage
  ( -- * Creating a Request
    UpdateModelPackage (..),
    newUpdateModelPackage,

    -- * Request Lenses
    updateModelPackage_customerMetadataPropertiesToRemove,
    updateModelPackage_modelApprovalStatus,
    updateModelPackage_approvalDescription,
    updateModelPackage_customerMetadataProperties,
    updateModelPackage_additionalInferenceSpecificationsToAdd,
    updateModelPackage_modelPackageArn,

    -- * Destructuring the Response
    UpdateModelPackageResponse (..),
    newUpdateModelPackageResponse,

    -- * Response Lenses
    updateModelPackageResponse_httpStatus,
    updateModelPackageResponse_modelPackageArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateModelPackage' smart constructor.
data UpdateModelPackage = UpdateModelPackage'
  { -- | The metadata properties associated with the model package versions to
    -- remove.
    customerMetadataPropertiesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The approval status of the model.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | A description for the approval status of the model.
    approvalDescription :: Prelude.Maybe Prelude.Text,
    -- | The metadata properties associated with the model package versions.
    customerMetadataProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of additional Inference Specification objects to be added to
    -- the existing array additional Inference Specification. Total number of
    -- additional Inference Specifications can not exceed 15. Each additional
    -- Inference Specification specifies artifacts based on this model package
    -- that can be used on inference endpoints. Generally used with SageMaker
    -- Neo to store the compiled artifacts.
    additionalInferenceSpecificationsToAdd :: Prelude.Maybe (Prelude.NonEmpty AdditionalInferenceSpecificationDefinition),
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerMetadataPropertiesToRemove', 'updateModelPackage_customerMetadataPropertiesToRemove' - The metadata properties associated with the model package versions to
-- remove.
--
-- 'modelApprovalStatus', 'updateModelPackage_modelApprovalStatus' - The approval status of the model.
--
-- 'approvalDescription', 'updateModelPackage_approvalDescription' - A description for the approval status of the model.
--
-- 'customerMetadataProperties', 'updateModelPackage_customerMetadataProperties' - The metadata properties associated with the model package versions.
--
-- 'additionalInferenceSpecificationsToAdd', 'updateModelPackage_additionalInferenceSpecificationsToAdd' - An array of additional Inference Specification objects to be added to
-- the existing array additional Inference Specification. Total number of
-- additional Inference Specifications can not exceed 15. Each additional
-- Inference Specification specifies artifacts based on this model package
-- that can be used on inference endpoints. Generally used with SageMaker
-- Neo to store the compiled artifacts.
--
-- 'modelPackageArn', 'updateModelPackage_modelPackageArn' - The Amazon Resource Name (ARN) of the model package.
newUpdateModelPackage ::
  -- | 'modelPackageArn'
  Prelude.Text ->
  UpdateModelPackage
newUpdateModelPackage pModelPackageArn_ =
  UpdateModelPackage'
    { customerMetadataPropertiesToRemove =
        Prelude.Nothing,
      modelApprovalStatus = Prelude.Nothing,
      approvalDescription = Prelude.Nothing,
      customerMetadataProperties = Prelude.Nothing,
      additionalInferenceSpecificationsToAdd =
        Prelude.Nothing,
      modelPackageArn = pModelPackageArn_
    }

-- | The metadata properties associated with the model package versions to
-- remove.
updateModelPackage_customerMetadataPropertiesToRemove :: Lens.Lens' UpdateModelPackage (Prelude.Maybe [Prelude.Text])
updateModelPackage_customerMetadataPropertiesToRemove = Lens.lens (\UpdateModelPackage' {customerMetadataPropertiesToRemove} -> customerMetadataPropertiesToRemove) (\s@UpdateModelPackage' {} a -> s {customerMetadataPropertiesToRemove = a} :: UpdateModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | The approval status of the model.
updateModelPackage_modelApprovalStatus :: Lens.Lens' UpdateModelPackage (Prelude.Maybe ModelApprovalStatus)
updateModelPackage_modelApprovalStatus = Lens.lens (\UpdateModelPackage' {modelApprovalStatus} -> modelApprovalStatus) (\s@UpdateModelPackage' {} a -> s {modelApprovalStatus = a} :: UpdateModelPackage)

-- | A description for the approval status of the model.
updateModelPackage_approvalDescription :: Lens.Lens' UpdateModelPackage (Prelude.Maybe Prelude.Text)
updateModelPackage_approvalDescription = Lens.lens (\UpdateModelPackage' {approvalDescription} -> approvalDescription) (\s@UpdateModelPackage' {} a -> s {approvalDescription = a} :: UpdateModelPackage)

-- | The metadata properties associated with the model package versions.
updateModelPackage_customerMetadataProperties :: Lens.Lens' UpdateModelPackage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateModelPackage_customerMetadataProperties = Lens.lens (\UpdateModelPackage' {customerMetadataProperties} -> customerMetadataProperties) (\s@UpdateModelPackage' {} a -> s {customerMetadataProperties = a} :: UpdateModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | An array of additional Inference Specification objects to be added to
-- the existing array additional Inference Specification. Total number of
-- additional Inference Specifications can not exceed 15. Each additional
-- Inference Specification specifies artifacts based on this model package
-- that can be used on inference endpoints. Generally used with SageMaker
-- Neo to store the compiled artifacts.
updateModelPackage_additionalInferenceSpecificationsToAdd :: Lens.Lens' UpdateModelPackage (Prelude.Maybe (Prelude.NonEmpty AdditionalInferenceSpecificationDefinition))
updateModelPackage_additionalInferenceSpecificationsToAdd = Lens.lens (\UpdateModelPackage' {additionalInferenceSpecificationsToAdd} -> additionalInferenceSpecificationsToAdd) (\s@UpdateModelPackage' {} a -> s {additionalInferenceSpecificationsToAdd = a} :: UpdateModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the model package.
updateModelPackage_modelPackageArn :: Lens.Lens' UpdateModelPackage Prelude.Text
updateModelPackage_modelPackageArn = Lens.lens (\UpdateModelPackage' {modelPackageArn} -> modelPackageArn) (\s@UpdateModelPackage' {} a -> s {modelPackageArn = a} :: UpdateModelPackage)

instance Core.AWSRequest UpdateModelPackage where
  type
    AWSResponse UpdateModelPackage =
      UpdateModelPackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelPackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ModelPackageArn")
      )

instance Prelude.Hashable UpdateModelPackage where
  hashWithSalt _salt UpdateModelPackage' {..} =
    _salt
      `Prelude.hashWithSalt` customerMetadataPropertiesToRemove
      `Prelude.hashWithSalt` modelApprovalStatus
      `Prelude.hashWithSalt` approvalDescription
      `Prelude.hashWithSalt` customerMetadataProperties
      `Prelude.hashWithSalt` additionalInferenceSpecificationsToAdd
      `Prelude.hashWithSalt` modelPackageArn

instance Prelude.NFData UpdateModelPackage where
  rnf UpdateModelPackage' {..} =
    Prelude.rnf customerMetadataPropertiesToRemove
      `Prelude.seq` Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf approvalDescription
      `Prelude.seq` Prelude.rnf customerMetadataProperties
      `Prelude.seq` Prelude.rnf additionalInferenceSpecificationsToAdd
      `Prelude.seq` Prelude.rnf modelPackageArn

instance Core.ToHeaders UpdateModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateModelPackage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateModelPackage where
  toJSON UpdateModelPackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomerMetadataPropertiesToRemove" Core..=)
              Prelude.<$> customerMetadataPropertiesToRemove,
            ("ModelApprovalStatus" Core..=)
              Prelude.<$> modelApprovalStatus,
            ("ApprovalDescription" Core..=)
              Prelude.<$> approvalDescription,
            ("CustomerMetadataProperties" Core..=)
              Prelude.<$> customerMetadataProperties,
            ("AdditionalInferenceSpecificationsToAdd" Core..=)
              Prelude.<$> additionalInferenceSpecificationsToAdd,
            Prelude.Just
              ("ModelPackageArn" Core..= modelPackageArn)
          ]
      )

instance Core.ToPath UpdateModelPackage where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelPackageResponse' smart constructor.
data UpdateModelPackageResponse = UpdateModelPackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the model.
    modelPackageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateModelPackageResponse where
  rnf UpdateModelPackageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelPackageArn
