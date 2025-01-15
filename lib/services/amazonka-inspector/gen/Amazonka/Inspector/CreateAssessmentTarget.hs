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
-- Module      : Amazonka.Inspector.CreateAssessmentTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new assessment target using the ARN of the resource group that
-- is generated by CreateResourceGroup. If resourceGroupArn is not
-- specified, all EC2 instances in the current AWS account and region are
-- included in the assessment target. If the
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_slr.html service-linked role>
-- isn’t already registered, this action also creates and registers a
-- service-linked role to grant Amazon Inspector access to AWS Services
-- needed to perform security assessments. You can create up to 50
-- assessment targets per AWS account. You can run up to 500 concurrent
-- agents per AWS account. For more information, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_applications.html Amazon Inspector Assessment Targets>.
module Amazonka.Inspector.CreateAssessmentTarget
  ( -- * Creating a Request
    CreateAssessmentTarget (..),
    newCreateAssessmentTarget,

    -- * Request Lenses
    createAssessmentTarget_resourceGroupArn,
    createAssessmentTarget_assessmentTargetName,

    -- * Destructuring the Response
    CreateAssessmentTargetResponse (..),
    newCreateAssessmentTargetResponse,

    -- * Response Lenses
    createAssessmentTargetResponse_httpStatus,
    createAssessmentTargetResponse_assessmentTargetArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAssessmentTarget' smart constructor.
data CreateAssessmentTarget = CreateAssessmentTarget'
  { -- | The ARN that specifies the resource group that is used to create the
    -- assessment target. If resourceGroupArn is not specified, all EC2
    -- instances in the current AWS account and region are included in the
    -- assessment target.
    resourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The user-defined name that identifies the assessment target that you
    -- want to create. The name must be unique within the AWS account.
    assessmentTargetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupArn', 'createAssessmentTarget_resourceGroupArn' - The ARN that specifies the resource group that is used to create the
-- assessment target. If resourceGroupArn is not specified, all EC2
-- instances in the current AWS account and region are included in the
-- assessment target.
--
-- 'assessmentTargetName', 'createAssessmentTarget_assessmentTargetName' - The user-defined name that identifies the assessment target that you
-- want to create. The name must be unique within the AWS account.
newCreateAssessmentTarget ::
  -- | 'assessmentTargetName'
  Prelude.Text ->
  CreateAssessmentTarget
newCreateAssessmentTarget pAssessmentTargetName_ =
  CreateAssessmentTarget'
    { resourceGroupArn =
        Prelude.Nothing,
      assessmentTargetName = pAssessmentTargetName_
    }

-- | The ARN that specifies the resource group that is used to create the
-- assessment target. If resourceGroupArn is not specified, all EC2
-- instances in the current AWS account and region are included in the
-- assessment target.
createAssessmentTarget_resourceGroupArn :: Lens.Lens' CreateAssessmentTarget (Prelude.Maybe Prelude.Text)
createAssessmentTarget_resourceGroupArn = Lens.lens (\CreateAssessmentTarget' {resourceGroupArn} -> resourceGroupArn) (\s@CreateAssessmentTarget' {} a -> s {resourceGroupArn = a} :: CreateAssessmentTarget)

-- | The user-defined name that identifies the assessment target that you
-- want to create. The name must be unique within the AWS account.
createAssessmentTarget_assessmentTargetName :: Lens.Lens' CreateAssessmentTarget Prelude.Text
createAssessmentTarget_assessmentTargetName = Lens.lens (\CreateAssessmentTarget' {assessmentTargetName} -> assessmentTargetName) (\s@CreateAssessmentTarget' {} a -> s {assessmentTargetName = a} :: CreateAssessmentTarget)

instance Core.AWSRequest CreateAssessmentTarget where
  type
    AWSResponse CreateAssessmentTarget =
      CreateAssessmentTargetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssessmentTargetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assessmentTargetArn")
      )

instance Prelude.Hashable CreateAssessmentTarget where
  hashWithSalt _salt CreateAssessmentTarget' {..} =
    _salt
      `Prelude.hashWithSalt` resourceGroupArn
      `Prelude.hashWithSalt` assessmentTargetName

instance Prelude.NFData CreateAssessmentTarget where
  rnf CreateAssessmentTarget' {..} =
    Prelude.rnf resourceGroupArn `Prelude.seq`
      Prelude.rnf assessmentTargetName

instance Data.ToHeaders CreateAssessmentTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.CreateAssessmentTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssessmentTarget where
  toJSON CreateAssessmentTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceGroupArn" Data..=)
              Prelude.<$> resourceGroupArn,
            Prelude.Just
              ( "assessmentTargetName"
                  Data..= assessmentTargetName
              )
          ]
      )

instance Data.ToPath CreateAssessmentTarget where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAssessmentTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssessmentTargetResponse' smart constructor.
data CreateAssessmentTargetResponse = CreateAssessmentTargetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN that specifies the assessment target that is created.
    assessmentTargetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAssessmentTargetResponse_httpStatus' - The response's http status code.
--
-- 'assessmentTargetArn', 'createAssessmentTargetResponse_assessmentTargetArn' - The ARN that specifies the assessment target that is created.
newCreateAssessmentTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assessmentTargetArn'
  Prelude.Text ->
  CreateAssessmentTargetResponse
newCreateAssessmentTargetResponse
  pHttpStatus_
  pAssessmentTargetArn_ =
    CreateAssessmentTargetResponse'
      { httpStatus =
          pHttpStatus_,
        assessmentTargetArn = pAssessmentTargetArn_
      }

-- | The response's http status code.
createAssessmentTargetResponse_httpStatus :: Lens.Lens' CreateAssessmentTargetResponse Prelude.Int
createAssessmentTargetResponse_httpStatus = Lens.lens (\CreateAssessmentTargetResponse' {httpStatus} -> httpStatus) (\s@CreateAssessmentTargetResponse' {} a -> s {httpStatus = a} :: CreateAssessmentTargetResponse)

-- | The ARN that specifies the assessment target that is created.
createAssessmentTargetResponse_assessmentTargetArn :: Lens.Lens' CreateAssessmentTargetResponse Prelude.Text
createAssessmentTargetResponse_assessmentTargetArn = Lens.lens (\CreateAssessmentTargetResponse' {assessmentTargetArn} -> assessmentTargetArn) (\s@CreateAssessmentTargetResponse' {} a -> s {assessmentTargetArn = a} :: CreateAssessmentTargetResponse)

instance
  Prelude.NFData
    CreateAssessmentTargetResponse
  where
  rnf CreateAssessmentTargetResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf assessmentTargetArn
