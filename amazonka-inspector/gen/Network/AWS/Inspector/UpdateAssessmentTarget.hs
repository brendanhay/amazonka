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
-- Module      : Network.AWS.Inspector.UpdateAssessmentTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the assessment target that is specified by the ARN of the
-- assessment target.
--
-- If resourceGroupArn is not specified, all EC2 instances in the current
-- AWS account and region are included in the assessment target.
module Network.AWS.Inspector.UpdateAssessmentTarget
  ( -- * Creating a Request
    UpdateAssessmentTarget (..),
    newUpdateAssessmentTarget,

    -- * Request Lenses
    updateAssessmentTarget_resourceGroupArn,
    updateAssessmentTarget_assessmentTargetArn,
    updateAssessmentTarget_assessmentTargetName,

    -- * Destructuring the Response
    UpdateAssessmentTargetResponse (..),
    newUpdateAssessmentTargetResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAssessmentTarget' smart constructor.
data UpdateAssessmentTarget = UpdateAssessmentTarget'
  { -- | The ARN of the resource group that is used to specify the new resource
    -- group to associate with the assessment target.
    resourceGroupArn :: Core.Maybe Core.Text,
    -- | The ARN of the assessment target that you want to update.
    assessmentTargetArn :: Core.Text,
    -- | The name of the assessment target that you want to update.
    assessmentTargetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAssessmentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupArn', 'updateAssessmentTarget_resourceGroupArn' - The ARN of the resource group that is used to specify the new resource
-- group to associate with the assessment target.
--
-- 'assessmentTargetArn', 'updateAssessmentTarget_assessmentTargetArn' - The ARN of the assessment target that you want to update.
--
-- 'assessmentTargetName', 'updateAssessmentTarget_assessmentTargetName' - The name of the assessment target that you want to update.
newUpdateAssessmentTarget ::
  -- | 'assessmentTargetArn'
  Core.Text ->
  -- | 'assessmentTargetName'
  Core.Text ->
  UpdateAssessmentTarget
newUpdateAssessmentTarget
  pAssessmentTargetArn_
  pAssessmentTargetName_ =
    UpdateAssessmentTarget'
      { resourceGroupArn =
          Core.Nothing,
        assessmentTargetArn = pAssessmentTargetArn_,
        assessmentTargetName = pAssessmentTargetName_
      }

-- | The ARN of the resource group that is used to specify the new resource
-- group to associate with the assessment target.
updateAssessmentTarget_resourceGroupArn :: Lens.Lens' UpdateAssessmentTarget (Core.Maybe Core.Text)
updateAssessmentTarget_resourceGroupArn = Lens.lens (\UpdateAssessmentTarget' {resourceGroupArn} -> resourceGroupArn) (\s@UpdateAssessmentTarget' {} a -> s {resourceGroupArn = a} :: UpdateAssessmentTarget)

-- | The ARN of the assessment target that you want to update.
updateAssessmentTarget_assessmentTargetArn :: Lens.Lens' UpdateAssessmentTarget Core.Text
updateAssessmentTarget_assessmentTargetArn = Lens.lens (\UpdateAssessmentTarget' {assessmentTargetArn} -> assessmentTargetArn) (\s@UpdateAssessmentTarget' {} a -> s {assessmentTargetArn = a} :: UpdateAssessmentTarget)

-- | The name of the assessment target that you want to update.
updateAssessmentTarget_assessmentTargetName :: Lens.Lens' UpdateAssessmentTarget Core.Text
updateAssessmentTarget_assessmentTargetName = Lens.lens (\UpdateAssessmentTarget' {assessmentTargetName} -> assessmentTargetName) (\s@UpdateAssessmentTarget' {} a -> s {assessmentTargetName = a} :: UpdateAssessmentTarget)

instance Core.AWSRequest UpdateAssessmentTarget where
  type
    AWSResponse UpdateAssessmentTarget =
      UpdateAssessmentTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateAssessmentTargetResponse'

instance Core.Hashable UpdateAssessmentTarget

instance Core.NFData UpdateAssessmentTarget

instance Core.ToHeaders UpdateAssessmentTarget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.UpdateAssessmentTarget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateAssessmentTarget where
  toJSON UpdateAssessmentTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("resourceGroupArn" Core..=)
              Core.<$> resourceGroupArn,
            Core.Just
              ("assessmentTargetArn" Core..= assessmentTargetArn),
            Core.Just
              ( "assessmentTargetName"
                  Core..= assessmentTargetName
              )
          ]
      )

instance Core.ToPath UpdateAssessmentTarget where
  toPath = Core.const "/"

instance Core.ToQuery UpdateAssessmentTarget where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAssessmentTargetResponse' smart constructor.
data UpdateAssessmentTargetResponse = UpdateAssessmentTargetResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAssessmentTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAssessmentTargetResponse ::
  UpdateAssessmentTargetResponse
newUpdateAssessmentTargetResponse =
  UpdateAssessmentTargetResponse'

instance Core.NFData UpdateAssessmentTargetResponse
