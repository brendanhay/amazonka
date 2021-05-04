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

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAssessmentTarget' smart constructor.
data UpdateAssessmentTarget = UpdateAssessmentTarget'
  { -- | The ARN of the resource group that is used to specify the new resource
    -- group to associate with the assessment target.
    resourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the assessment target that you want to update.
    assessmentTargetArn :: Prelude.Text,
    -- | The name of the assessment target that you want to update.
    assessmentTargetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'assessmentTargetName'
  Prelude.Text ->
  UpdateAssessmentTarget
newUpdateAssessmentTarget
  pAssessmentTargetArn_
  pAssessmentTargetName_ =
    UpdateAssessmentTarget'
      { resourceGroupArn =
          Prelude.Nothing,
        assessmentTargetArn = pAssessmentTargetArn_,
        assessmentTargetName = pAssessmentTargetName_
      }

-- | The ARN of the resource group that is used to specify the new resource
-- group to associate with the assessment target.
updateAssessmentTarget_resourceGroupArn :: Lens.Lens' UpdateAssessmentTarget (Prelude.Maybe Prelude.Text)
updateAssessmentTarget_resourceGroupArn = Lens.lens (\UpdateAssessmentTarget' {resourceGroupArn} -> resourceGroupArn) (\s@UpdateAssessmentTarget' {} a -> s {resourceGroupArn = a} :: UpdateAssessmentTarget)

-- | The ARN of the assessment target that you want to update.
updateAssessmentTarget_assessmentTargetArn :: Lens.Lens' UpdateAssessmentTarget Prelude.Text
updateAssessmentTarget_assessmentTargetArn = Lens.lens (\UpdateAssessmentTarget' {assessmentTargetArn} -> assessmentTargetArn) (\s@UpdateAssessmentTarget' {} a -> s {assessmentTargetArn = a} :: UpdateAssessmentTarget)

-- | The name of the assessment target that you want to update.
updateAssessmentTarget_assessmentTargetName :: Lens.Lens' UpdateAssessmentTarget Prelude.Text
updateAssessmentTarget_assessmentTargetName = Lens.lens (\UpdateAssessmentTarget' {assessmentTargetName} -> assessmentTargetName) (\s@UpdateAssessmentTarget' {} a -> s {assessmentTargetName = a} :: UpdateAssessmentTarget)

instance Prelude.AWSRequest UpdateAssessmentTarget where
  type
    Rs UpdateAssessmentTarget =
      UpdateAssessmentTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateAssessmentTargetResponse'

instance Prelude.Hashable UpdateAssessmentTarget

instance Prelude.NFData UpdateAssessmentTarget

instance Prelude.ToHeaders UpdateAssessmentTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.UpdateAssessmentTarget" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateAssessmentTarget where
  toJSON UpdateAssessmentTarget' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("resourceGroupArn" Prelude..=)
              Prelude.<$> resourceGroupArn,
            Prelude.Just
              ( "assessmentTargetArn"
                  Prelude..= assessmentTargetArn
              ),
            Prelude.Just
              ( "assessmentTargetName"
                  Prelude..= assessmentTargetName
              )
          ]
      )

instance Prelude.ToPath UpdateAssessmentTarget where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAssessmentTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssessmentTargetResponse' smart constructor.
data UpdateAssessmentTargetResponse = UpdateAssessmentTargetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAssessmentTargetResponse ::
  UpdateAssessmentTargetResponse
newUpdateAssessmentTargetResponse =
  UpdateAssessmentTargetResponse'

instance
  Prelude.NFData
    UpdateAssessmentTargetResponse
