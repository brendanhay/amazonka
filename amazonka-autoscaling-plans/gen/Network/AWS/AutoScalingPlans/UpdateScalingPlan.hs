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
-- Module      : Network.AWS.AutoScalingPlans.UpdateScalingPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified scaling plan.
--
-- You cannot update a scaling plan if it is in the process of being
-- created, updated, or deleted.
module Network.AWS.AutoScalingPlans.UpdateScalingPlan
  ( -- * Creating a Request
    UpdateScalingPlan (..),
    newUpdateScalingPlan,

    -- * Request Lenses
    updateScalingPlan_applicationSource,
    updateScalingPlan_scalingInstructions,
    updateScalingPlan_scalingPlanName,
    updateScalingPlan_scalingPlanVersion,

    -- * Destructuring the Response
    UpdateScalingPlanResponse (..),
    newUpdateScalingPlanResponse,

    -- * Response Lenses
    updateScalingPlanResponse_httpStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateScalingPlan' smart constructor.
data UpdateScalingPlan = UpdateScalingPlan'
  { -- | A CloudFormation stack or set of tags.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
    -- in the /AWS Auto Scaling API Reference/.
    applicationSource :: Core.Maybe ApplicationSource,
    -- | The scaling instructions.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
    -- in the /AWS Auto Scaling API Reference/.
    scalingInstructions :: Core.Maybe [ScalingInstruction],
    -- | The name of the scaling plan.
    scalingPlanName :: Core.Text,
    -- | The version number of the scaling plan. The only valid value is @1@.
    -- Currently, you cannot have multiple scaling plan versions.
    scalingPlanVersion :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateScalingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationSource', 'updateScalingPlan_applicationSource' - A CloudFormation stack or set of tags.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
-- in the /AWS Auto Scaling API Reference/.
--
-- 'scalingInstructions', 'updateScalingPlan_scalingInstructions' - The scaling instructions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
-- in the /AWS Auto Scaling API Reference/.
--
-- 'scalingPlanName', 'updateScalingPlan_scalingPlanName' - The name of the scaling plan.
--
-- 'scalingPlanVersion', 'updateScalingPlan_scalingPlanVersion' - The version number of the scaling plan. The only valid value is @1@.
-- Currently, you cannot have multiple scaling plan versions.
newUpdateScalingPlan ::
  -- | 'scalingPlanName'
  Core.Text ->
  -- | 'scalingPlanVersion'
  Core.Integer ->
  UpdateScalingPlan
newUpdateScalingPlan
  pScalingPlanName_
  pScalingPlanVersion_ =
    UpdateScalingPlan'
      { applicationSource =
          Core.Nothing,
        scalingInstructions = Core.Nothing,
        scalingPlanName = pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_
      }

-- | A CloudFormation stack or set of tags.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
-- in the /AWS Auto Scaling API Reference/.
updateScalingPlan_applicationSource :: Lens.Lens' UpdateScalingPlan (Core.Maybe ApplicationSource)
updateScalingPlan_applicationSource = Lens.lens (\UpdateScalingPlan' {applicationSource} -> applicationSource) (\s@UpdateScalingPlan' {} a -> s {applicationSource = a} :: UpdateScalingPlan)

-- | The scaling instructions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
-- in the /AWS Auto Scaling API Reference/.
updateScalingPlan_scalingInstructions :: Lens.Lens' UpdateScalingPlan (Core.Maybe [ScalingInstruction])
updateScalingPlan_scalingInstructions = Lens.lens (\UpdateScalingPlan' {scalingInstructions} -> scalingInstructions) (\s@UpdateScalingPlan' {} a -> s {scalingInstructions = a} :: UpdateScalingPlan) Core.. Lens.mapping Lens._Coerce

-- | The name of the scaling plan.
updateScalingPlan_scalingPlanName :: Lens.Lens' UpdateScalingPlan Core.Text
updateScalingPlan_scalingPlanName = Lens.lens (\UpdateScalingPlan' {scalingPlanName} -> scalingPlanName) (\s@UpdateScalingPlan' {} a -> s {scalingPlanName = a} :: UpdateScalingPlan)

-- | The version number of the scaling plan. The only valid value is @1@.
-- Currently, you cannot have multiple scaling plan versions.
updateScalingPlan_scalingPlanVersion :: Lens.Lens' UpdateScalingPlan Core.Integer
updateScalingPlan_scalingPlanVersion = Lens.lens (\UpdateScalingPlan' {scalingPlanVersion} -> scalingPlanVersion) (\s@UpdateScalingPlan' {} a -> s {scalingPlanVersion = a} :: UpdateScalingPlan)

instance Core.AWSRequest UpdateScalingPlan where
  type
    AWSResponse UpdateScalingPlan =
      UpdateScalingPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateScalingPlanResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateScalingPlan

instance Core.NFData UpdateScalingPlan

instance Core.ToHeaders UpdateScalingPlan where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AnyScaleScalingPlannerFrontendService.UpdateScalingPlan" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateScalingPlan where
  toJSON UpdateScalingPlan' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ApplicationSource" Core..=)
              Core.<$> applicationSource,
            ("ScalingInstructions" Core..=)
              Core.<$> scalingInstructions,
            Core.Just
              ("ScalingPlanName" Core..= scalingPlanName),
            Core.Just
              ("ScalingPlanVersion" Core..= scalingPlanVersion)
          ]
      )

instance Core.ToPath UpdateScalingPlan where
  toPath = Core.const "/"

instance Core.ToQuery UpdateScalingPlan where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateScalingPlanResponse' smart constructor.
data UpdateScalingPlanResponse = UpdateScalingPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateScalingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateScalingPlanResponse_httpStatus' - The response's http status code.
newUpdateScalingPlanResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateScalingPlanResponse
newUpdateScalingPlanResponse pHttpStatus_ =
  UpdateScalingPlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateScalingPlanResponse_httpStatus :: Lens.Lens' UpdateScalingPlanResponse Core.Int
updateScalingPlanResponse_httpStatus = Lens.lens (\UpdateScalingPlanResponse' {httpStatus} -> httpStatus) (\s@UpdateScalingPlanResponse' {} a -> s {httpStatus = a} :: UpdateScalingPlanResponse)

instance Core.NFData UpdateScalingPlanResponse
