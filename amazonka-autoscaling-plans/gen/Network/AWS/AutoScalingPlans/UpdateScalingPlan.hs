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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateScalingPlan' smart constructor.
data UpdateScalingPlan = UpdateScalingPlan'
  { -- | A CloudFormation stack or set of tags.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
    -- in the /AWS Auto Scaling API Reference/.
    applicationSource :: Prelude.Maybe ApplicationSource,
    -- | The scaling instructions.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
    -- in the /AWS Auto Scaling API Reference/.
    scalingInstructions :: Prelude.Maybe [ScalingInstruction],
    -- | The name of the scaling plan.
    scalingPlanName :: Prelude.Text,
    -- | The version number of the scaling plan. The only valid value is @1@.
    -- Currently, you cannot have multiple scaling plan versions.
    scalingPlanVersion :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'scalingPlanVersion'
  Prelude.Integer ->
  UpdateScalingPlan
newUpdateScalingPlan
  pScalingPlanName_
  pScalingPlanVersion_ =
    UpdateScalingPlan'
      { applicationSource =
          Prelude.Nothing,
        scalingInstructions = Prelude.Nothing,
        scalingPlanName = pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_
      }

-- | A CloudFormation stack or set of tags.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
-- in the /AWS Auto Scaling API Reference/.
updateScalingPlan_applicationSource :: Lens.Lens' UpdateScalingPlan (Prelude.Maybe ApplicationSource)
updateScalingPlan_applicationSource = Lens.lens (\UpdateScalingPlan' {applicationSource} -> applicationSource) (\s@UpdateScalingPlan' {} a -> s {applicationSource = a} :: UpdateScalingPlan)

-- | The scaling instructions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
-- in the /AWS Auto Scaling API Reference/.
updateScalingPlan_scalingInstructions :: Lens.Lens' UpdateScalingPlan (Prelude.Maybe [ScalingInstruction])
updateScalingPlan_scalingInstructions = Lens.lens (\UpdateScalingPlan' {scalingInstructions} -> scalingInstructions) (\s@UpdateScalingPlan' {} a -> s {scalingInstructions = a} :: UpdateScalingPlan) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the scaling plan.
updateScalingPlan_scalingPlanName :: Lens.Lens' UpdateScalingPlan Prelude.Text
updateScalingPlan_scalingPlanName = Lens.lens (\UpdateScalingPlan' {scalingPlanName} -> scalingPlanName) (\s@UpdateScalingPlan' {} a -> s {scalingPlanName = a} :: UpdateScalingPlan)

-- | The version number of the scaling plan. The only valid value is @1@.
-- Currently, you cannot have multiple scaling plan versions.
updateScalingPlan_scalingPlanVersion :: Lens.Lens' UpdateScalingPlan Prelude.Integer
updateScalingPlan_scalingPlanVersion = Lens.lens (\UpdateScalingPlan' {scalingPlanVersion} -> scalingPlanVersion) (\s@UpdateScalingPlan' {} a -> s {scalingPlanVersion = a} :: UpdateScalingPlan)

instance Prelude.AWSRequest UpdateScalingPlan where
  type Rs UpdateScalingPlan = UpdateScalingPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateScalingPlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateScalingPlan

instance Prelude.NFData UpdateScalingPlan

instance Prelude.ToHeaders UpdateScalingPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AnyScaleScalingPlannerFrontendService.UpdateScalingPlan" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateScalingPlan where
  toJSON UpdateScalingPlan' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ApplicationSource" Prelude..=)
              Prelude.<$> applicationSource,
            ("ScalingInstructions" Prelude..=)
              Prelude.<$> scalingInstructions,
            Prelude.Just
              ("ScalingPlanName" Prelude..= scalingPlanName),
            Prelude.Just
              ( "ScalingPlanVersion"
                  Prelude..= scalingPlanVersion
              )
          ]
      )

instance Prelude.ToPath UpdateScalingPlan where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateScalingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateScalingPlanResponse' smart constructor.
data UpdateScalingPlanResponse = UpdateScalingPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateScalingPlanResponse
newUpdateScalingPlanResponse pHttpStatus_ =
  UpdateScalingPlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateScalingPlanResponse_httpStatus :: Lens.Lens' UpdateScalingPlanResponse Prelude.Int
updateScalingPlanResponse_httpStatus = Lens.lens (\UpdateScalingPlanResponse' {httpStatus} -> httpStatus) (\s@UpdateScalingPlanResponse' {} a -> s {httpStatus = a} :: UpdateScalingPlanResponse)

instance Prelude.NFData UpdateScalingPlanResponse
