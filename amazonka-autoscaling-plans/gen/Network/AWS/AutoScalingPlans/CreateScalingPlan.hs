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
-- Module      : Network.AWS.AutoScalingPlans.CreateScalingPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scaling plan.
module Network.AWS.AutoScalingPlans.CreateScalingPlan
  ( -- * Creating a Request
    CreateScalingPlan (..),
    newCreateScalingPlan,

    -- * Request Lenses
    createScalingPlan_scalingPlanName,
    createScalingPlan_applicationSource,
    createScalingPlan_scalingInstructions,

    -- * Destructuring the Response
    CreateScalingPlanResponse (..),
    newCreateScalingPlanResponse,

    -- * Response Lenses
    createScalingPlanResponse_httpStatus,
    createScalingPlanResponse_scalingPlanVersion,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateScalingPlan' smart constructor.
data CreateScalingPlan = CreateScalingPlan'
  { -- | The name of the scaling plan. Names cannot contain vertical bars,
    -- colons, or forward slashes.
    scalingPlanName :: Prelude.Text,
    -- | A CloudFormation stack or set of tags. You can create one scaling plan
    -- per application source.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
    -- in the /AWS Auto Scaling API Reference/.
    applicationSource :: ApplicationSource,
    -- | The scaling instructions.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
    -- in the /AWS Auto Scaling API Reference/.
    scalingInstructions :: [ScalingInstruction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateScalingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingPlanName', 'createScalingPlan_scalingPlanName' - The name of the scaling plan. Names cannot contain vertical bars,
-- colons, or forward slashes.
--
-- 'applicationSource', 'createScalingPlan_applicationSource' - A CloudFormation stack or set of tags. You can create one scaling plan
-- per application source.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
-- in the /AWS Auto Scaling API Reference/.
--
-- 'scalingInstructions', 'createScalingPlan_scalingInstructions' - The scaling instructions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
-- in the /AWS Auto Scaling API Reference/.
newCreateScalingPlan ::
  -- | 'scalingPlanName'
  Prelude.Text ->
  -- | 'applicationSource'
  ApplicationSource ->
  CreateScalingPlan
newCreateScalingPlan
  pScalingPlanName_
  pApplicationSource_ =
    CreateScalingPlan'
      { scalingPlanName =
          pScalingPlanName_,
        applicationSource = pApplicationSource_,
        scalingInstructions = Prelude.mempty
      }

-- | The name of the scaling plan. Names cannot contain vertical bars,
-- colons, or forward slashes.
createScalingPlan_scalingPlanName :: Lens.Lens' CreateScalingPlan Prelude.Text
createScalingPlan_scalingPlanName = Lens.lens (\CreateScalingPlan' {scalingPlanName} -> scalingPlanName) (\s@CreateScalingPlan' {} a -> s {scalingPlanName = a} :: CreateScalingPlan)

-- | A CloudFormation stack or set of tags. You can create one scaling plan
-- per application source.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ApplicationSource.html ApplicationSource>
-- in the /AWS Auto Scaling API Reference/.
createScalingPlan_applicationSource :: Lens.Lens' CreateScalingPlan ApplicationSource
createScalingPlan_applicationSource = Lens.lens (\CreateScalingPlan' {applicationSource} -> applicationSource) (\s@CreateScalingPlan' {} a -> s {applicationSource = a} :: CreateScalingPlan)

-- | The scaling instructions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/APIReference/API_ScalingInstruction.html ScalingInstruction>
-- in the /AWS Auto Scaling API Reference/.
createScalingPlan_scalingInstructions :: Lens.Lens' CreateScalingPlan [ScalingInstruction]
createScalingPlan_scalingInstructions = Lens.lens (\CreateScalingPlan' {scalingInstructions} -> scalingInstructions) (\s@CreateScalingPlan' {} a -> s {scalingInstructions = a} :: CreateScalingPlan) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreateScalingPlan where
  type Rs CreateScalingPlan = CreateScalingPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScalingPlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ScalingPlanVersion")
      )

instance Prelude.Hashable CreateScalingPlan

instance Prelude.NFData CreateScalingPlan

instance Prelude.ToHeaders CreateScalingPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AnyScaleScalingPlannerFrontendService.CreateScalingPlan" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateScalingPlan where
  toJSON CreateScalingPlan' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScalingPlanName" Prelude..= scalingPlanName),
            Prelude.Just
              ("ApplicationSource" Prelude..= applicationSource),
            Prelude.Just
              ( "ScalingInstructions"
                  Prelude..= scalingInstructions
              )
          ]
      )

instance Prelude.ToPath CreateScalingPlan where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateScalingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScalingPlanResponse' smart constructor.
data CreateScalingPlanResponse = CreateScalingPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The version number of the scaling plan. This value is always @1@.
    -- Currently, you cannot have multiple scaling plan versions.
    scalingPlanVersion :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateScalingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createScalingPlanResponse_httpStatus' - The response's http status code.
--
-- 'scalingPlanVersion', 'createScalingPlanResponse_scalingPlanVersion' - The version number of the scaling plan. This value is always @1@.
-- Currently, you cannot have multiple scaling plan versions.
newCreateScalingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'scalingPlanVersion'
  Prelude.Integer ->
  CreateScalingPlanResponse
newCreateScalingPlanResponse
  pHttpStatus_
  pScalingPlanVersion_ =
    CreateScalingPlanResponse'
      { httpStatus =
          pHttpStatus_,
        scalingPlanVersion = pScalingPlanVersion_
      }

-- | The response's http status code.
createScalingPlanResponse_httpStatus :: Lens.Lens' CreateScalingPlanResponse Prelude.Int
createScalingPlanResponse_httpStatus = Lens.lens (\CreateScalingPlanResponse' {httpStatus} -> httpStatus) (\s@CreateScalingPlanResponse' {} a -> s {httpStatus = a} :: CreateScalingPlanResponse)

-- | The version number of the scaling plan. This value is always @1@.
-- Currently, you cannot have multiple scaling plan versions.
createScalingPlanResponse_scalingPlanVersion :: Lens.Lens' CreateScalingPlanResponse Prelude.Integer
createScalingPlanResponse_scalingPlanVersion = Lens.lens (\CreateScalingPlanResponse' {scalingPlanVersion} -> scalingPlanVersion) (\s@CreateScalingPlanResponse' {} a -> s {scalingPlanVersion = a} :: CreateScalingPlanResponse)

instance Prelude.NFData CreateScalingPlanResponse
