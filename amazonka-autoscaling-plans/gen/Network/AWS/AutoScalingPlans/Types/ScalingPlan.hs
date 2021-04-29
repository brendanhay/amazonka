{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPlan where

import Network.AWS.AutoScalingPlans.Types.ApplicationSource
import Network.AWS.AutoScalingPlans.Types.ScalingInstruction
import Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a scaling plan.
--
-- /See:/ 'newScalingPlan' smart constructor.
data ScalingPlan = ScalingPlan'
  { -- | A simple message about the current status of the scaling plan.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Unix time stamp when the scaling plan was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Unix time stamp when the scaling plan entered the current status.
    statusStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the scaling plan.
    scalingPlanName :: Prelude.Text,
    -- | The version number of the scaling plan.
    scalingPlanVersion :: Prelude.Integer,
    -- | A CloudFormation stack or a set of tags. You can create one scaling plan
    -- per application source.
    applicationSource :: ApplicationSource,
    -- | The scaling instructions.
    scalingInstructions :: [ScalingInstruction],
    -- | The status of the scaling plan.
    --
    -- -   @Active@ - The scaling plan is active.
    --
    -- -   @ActiveWithProblems@ - The scaling plan is active, but the scaling
    --     configuration for one or more resources could not be applied.
    --
    -- -   @CreationInProgress@ - The scaling plan is being created.
    --
    -- -   @CreationFailed@ - The scaling plan could not be created.
    --
    -- -   @DeletionInProgress@ - The scaling plan is being deleted.
    --
    -- -   @DeletionFailed@ - The scaling plan could not be deleted.
    --
    -- -   @UpdateInProgress@ - The scaling plan is being updated.
    --
    -- -   @UpdateFailed@ - The scaling plan could not be updated.
    statusCode :: ScalingPlanStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScalingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'scalingPlan_statusMessage' - A simple message about the current status of the scaling plan.
--
-- 'creationTime', 'scalingPlan_creationTime' - The Unix time stamp when the scaling plan was created.
--
-- 'statusStartTime', 'scalingPlan_statusStartTime' - The Unix time stamp when the scaling plan entered the current status.
--
-- 'scalingPlanName', 'scalingPlan_scalingPlanName' - The name of the scaling plan.
--
-- 'scalingPlanVersion', 'scalingPlan_scalingPlanVersion' - The version number of the scaling plan.
--
-- 'applicationSource', 'scalingPlan_applicationSource' - A CloudFormation stack or a set of tags. You can create one scaling plan
-- per application source.
--
-- 'scalingInstructions', 'scalingPlan_scalingInstructions' - The scaling instructions.
--
-- 'statusCode', 'scalingPlan_statusCode' - The status of the scaling plan.
--
-- -   @Active@ - The scaling plan is active.
--
-- -   @ActiveWithProblems@ - The scaling plan is active, but the scaling
--     configuration for one or more resources could not be applied.
--
-- -   @CreationInProgress@ - The scaling plan is being created.
--
-- -   @CreationFailed@ - The scaling plan could not be created.
--
-- -   @DeletionInProgress@ - The scaling plan is being deleted.
--
-- -   @DeletionFailed@ - The scaling plan could not be deleted.
--
-- -   @UpdateInProgress@ - The scaling plan is being updated.
--
-- -   @UpdateFailed@ - The scaling plan could not be updated.
newScalingPlan ::
  -- | 'scalingPlanName'
  Prelude.Text ->
  -- | 'scalingPlanVersion'
  Prelude.Integer ->
  -- | 'applicationSource'
  ApplicationSource ->
  -- | 'statusCode'
  ScalingPlanStatusCode ->
  ScalingPlan
newScalingPlan
  pScalingPlanName_
  pScalingPlanVersion_
  pApplicationSource_
  pStatusCode_ =
    ScalingPlan'
      { statusMessage = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        statusStartTime = Prelude.Nothing,
        scalingPlanName = pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_,
        applicationSource = pApplicationSource_,
        scalingInstructions = Prelude.mempty,
        statusCode = pStatusCode_
      }

-- | A simple message about the current status of the scaling plan.
scalingPlan_statusMessage :: Lens.Lens' ScalingPlan (Prelude.Maybe Prelude.Text)
scalingPlan_statusMessage = Lens.lens (\ScalingPlan' {statusMessage} -> statusMessage) (\s@ScalingPlan' {} a -> s {statusMessage = a} :: ScalingPlan)

-- | The Unix time stamp when the scaling plan was created.
scalingPlan_creationTime :: Lens.Lens' ScalingPlan (Prelude.Maybe Prelude.UTCTime)
scalingPlan_creationTime = Lens.lens (\ScalingPlan' {creationTime} -> creationTime) (\s@ScalingPlan' {} a -> s {creationTime = a} :: ScalingPlan) Prelude.. Lens.mapping Prelude._Time

-- | The Unix time stamp when the scaling plan entered the current status.
scalingPlan_statusStartTime :: Lens.Lens' ScalingPlan (Prelude.Maybe Prelude.UTCTime)
scalingPlan_statusStartTime = Lens.lens (\ScalingPlan' {statusStartTime} -> statusStartTime) (\s@ScalingPlan' {} a -> s {statusStartTime = a} :: ScalingPlan) Prelude.. Lens.mapping Prelude._Time

-- | The name of the scaling plan.
scalingPlan_scalingPlanName :: Lens.Lens' ScalingPlan Prelude.Text
scalingPlan_scalingPlanName = Lens.lens (\ScalingPlan' {scalingPlanName} -> scalingPlanName) (\s@ScalingPlan' {} a -> s {scalingPlanName = a} :: ScalingPlan)

-- | The version number of the scaling plan.
scalingPlan_scalingPlanVersion :: Lens.Lens' ScalingPlan Prelude.Integer
scalingPlan_scalingPlanVersion = Lens.lens (\ScalingPlan' {scalingPlanVersion} -> scalingPlanVersion) (\s@ScalingPlan' {} a -> s {scalingPlanVersion = a} :: ScalingPlan)

-- | A CloudFormation stack or a set of tags. You can create one scaling plan
-- per application source.
scalingPlan_applicationSource :: Lens.Lens' ScalingPlan ApplicationSource
scalingPlan_applicationSource = Lens.lens (\ScalingPlan' {applicationSource} -> applicationSource) (\s@ScalingPlan' {} a -> s {applicationSource = a} :: ScalingPlan)

-- | The scaling instructions.
scalingPlan_scalingInstructions :: Lens.Lens' ScalingPlan [ScalingInstruction]
scalingPlan_scalingInstructions = Lens.lens (\ScalingPlan' {scalingInstructions} -> scalingInstructions) (\s@ScalingPlan' {} a -> s {scalingInstructions = a} :: ScalingPlan) Prelude.. Prelude._Coerce

-- | The status of the scaling plan.
--
-- -   @Active@ - The scaling plan is active.
--
-- -   @ActiveWithProblems@ - The scaling plan is active, but the scaling
--     configuration for one or more resources could not be applied.
--
-- -   @CreationInProgress@ - The scaling plan is being created.
--
-- -   @CreationFailed@ - The scaling plan could not be created.
--
-- -   @DeletionInProgress@ - The scaling plan is being deleted.
--
-- -   @DeletionFailed@ - The scaling plan could not be deleted.
--
-- -   @UpdateInProgress@ - The scaling plan is being updated.
--
-- -   @UpdateFailed@ - The scaling plan could not be updated.
scalingPlan_statusCode :: Lens.Lens' ScalingPlan ScalingPlanStatusCode
scalingPlan_statusCode = Lens.lens (\ScalingPlan' {statusCode} -> statusCode) (\s@ScalingPlan' {} a -> s {statusCode = a} :: ScalingPlan)

instance Prelude.FromJSON ScalingPlan where
  parseJSON =
    Prelude.withObject
      "ScalingPlan"
      ( \x ->
          ScalingPlan'
            Prelude.<$> (x Prelude..:? "StatusMessage")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "StatusStartTime")
            Prelude.<*> (x Prelude..: "ScalingPlanName")
            Prelude.<*> (x Prelude..: "ScalingPlanVersion")
            Prelude.<*> (x Prelude..: "ApplicationSource")
            Prelude.<*> ( x Prelude..:? "ScalingInstructions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "StatusCode")
      )

instance Prelude.Hashable ScalingPlan

instance Prelude.NFData ScalingPlan
