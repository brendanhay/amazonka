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
-- Module      : Amazonka.AutoScalingPlans.Types.ScalingPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ScalingPlan where

import Amazonka.AutoScalingPlans.Types.ApplicationSource
import Amazonka.AutoScalingPlans.Types.ScalingInstruction
import Amazonka.AutoScalingPlans.Types.ScalingPlanStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a scaling plan.
--
-- /See:/ 'newScalingPlan' smart constructor.
data ScalingPlan = ScalingPlan'
  { -- | The Unix time stamp when the scaling plan was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A simple message about the current status of the scaling plan.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Unix time stamp when the scaling plan entered the current status.
    statusStartTime :: Prelude.Maybe Data.POSIX,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'scalingPlan_creationTime' - The Unix time stamp when the scaling plan was created.
--
-- 'statusMessage', 'scalingPlan_statusMessage' - A simple message about the current status of the scaling plan.
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
      { creationTime = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        statusStartTime = Prelude.Nothing,
        scalingPlanName = pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_,
        applicationSource = pApplicationSource_,
        scalingInstructions = Prelude.mempty,
        statusCode = pStatusCode_
      }

-- | The Unix time stamp when the scaling plan was created.
scalingPlan_creationTime :: Lens.Lens' ScalingPlan (Prelude.Maybe Prelude.UTCTime)
scalingPlan_creationTime = Lens.lens (\ScalingPlan' {creationTime} -> creationTime) (\s@ScalingPlan' {} a -> s {creationTime = a} :: ScalingPlan) Prelude.. Lens.mapping Data._Time

-- | A simple message about the current status of the scaling plan.
scalingPlan_statusMessage :: Lens.Lens' ScalingPlan (Prelude.Maybe Prelude.Text)
scalingPlan_statusMessage = Lens.lens (\ScalingPlan' {statusMessage} -> statusMessage) (\s@ScalingPlan' {} a -> s {statusMessage = a} :: ScalingPlan)

-- | The Unix time stamp when the scaling plan entered the current status.
scalingPlan_statusStartTime :: Lens.Lens' ScalingPlan (Prelude.Maybe Prelude.UTCTime)
scalingPlan_statusStartTime = Lens.lens (\ScalingPlan' {statusStartTime} -> statusStartTime) (\s@ScalingPlan' {} a -> s {statusStartTime = a} :: ScalingPlan) Prelude.. Lens.mapping Data._Time

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
scalingPlan_scalingInstructions = Lens.lens (\ScalingPlan' {scalingInstructions} -> scalingInstructions) (\s@ScalingPlan' {} a -> s {scalingInstructions = a} :: ScalingPlan) Prelude.. Lens.coerced

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

instance Data.FromJSON ScalingPlan where
  parseJSON =
    Data.withObject
      "ScalingPlan"
      ( \x ->
          ScalingPlan'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "StatusStartTime")
            Prelude.<*> (x Data..: "ScalingPlanName")
            Prelude.<*> (x Data..: "ScalingPlanVersion")
            Prelude.<*> (x Data..: "ApplicationSource")
            Prelude.<*> ( x
                            Data..:? "ScalingInstructions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "StatusCode")
      )

instance Prelude.Hashable ScalingPlan where
  hashWithSalt _salt ScalingPlan' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` statusStartTime
      `Prelude.hashWithSalt` scalingPlanName
      `Prelude.hashWithSalt` scalingPlanVersion
      `Prelude.hashWithSalt` applicationSource
      `Prelude.hashWithSalt` scalingInstructions
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData ScalingPlan where
  rnf ScalingPlan' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf statusMessage `Prelude.seq`
        Prelude.rnf statusStartTime `Prelude.seq`
          Prelude.rnf scalingPlanName `Prelude.seq`
            Prelude.rnf scalingPlanVersion `Prelude.seq`
              Prelude.rnf applicationSource `Prelude.seq`
                Prelude.rnf scalingInstructions `Prelude.seq`
                  Prelude.rnf statusCode
