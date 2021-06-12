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
-- Module      : Network.AWS.IoT.Types.BehaviorModelTrainingSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BehaviorModelTrainingSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ModelStatus
import qualified Network.AWS.Lens as Lens

-- | The summary of an ML Detect behavior model.
--
-- /See:/ 'newBehaviorModelTrainingSummary' smart constructor.
data BehaviorModelTrainingSummary = BehaviorModelTrainingSummary'
  { -- | The date the model was last refreshed.
    lastModelRefreshDate :: Core.Maybe Core.POSIX,
    -- | The percentage of datapoints collected.
    datapointsCollectionPercentage :: Core.Maybe Core.Double,
    -- | The status of the behavior model.
    modelStatus :: Core.Maybe ModelStatus,
    -- | The name of the behavior.
    behaviorName :: Core.Maybe Core.Text,
    -- | The date a training model started collecting data.
    trainingDataCollectionStartDate :: Core.Maybe Core.POSIX,
    -- | The name of the security profile.
    securityProfileName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BehaviorModelTrainingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModelRefreshDate', 'behaviorModelTrainingSummary_lastModelRefreshDate' - The date the model was last refreshed.
--
-- 'datapointsCollectionPercentage', 'behaviorModelTrainingSummary_datapointsCollectionPercentage' - The percentage of datapoints collected.
--
-- 'modelStatus', 'behaviorModelTrainingSummary_modelStatus' - The status of the behavior model.
--
-- 'behaviorName', 'behaviorModelTrainingSummary_behaviorName' - The name of the behavior.
--
-- 'trainingDataCollectionStartDate', 'behaviorModelTrainingSummary_trainingDataCollectionStartDate' - The date a training model started collecting data.
--
-- 'securityProfileName', 'behaviorModelTrainingSummary_securityProfileName' - The name of the security profile.
newBehaviorModelTrainingSummary ::
  BehaviorModelTrainingSummary
newBehaviorModelTrainingSummary =
  BehaviorModelTrainingSummary'
    { lastModelRefreshDate =
        Core.Nothing,
      datapointsCollectionPercentage = Core.Nothing,
      modelStatus = Core.Nothing,
      behaviorName = Core.Nothing,
      trainingDataCollectionStartDate =
        Core.Nothing,
      securityProfileName = Core.Nothing
    }

-- | The date the model was last refreshed.
behaviorModelTrainingSummary_lastModelRefreshDate :: Lens.Lens' BehaviorModelTrainingSummary (Core.Maybe Core.UTCTime)
behaviorModelTrainingSummary_lastModelRefreshDate = Lens.lens (\BehaviorModelTrainingSummary' {lastModelRefreshDate} -> lastModelRefreshDate) (\s@BehaviorModelTrainingSummary' {} a -> s {lastModelRefreshDate = a} :: BehaviorModelTrainingSummary) Core.. Lens.mapping Core._Time

-- | The percentage of datapoints collected.
behaviorModelTrainingSummary_datapointsCollectionPercentage :: Lens.Lens' BehaviorModelTrainingSummary (Core.Maybe Core.Double)
behaviorModelTrainingSummary_datapointsCollectionPercentage = Lens.lens (\BehaviorModelTrainingSummary' {datapointsCollectionPercentage} -> datapointsCollectionPercentage) (\s@BehaviorModelTrainingSummary' {} a -> s {datapointsCollectionPercentage = a} :: BehaviorModelTrainingSummary)

-- | The status of the behavior model.
behaviorModelTrainingSummary_modelStatus :: Lens.Lens' BehaviorModelTrainingSummary (Core.Maybe ModelStatus)
behaviorModelTrainingSummary_modelStatus = Lens.lens (\BehaviorModelTrainingSummary' {modelStatus} -> modelStatus) (\s@BehaviorModelTrainingSummary' {} a -> s {modelStatus = a} :: BehaviorModelTrainingSummary)

-- | The name of the behavior.
behaviorModelTrainingSummary_behaviorName :: Lens.Lens' BehaviorModelTrainingSummary (Core.Maybe Core.Text)
behaviorModelTrainingSummary_behaviorName = Lens.lens (\BehaviorModelTrainingSummary' {behaviorName} -> behaviorName) (\s@BehaviorModelTrainingSummary' {} a -> s {behaviorName = a} :: BehaviorModelTrainingSummary)

-- | The date a training model started collecting data.
behaviorModelTrainingSummary_trainingDataCollectionStartDate :: Lens.Lens' BehaviorModelTrainingSummary (Core.Maybe Core.UTCTime)
behaviorModelTrainingSummary_trainingDataCollectionStartDate = Lens.lens (\BehaviorModelTrainingSummary' {trainingDataCollectionStartDate} -> trainingDataCollectionStartDate) (\s@BehaviorModelTrainingSummary' {} a -> s {trainingDataCollectionStartDate = a} :: BehaviorModelTrainingSummary) Core.. Lens.mapping Core._Time

-- | The name of the security profile.
behaviorModelTrainingSummary_securityProfileName :: Lens.Lens' BehaviorModelTrainingSummary (Core.Maybe Core.Text)
behaviorModelTrainingSummary_securityProfileName = Lens.lens (\BehaviorModelTrainingSummary' {securityProfileName} -> securityProfileName) (\s@BehaviorModelTrainingSummary' {} a -> s {securityProfileName = a} :: BehaviorModelTrainingSummary)

instance Core.FromJSON BehaviorModelTrainingSummary where
  parseJSON =
    Core.withObject
      "BehaviorModelTrainingSummary"
      ( \x ->
          BehaviorModelTrainingSummary'
            Core.<$> (x Core..:? "lastModelRefreshDate")
            Core.<*> (x Core..:? "datapointsCollectionPercentage")
            Core.<*> (x Core..:? "modelStatus")
            Core.<*> (x Core..:? "behaviorName")
            Core.<*> (x Core..:? "trainingDataCollectionStartDate")
            Core.<*> (x Core..:? "securityProfileName")
      )

instance Core.Hashable BehaviorModelTrainingSummary

instance Core.NFData BehaviorModelTrainingSummary
