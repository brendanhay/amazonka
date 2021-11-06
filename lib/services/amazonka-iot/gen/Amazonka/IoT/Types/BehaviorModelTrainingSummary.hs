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
-- Module      : Amazonka.IoT.Types.BehaviorModelTrainingSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.BehaviorModelTrainingSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.ModelStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary of an ML Detect behavior model.
--
-- /See:/ 'newBehaviorModelTrainingSummary' smart constructor.
data BehaviorModelTrainingSummary = BehaviorModelTrainingSummary'
  { -- | The date the model was last refreshed.
    lastModelRefreshDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the behavior.
    behaviorName :: Prelude.Maybe Prelude.Text,
    -- | The percentage of datapoints collected.
    datapointsCollectionPercentage :: Prelude.Maybe Prelude.Double,
    -- | The name of the security profile.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The date a training model started collecting data.
    trainingDataCollectionStartDate :: Prelude.Maybe Core.POSIX,
    -- | The status of the behavior model.
    modelStatus :: Prelude.Maybe ModelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'behaviorName', 'behaviorModelTrainingSummary_behaviorName' - The name of the behavior.
--
-- 'datapointsCollectionPercentage', 'behaviorModelTrainingSummary_datapointsCollectionPercentage' - The percentage of datapoints collected.
--
-- 'securityProfileName', 'behaviorModelTrainingSummary_securityProfileName' - The name of the security profile.
--
-- 'trainingDataCollectionStartDate', 'behaviorModelTrainingSummary_trainingDataCollectionStartDate' - The date a training model started collecting data.
--
-- 'modelStatus', 'behaviorModelTrainingSummary_modelStatus' - The status of the behavior model.
newBehaviorModelTrainingSummary ::
  BehaviorModelTrainingSummary
newBehaviorModelTrainingSummary =
  BehaviorModelTrainingSummary'
    { lastModelRefreshDate =
        Prelude.Nothing,
      behaviorName = Prelude.Nothing,
      datapointsCollectionPercentage =
        Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      trainingDataCollectionStartDate =
        Prelude.Nothing,
      modelStatus = Prelude.Nothing
    }

-- | The date the model was last refreshed.
behaviorModelTrainingSummary_lastModelRefreshDate :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.UTCTime)
behaviorModelTrainingSummary_lastModelRefreshDate = Lens.lens (\BehaviorModelTrainingSummary' {lastModelRefreshDate} -> lastModelRefreshDate) (\s@BehaviorModelTrainingSummary' {} a -> s {lastModelRefreshDate = a} :: BehaviorModelTrainingSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the behavior.
behaviorModelTrainingSummary_behaviorName :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.Text)
behaviorModelTrainingSummary_behaviorName = Lens.lens (\BehaviorModelTrainingSummary' {behaviorName} -> behaviorName) (\s@BehaviorModelTrainingSummary' {} a -> s {behaviorName = a} :: BehaviorModelTrainingSummary)

-- | The percentage of datapoints collected.
behaviorModelTrainingSummary_datapointsCollectionPercentage :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.Double)
behaviorModelTrainingSummary_datapointsCollectionPercentage = Lens.lens (\BehaviorModelTrainingSummary' {datapointsCollectionPercentage} -> datapointsCollectionPercentage) (\s@BehaviorModelTrainingSummary' {} a -> s {datapointsCollectionPercentage = a} :: BehaviorModelTrainingSummary)

-- | The name of the security profile.
behaviorModelTrainingSummary_securityProfileName :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.Text)
behaviorModelTrainingSummary_securityProfileName = Lens.lens (\BehaviorModelTrainingSummary' {securityProfileName} -> securityProfileName) (\s@BehaviorModelTrainingSummary' {} a -> s {securityProfileName = a} :: BehaviorModelTrainingSummary)

-- | The date a training model started collecting data.
behaviorModelTrainingSummary_trainingDataCollectionStartDate :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.UTCTime)
behaviorModelTrainingSummary_trainingDataCollectionStartDate = Lens.lens (\BehaviorModelTrainingSummary' {trainingDataCollectionStartDate} -> trainingDataCollectionStartDate) (\s@BehaviorModelTrainingSummary' {} a -> s {trainingDataCollectionStartDate = a} :: BehaviorModelTrainingSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the behavior model.
behaviorModelTrainingSummary_modelStatus :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe ModelStatus)
behaviorModelTrainingSummary_modelStatus = Lens.lens (\BehaviorModelTrainingSummary' {modelStatus} -> modelStatus) (\s@BehaviorModelTrainingSummary' {} a -> s {modelStatus = a} :: BehaviorModelTrainingSummary)

instance Core.FromJSON BehaviorModelTrainingSummary where
  parseJSON =
    Core.withObject
      "BehaviorModelTrainingSummary"
      ( \x ->
          BehaviorModelTrainingSummary'
            Prelude.<$> (x Core..:? "lastModelRefreshDate")
            Prelude.<*> (x Core..:? "behaviorName")
            Prelude.<*> (x Core..:? "datapointsCollectionPercentage")
            Prelude.<*> (x Core..:? "securityProfileName")
            Prelude.<*> (x Core..:? "trainingDataCollectionStartDate")
            Prelude.<*> (x Core..:? "modelStatus")
      )

instance
  Prelude.Hashable
    BehaviorModelTrainingSummary

instance Prelude.NFData BehaviorModelTrainingSummary
