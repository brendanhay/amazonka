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
-- Module      : Network.AWS.IoT.Types.BehaviorModelTrainingSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BehaviorModelTrainingSummary where

import Network.AWS.IoT.Types.ModelStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary of an ML Detect behavior model.
--
-- /See:/ 'newBehaviorModelTrainingSummary' smart constructor.
data BehaviorModelTrainingSummary = BehaviorModelTrainingSummary'
  { -- | The date the model was last refreshed.
    lastModelRefreshDate :: Prelude.Maybe Prelude.POSIX,
    -- | The percentage of datapoints collected.
    datapointsCollectionPercentage :: Prelude.Maybe Prelude.Double,
    -- | The status of the behavior model.
    modelStatus :: Prelude.Maybe ModelStatus,
    -- | The name of the behavior.
    behaviorName :: Prelude.Maybe Prelude.Text,
    -- | The date a training model started collecting data.
    trainingDataCollectionStartDate :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the security profile.
    securityProfileName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      datapointsCollectionPercentage =
        Prelude.Nothing,
      modelStatus = Prelude.Nothing,
      behaviorName = Prelude.Nothing,
      trainingDataCollectionStartDate =
        Prelude.Nothing,
      securityProfileName = Prelude.Nothing
    }

-- | The date the model was last refreshed.
behaviorModelTrainingSummary_lastModelRefreshDate :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.UTCTime)
behaviorModelTrainingSummary_lastModelRefreshDate = Lens.lens (\BehaviorModelTrainingSummary' {lastModelRefreshDate} -> lastModelRefreshDate) (\s@BehaviorModelTrainingSummary' {} a -> s {lastModelRefreshDate = a} :: BehaviorModelTrainingSummary) Prelude.. Lens.mapping Prelude._Time

-- | The percentage of datapoints collected.
behaviorModelTrainingSummary_datapointsCollectionPercentage :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.Double)
behaviorModelTrainingSummary_datapointsCollectionPercentage = Lens.lens (\BehaviorModelTrainingSummary' {datapointsCollectionPercentage} -> datapointsCollectionPercentage) (\s@BehaviorModelTrainingSummary' {} a -> s {datapointsCollectionPercentage = a} :: BehaviorModelTrainingSummary)

-- | The status of the behavior model.
behaviorModelTrainingSummary_modelStatus :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe ModelStatus)
behaviorModelTrainingSummary_modelStatus = Lens.lens (\BehaviorModelTrainingSummary' {modelStatus} -> modelStatus) (\s@BehaviorModelTrainingSummary' {} a -> s {modelStatus = a} :: BehaviorModelTrainingSummary)

-- | The name of the behavior.
behaviorModelTrainingSummary_behaviorName :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.Text)
behaviorModelTrainingSummary_behaviorName = Lens.lens (\BehaviorModelTrainingSummary' {behaviorName} -> behaviorName) (\s@BehaviorModelTrainingSummary' {} a -> s {behaviorName = a} :: BehaviorModelTrainingSummary)

-- | The date a training model started collecting data.
behaviorModelTrainingSummary_trainingDataCollectionStartDate :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.UTCTime)
behaviorModelTrainingSummary_trainingDataCollectionStartDate = Lens.lens (\BehaviorModelTrainingSummary' {trainingDataCollectionStartDate} -> trainingDataCollectionStartDate) (\s@BehaviorModelTrainingSummary' {} a -> s {trainingDataCollectionStartDate = a} :: BehaviorModelTrainingSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the security profile.
behaviorModelTrainingSummary_securityProfileName :: Lens.Lens' BehaviorModelTrainingSummary (Prelude.Maybe Prelude.Text)
behaviorModelTrainingSummary_securityProfileName = Lens.lens (\BehaviorModelTrainingSummary' {securityProfileName} -> securityProfileName) (\s@BehaviorModelTrainingSummary' {} a -> s {securityProfileName = a} :: BehaviorModelTrainingSummary)

instance
  Prelude.FromJSON
    BehaviorModelTrainingSummary
  where
  parseJSON =
    Prelude.withObject
      "BehaviorModelTrainingSummary"
      ( \x ->
          BehaviorModelTrainingSummary'
            Prelude.<$> (x Prelude..:? "lastModelRefreshDate")
            Prelude.<*> (x Prelude..:? "datapointsCollectionPercentage")
            Prelude.<*> (x Prelude..:? "modelStatus")
            Prelude.<*> (x Prelude..:? "behaviorName")
            Prelude.<*> (x Prelude..:? "trainingDataCollectionStartDate")
            Prelude.<*> (x Prelude..:? "securityProfileName")
      )

instance
  Prelude.Hashable
    BehaviorModelTrainingSummary

instance Prelude.NFData BehaviorModelTrainingSummary
