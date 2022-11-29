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
-- Module      : Amazonka.IoTAnalytics.Types.Dataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Dataset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTAnalytics.Types.DatasetAction
import Amazonka.IoTAnalytics.Types.DatasetContentDeliveryRule
import Amazonka.IoTAnalytics.Types.DatasetStatus
import Amazonka.IoTAnalytics.Types.DatasetTrigger
import Amazonka.IoTAnalytics.Types.LateDataRule
import Amazonka.IoTAnalytics.Types.RetentionPeriod
import Amazonka.IoTAnalytics.Types.VersioningConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Information about a dataset.
--
-- /See:/ 'newDataset' smart constructor.
data Dataset = Dataset'
  { -- | The name of the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of data rules that send notifications to CloudWatch, when data
    -- arrives late. To specify @lateDataRules@, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    lateDataRules :: Prelude.Maybe (Prelude.NonEmpty LateDataRule),
    -- | Optional. How many versions of dataset contents are kept. If not
    -- specified or set to null, only the latest version plus the latest
    -- succeeded version (if they are different) are kept for the time period
    -- specified by the @retentionPeriod@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
    -- in the /IoT Analytics User Guide/.
    versioningConfiguration :: Prelude.Maybe VersioningConfiguration,
    -- | The ARN of the dataset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The @DatasetTrigger@ objects that specify when the dataset is
    -- automatically updated.
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | When dataset contents are created they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Prelude.Maybe [DatasetContentDeliveryRule],
    -- | Optional. How long, in days, message data is kept for the dataset.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | When the dataset was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The last time the dataset was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The @DatasetAction@ objects that automatically create the dataset
    -- contents.
    actions :: Prelude.Maybe (Prelude.NonEmpty DatasetAction)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Dataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dataset_name' - The name of the dataset.
--
-- 'lateDataRules', 'dataset_lateDataRules' - A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'versioningConfiguration', 'dataset_versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
-- in the /IoT Analytics User Guide/.
--
-- 'arn', 'dataset_arn' - The ARN of the dataset.
--
-- 'status', 'dataset_status' - The status of the dataset.
--
-- 'triggers', 'dataset_triggers' - The @DatasetTrigger@ objects that specify when the dataset is
-- automatically updated.
--
-- 'contentDeliveryRules', 'dataset_contentDeliveryRules' - When dataset contents are created they are delivered to destinations
-- specified here.
--
-- 'retentionPeriod', 'dataset_retentionPeriod' - Optional. How long, in days, message data is kept for the dataset.
--
-- 'creationTime', 'dataset_creationTime' - When the dataset was created.
--
-- 'lastUpdateTime', 'dataset_lastUpdateTime' - The last time the dataset was updated.
--
-- 'actions', 'dataset_actions' - The @DatasetAction@ objects that automatically create the dataset
-- contents.
newDataset ::
  Dataset
newDataset =
  Dataset'
    { name = Prelude.Nothing,
      lateDataRules = Prelude.Nothing,
      versioningConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      triggers = Prelude.Nothing,
      contentDeliveryRules = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      actions = Prelude.Nothing
    }

-- | The name of the dataset.
dataset_name :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_name = Lens.lens (\Dataset' {name} -> name) (\s@Dataset' {} a -> s {name = a} :: Dataset)

-- | A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
dataset_lateDataRules :: Lens.Lens' Dataset (Prelude.Maybe (Prelude.NonEmpty LateDataRule))
dataset_lateDataRules = Lens.lens (\Dataset' {lateDataRules} -> lateDataRules) (\s@Dataset' {} a -> s {lateDataRules = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
-- in the /IoT Analytics User Guide/.
dataset_versioningConfiguration :: Lens.Lens' Dataset (Prelude.Maybe VersioningConfiguration)
dataset_versioningConfiguration = Lens.lens (\Dataset' {versioningConfiguration} -> versioningConfiguration) (\s@Dataset' {} a -> s {versioningConfiguration = a} :: Dataset)

-- | The ARN of the dataset.
dataset_arn :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_arn = Lens.lens (\Dataset' {arn} -> arn) (\s@Dataset' {} a -> s {arn = a} :: Dataset)

-- | The status of the dataset.
dataset_status :: Lens.Lens' Dataset (Prelude.Maybe DatasetStatus)
dataset_status = Lens.lens (\Dataset' {status} -> status) (\s@Dataset' {} a -> s {status = a} :: Dataset)

-- | The @DatasetTrigger@ objects that specify when the dataset is
-- automatically updated.
dataset_triggers :: Lens.Lens' Dataset (Prelude.Maybe [DatasetTrigger])
dataset_triggers = Lens.lens (\Dataset' {triggers} -> triggers) (\s@Dataset' {} a -> s {triggers = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | When dataset contents are created they are delivered to destinations
-- specified here.
dataset_contentDeliveryRules :: Lens.Lens' Dataset (Prelude.Maybe [DatasetContentDeliveryRule])
dataset_contentDeliveryRules = Lens.lens (\Dataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@Dataset' {} a -> s {contentDeliveryRules = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | Optional. How long, in days, message data is kept for the dataset.
dataset_retentionPeriod :: Lens.Lens' Dataset (Prelude.Maybe RetentionPeriod)
dataset_retentionPeriod = Lens.lens (\Dataset' {retentionPeriod} -> retentionPeriod) (\s@Dataset' {} a -> s {retentionPeriod = a} :: Dataset)

-- | When the dataset was created.
dataset_creationTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_creationTime = Lens.lens (\Dataset' {creationTime} -> creationTime) (\s@Dataset' {} a -> s {creationTime = a} :: Dataset) Prelude.. Lens.mapping Core._Time

-- | The last time the dataset was updated.
dataset_lastUpdateTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_lastUpdateTime = Lens.lens (\Dataset' {lastUpdateTime} -> lastUpdateTime) (\s@Dataset' {} a -> s {lastUpdateTime = a} :: Dataset) Prelude.. Lens.mapping Core._Time

-- | The @DatasetAction@ objects that automatically create the dataset
-- contents.
dataset_actions :: Lens.Lens' Dataset (Prelude.Maybe (Prelude.NonEmpty DatasetAction))
dataset_actions = Lens.lens (\Dataset' {actions} -> actions) (\s@Dataset' {} a -> s {actions = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Dataset where
  parseJSON =
    Core.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "lateDataRules")
            Prelude.<*> (x Core..:? "versioningConfiguration")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "triggers" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "contentDeliveryRules"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "retentionPeriod")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "actions")
      )

instance Prelude.Hashable Dataset where
  hashWithSalt _salt Dataset' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lateDataRules
      `Prelude.hashWithSalt` versioningConfiguration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` triggers
      `Prelude.hashWithSalt` contentDeliveryRules
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` actions

instance Prelude.NFData Dataset where
  rnf Dataset' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lateDataRules
      `Prelude.seq` Prelude.rnf versioningConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf triggers
      `Prelude.seq` Prelude.rnf contentDeliveryRules
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf actions
