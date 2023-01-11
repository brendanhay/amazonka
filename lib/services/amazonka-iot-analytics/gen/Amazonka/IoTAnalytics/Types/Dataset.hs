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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Dataset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The @DatasetAction@ objects that automatically create the dataset
    -- contents.
    actions :: Prelude.Maybe (Prelude.NonEmpty DatasetAction),
    -- | The ARN of the dataset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When dataset contents are created they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Prelude.Maybe [DatasetContentDeliveryRule],
    -- | When the dataset was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the dataset was updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | A list of data rules that send notifications to CloudWatch, when data
    -- arrives late. To specify @lateDataRules@, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    lateDataRules :: Prelude.Maybe (Prelude.NonEmpty LateDataRule),
    -- | The name of the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | Optional. How long, in days, message data is kept for the dataset.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The status of the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The @DatasetTrigger@ objects that specify when the dataset is
    -- automatically updated.
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | Optional. How many versions of dataset contents are kept. If not
    -- specified or set to null, only the latest version plus the latest
    -- succeeded version (if they are different) are kept for the time period
    -- specified by the @retentionPeriod@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
    -- in the /IoT Analytics User Guide/.
    versioningConfiguration :: Prelude.Maybe VersioningConfiguration
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
-- 'actions', 'dataset_actions' - The @DatasetAction@ objects that automatically create the dataset
-- contents.
--
-- 'arn', 'dataset_arn' - The ARN of the dataset.
--
-- 'contentDeliveryRules', 'dataset_contentDeliveryRules' - When dataset contents are created they are delivered to destinations
-- specified here.
--
-- 'creationTime', 'dataset_creationTime' - When the dataset was created.
--
-- 'lastUpdateTime', 'dataset_lastUpdateTime' - The last time the dataset was updated.
--
-- 'lateDataRules', 'dataset_lateDataRules' - A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'name', 'dataset_name' - The name of the dataset.
--
-- 'retentionPeriod', 'dataset_retentionPeriod' - Optional. How long, in days, message data is kept for the dataset.
--
-- 'status', 'dataset_status' - The status of the dataset.
--
-- 'triggers', 'dataset_triggers' - The @DatasetTrigger@ objects that specify when the dataset is
-- automatically updated.
--
-- 'versioningConfiguration', 'dataset_versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
-- in the /IoT Analytics User Guide/.
newDataset ::
  Dataset
newDataset =
  Dataset'
    { actions = Prelude.Nothing,
      arn = Prelude.Nothing,
      contentDeliveryRules = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      lateDataRules = Prelude.Nothing,
      name = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      status = Prelude.Nothing,
      triggers = Prelude.Nothing,
      versioningConfiguration = Prelude.Nothing
    }

-- | The @DatasetAction@ objects that automatically create the dataset
-- contents.
dataset_actions :: Lens.Lens' Dataset (Prelude.Maybe (Prelude.NonEmpty DatasetAction))
dataset_actions = Lens.lens (\Dataset' {actions} -> actions) (\s@Dataset' {} a -> s {actions = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the dataset.
dataset_arn :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_arn = Lens.lens (\Dataset' {arn} -> arn) (\s@Dataset' {} a -> s {arn = a} :: Dataset)

-- | When dataset contents are created they are delivered to destinations
-- specified here.
dataset_contentDeliveryRules :: Lens.Lens' Dataset (Prelude.Maybe [DatasetContentDeliveryRule])
dataset_contentDeliveryRules = Lens.lens (\Dataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@Dataset' {} a -> s {contentDeliveryRules = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | When the dataset was created.
dataset_creationTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_creationTime = Lens.lens (\Dataset' {creationTime} -> creationTime) (\s@Dataset' {} a -> s {creationTime = a} :: Dataset) Prelude.. Lens.mapping Data._Time

-- | The last time the dataset was updated.
dataset_lastUpdateTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_lastUpdateTime = Lens.lens (\Dataset' {lastUpdateTime} -> lastUpdateTime) (\s@Dataset' {} a -> s {lastUpdateTime = a} :: Dataset) Prelude.. Lens.mapping Data._Time

-- | A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
dataset_lateDataRules :: Lens.Lens' Dataset (Prelude.Maybe (Prelude.NonEmpty LateDataRule))
dataset_lateDataRules = Lens.lens (\Dataset' {lateDataRules} -> lateDataRules) (\s@Dataset' {} a -> s {lateDataRules = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset.
dataset_name :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_name = Lens.lens (\Dataset' {name} -> name) (\s@Dataset' {} a -> s {name = a} :: Dataset)

-- | Optional. How long, in days, message data is kept for the dataset.
dataset_retentionPeriod :: Lens.Lens' Dataset (Prelude.Maybe RetentionPeriod)
dataset_retentionPeriod = Lens.lens (\Dataset' {retentionPeriod} -> retentionPeriod) (\s@Dataset' {} a -> s {retentionPeriod = a} :: Dataset)

-- | The status of the dataset.
dataset_status :: Lens.Lens' Dataset (Prelude.Maybe DatasetStatus)
dataset_status = Lens.lens (\Dataset' {status} -> status) (\s@Dataset' {} a -> s {status = a} :: Dataset)

-- | The @DatasetTrigger@ objects that specify when the dataset is
-- automatically updated.
dataset_triggers :: Lens.Lens' Dataset (Prelude.Maybe [DatasetTrigger])
dataset_triggers = Lens.lens (\Dataset' {triggers} -> triggers) (\s@Dataset' {} a -> s {triggers = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
-- in the /IoT Analytics User Guide/.
dataset_versioningConfiguration :: Lens.Lens' Dataset (Prelude.Maybe VersioningConfiguration)
dataset_versioningConfiguration = Lens.lens (\Dataset' {versioningConfiguration} -> versioningConfiguration) (\s@Dataset' {} a -> s {versioningConfiguration = a} :: Dataset)

instance Data.FromJSON Dataset where
  parseJSON =
    Data.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Prelude.<$> (x Data..:? "actions")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> ( x Data..:? "contentDeliveryRules"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "lateDataRules")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "retentionPeriod")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "triggers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "versioningConfiguration")
      )

instance Prelude.Hashable Dataset where
  hashWithSalt _salt Dataset' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` contentDeliveryRules
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` lateDataRules
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` triggers
      `Prelude.hashWithSalt` versioningConfiguration

instance Prelude.NFData Dataset where
  rnf Dataset' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf contentDeliveryRules
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf lateDataRules
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf triggers
      `Prelude.seq` Prelude.rnf versioningConfiguration
