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
-- Module      : Network.AWS.IoTAnalytics.Types.Dataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Dataset where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.DatasetAction
import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import Network.AWS.IoTAnalytics.Types.LateDataRule
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import Network.AWS.IoTAnalytics.Types.VersioningConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a dataset.
--
-- /See:/ 'newDataset' smart constructor.
data Dataset = Dataset'
  { -- | When the dataset was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | Optional. How many versions of dataset contents are kept. If not
    -- specified or set to null, only the latest version plus the latest
    -- succeeded version (if they are different) are kept for the time period
    -- specified by the @retentionPeriod@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
    -- in the /IoT Analytics User Guide/.
    versioningConfiguration :: Prelude.Maybe VersioningConfiguration,
    -- | The ARN of the dataset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The @DatasetAction@ objects that automatically create the dataset
    -- contents.
    actions :: Prelude.Maybe (Prelude.NonEmpty DatasetAction),
    -- | The @DatasetTrigger@ objects that specify when the dataset is
    -- automatically updated.
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | Optional. How long, in days, message data is kept for the dataset.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | A list of data rules that send notifications to CloudWatch, when data
    -- arrives late. To specify @lateDataRules@, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    lateDataRules :: Prelude.Maybe (Prelude.NonEmpty LateDataRule),
    -- | The name of the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | When dataset contents are created they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Prelude.Maybe [DatasetContentDeliveryRule],
    -- | The last time the dataset was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX
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
-- 'creationTime', 'dataset_creationTime' - When the dataset was created.
--
-- 'status', 'dataset_status' - The status of the dataset.
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
-- 'actions', 'dataset_actions' - The @DatasetAction@ objects that automatically create the dataset
-- contents.
--
-- 'triggers', 'dataset_triggers' - The @DatasetTrigger@ objects that specify when the dataset is
-- automatically updated.
--
-- 'retentionPeriod', 'dataset_retentionPeriod' - Optional. How long, in days, message data is kept for the dataset.
--
-- 'lateDataRules', 'dataset_lateDataRules' - A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'name', 'dataset_name' - The name of the dataset.
--
-- 'contentDeliveryRules', 'dataset_contentDeliveryRules' - When dataset contents are created they are delivered to destinations
-- specified here.
--
-- 'lastUpdateTime', 'dataset_lastUpdateTime' - The last time the dataset was updated.
newDataset ::
  Dataset
newDataset =
  Dataset'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      versioningConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      actions = Prelude.Nothing,
      triggers = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      lateDataRules = Prelude.Nothing,
      name = Prelude.Nothing,
      contentDeliveryRules = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | When the dataset was created.
dataset_creationTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_creationTime = Lens.lens (\Dataset' {creationTime} -> creationTime) (\s@Dataset' {} a -> s {creationTime = a} :: Dataset) Prelude.. Lens.mapping Core._Time

-- | The status of the dataset.
dataset_status :: Lens.Lens' Dataset (Prelude.Maybe DatasetStatus)
dataset_status = Lens.lens (\Dataset' {status} -> status) (\s@Dataset' {} a -> s {status = a} :: Dataset)

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

-- | The @DatasetAction@ objects that automatically create the dataset
-- contents.
dataset_actions :: Lens.Lens' Dataset (Prelude.Maybe (Prelude.NonEmpty DatasetAction))
dataset_actions = Lens.lens (\Dataset' {actions} -> actions) (\s@Dataset' {} a -> s {actions = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | The @DatasetTrigger@ objects that specify when the dataset is
-- automatically updated.
dataset_triggers :: Lens.Lens' Dataset (Prelude.Maybe [DatasetTrigger])
dataset_triggers = Lens.lens (\Dataset' {triggers} -> triggers) (\s@Dataset' {} a -> s {triggers = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | Optional. How long, in days, message data is kept for the dataset.
dataset_retentionPeriod :: Lens.Lens' Dataset (Prelude.Maybe RetentionPeriod)
dataset_retentionPeriod = Lens.lens (\Dataset' {retentionPeriod} -> retentionPeriod) (\s@Dataset' {} a -> s {retentionPeriod = a} :: Dataset)

-- | A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
dataset_lateDataRules :: Lens.Lens' Dataset (Prelude.Maybe (Prelude.NonEmpty LateDataRule))
dataset_lateDataRules = Lens.lens (\Dataset' {lateDataRules} -> lateDataRules) (\s@Dataset' {} a -> s {lateDataRules = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset.
dataset_name :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_name = Lens.lens (\Dataset' {name} -> name) (\s@Dataset' {} a -> s {name = a} :: Dataset)

-- | When dataset contents are created they are delivered to destinations
-- specified here.
dataset_contentDeliveryRules :: Lens.Lens' Dataset (Prelude.Maybe [DatasetContentDeliveryRule])
dataset_contentDeliveryRules = Lens.lens (\Dataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@Dataset' {} a -> s {contentDeliveryRules = a} :: Dataset) Prelude.. Lens.mapping Lens.coerced

-- | The last time the dataset was updated.
dataset_lastUpdateTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_lastUpdateTime = Lens.lens (\Dataset' {lastUpdateTime} -> lastUpdateTime) (\s@Dataset' {} a -> s {lastUpdateTime = a} :: Dataset) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Dataset where
  parseJSON =
    Core.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "versioningConfiguration")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "actions")
            Prelude.<*> (x Core..:? "triggers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "retentionPeriod")
            Prelude.<*> (x Core..:? "lateDataRules")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> ( x Core..:? "contentDeliveryRules"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lastUpdateTime")
      )

instance Prelude.Hashable Dataset

instance Prelude.NFData Dataset
