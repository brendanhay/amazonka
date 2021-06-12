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

-- | Information about a data set.
--
-- /See:/ 'newDataset' smart constructor.
data Dataset = Dataset'
  { -- | The status of the data set.
    status :: Core.Maybe DatasetStatus,
    -- | When the data set was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The last time the data set was updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The @DatasetTrigger@ objects that specify when the data set is
    -- automatically updated.
    triggers :: Core.Maybe [DatasetTrigger],
    -- | The @DatasetAction@ objects that automatically create the data set
    -- contents.
    actions :: Core.Maybe (Core.NonEmpty DatasetAction),
    -- | The ARN of the data set.
    arn :: Core.Maybe Core.Text,
    -- | The name of the data set.
    name :: Core.Maybe Core.Text,
    -- | Optional. How long, in days, message data is kept for the data set.
    retentionPeriod :: Core.Maybe RetentionPeriod,
    -- | A list of data rules that send notifications to Amazon CloudWatch, when
    -- data arrives late. To specify @lateDataRules@, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    lateDataRules :: Core.Maybe (Core.NonEmpty LateDataRule),
    -- | When dataset contents are created they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Core.Maybe [DatasetContentDeliveryRule],
    -- | Optional. How many versions of dataset contents are kept. If not
    -- specified or set to null, only the latest version plus the latest
    -- succeeded version (if they are different) are kept for the time period
    -- specified by the @retentionPeriod@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
    -- in the /AWS IoT Analytics User Guide/.
    versioningConfiguration :: Core.Maybe VersioningConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Dataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dataset_status' - The status of the data set.
--
-- 'creationTime', 'dataset_creationTime' - When the data set was created.
--
-- 'lastUpdateTime', 'dataset_lastUpdateTime' - The last time the data set was updated.
--
-- 'triggers', 'dataset_triggers' - The @DatasetTrigger@ objects that specify when the data set is
-- automatically updated.
--
-- 'actions', 'dataset_actions' - The @DatasetAction@ objects that automatically create the data set
-- contents.
--
-- 'arn', 'dataset_arn' - The ARN of the data set.
--
-- 'name', 'dataset_name' - The name of the data set.
--
-- 'retentionPeriod', 'dataset_retentionPeriod' - Optional. How long, in days, message data is kept for the data set.
--
-- 'lateDataRules', 'dataset_lateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when
-- data arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'contentDeliveryRules', 'dataset_contentDeliveryRules' - When dataset contents are created they are delivered to destinations
-- specified here.
--
-- 'versioningConfiguration', 'dataset_versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
newDataset ::
  Dataset
newDataset =
  Dataset'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      triggers = Core.Nothing,
      actions = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      retentionPeriod = Core.Nothing,
      lateDataRules = Core.Nothing,
      contentDeliveryRules = Core.Nothing,
      versioningConfiguration = Core.Nothing
    }

-- | The status of the data set.
dataset_status :: Lens.Lens' Dataset (Core.Maybe DatasetStatus)
dataset_status = Lens.lens (\Dataset' {status} -> status) (\s@Dataset' {} a -> s {status = a} :: Dataset)

-- | When the data set was created.
dataset_creationTime :: Lens.Lens' Dataset (Core.Maybe Core.UTCTime)
dataset_creationTime = Lens.lens (\Dataset' {creationTime} -> creationTime) (\s@Dataset' {} a -> s {creationTime = a} :: Dataset) Core.. Lens.mapping Core._Time

-- | The last time the data set was updated.
dataset_lastUpdateTime :: Lens.Lens' Dataset (Core.Maybe Core.UTCTime)
dataset_lastUpdateTime = Lens.lens (\Dataset' {lastUpdateTime} -> lastUpdateTime) (\s@Dataset' {} a -> s {lastUpdateTime = a} :: Dataset) Core.. Lens.mapping Core._Time

-- | The @DatasetTrigger@ objects that specify when the data set is
-- automatically updated.
dataset_triggers :: Lens.Lens' Dataset (Core.Maybe [DatasetTrigger])
dataset_triggers = Lens.lens (\Dataset' {triggers} -> triggers) (\s@Dataset' {} a -> s {triggers = a} :: Dataset) Core.. Lens.mapping Lens._Coerce

-- | The @DatasetAction@ objects that automatically create the data set
-- contents.
dataset_actions :: Lens.Lens' Dataset (Core.Maybe (Core.NonEmpty DatasetAction))
dataset_actions = Lens.lens (\Dataset' {actions} -> actions) (\s@Dataset' {} a -> s {actions = a} :: Dataset) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the data set.
dataset_arn :: Lens.Lens' Dataset (Core.Maybe Core.Text)
dataset_arn = Lens.lens (\Dataset' {arn} -> arn) (\s@Dataset' {} a -> s {arn = a} :: Dataset)

-- | The name of the data set.
dataset_name :: Lens.Lens' Dataset (Core.Maybe Core.Text)
dataset_name = Lens.lens (\Dataset' {name} -> name) (\s@Dataset' {} a -> s {name = a} :: Dataset)

-- | Optional. How long, in days, message data is kept for the data set.
dataset_retentionPeriod :: Lens.Lens' Dataset (Core.Maybe RetentionPeriod)
dataset_retentionPeriod = Lens.lens (\Dataset' {retentionPeriod} -> retentionPeriod) (\s@Dataset' {} a -> s {retentionPeriod = a} :: Dataset)

-- | A list of data rules that send notifications to Amazon CloudWatch, when
-- data arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
dataset_lateDataRules :: Lens.Lens' Dataset (Core.Maybe (Core.NonEmpty LateDataRule))
dataset_lateDataRules = Lens.lens (\Dataset' {lateDataRules} -> lateDataRules) (\s@Dataset' {} a -> s {lateDataRules = a} :: Dataset) Core.. Lens.mapping Lens._Coerce

-- | When dataset contents are created they are delivered to destinations
-- specified here.
dataset_contentDeliveryRules :: Lens.Lens' Dataset (Core.Maybe [DatasetContentDeliveryRule])
dataset_contentDeliveryRules = Lens.lens (\Dataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@Dataset' {} a -> s {contentDeliveryRules = a} :: Dataset) Core.. Lens.mapping Lens._Coerce

-- | Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
dataset_versioningConfiguration :: Lens.Lens' Dataset (Core.Maybe VersioningConfiguration)
dataset_versioningConfiguration = Lens.lens (\Dataset' {versioningConfiguration} -> versioningConfiguration) (\s@Dataset' {} a -> s {versioningConfiguration = a} :: Dataset)

instance Core.FromJSON Dataset where
  parseJSON =
    Core.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "lastUpdateTime")
            Core.<*> (x Core..:? "triggers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "actions")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "retentionPeriod")
            Core.<*> (x Core..:? "lateDataRules")
            Core.<*> ( x Core..:? "contentDeliveryRules"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "versioningConfiguration")
      )

instance Core.Hashable Dataset

instance Core.NFData Dataset
