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
-- Module      : Network.AWS.IoTAnalytics.UpdateDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a data set.
module Network.AWS.IoTAnalytics.UpdateDataset
  ( -- * Creating a Request
    UpdateDataset (..),
    newUpdateDataset,

    -- * Request Lenses
    updateDataset_triggers,
    updateDataset_retentionPeriod,
    updateDataset_lateDataRules,
    updateDataset_contentDeliveryRules,
    updateDataset_versioningConfiguration,
    updateDataset_datasetName,
    updateDataset_actions,

    -- * Destructuring the Response
    UpdateDatasetResponse (..),
    newUpdateDatasetResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { -- | A list of @DatasetTrigger@ objects. The list can be empty or can contain
    -- up to five @DatasetTrigger@ objects.
    triggers :: Core.Maybe [DatasetTrigger],
    -- | How long, in days, dataset contents are kept for the dataset.
    retentionPeriod :: Core.Maybe RetentionPeriod,
    -- | A list of data rules that send notifications to Amazon CloudWatch, when
    -- data arrives late. To specify @lateDataRules@, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    lateDataRules :: Core.Maybe (Core.NonEmpty LateDataRule),
    -- | When dataset contents are created, they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Core.Maybe [DatasetContentDeliveryRule],
    -- | Optional. How many versions of dataset contents are kept. If not
    -- specified or set to null, only the latest version plus the latest
    -- succeeded version (if they are different) are kept for the time period
    -- specified by the @retentionPeriod@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
    -- in the /AWS IoT Analytics User Guide/.
    versioningConfiguration :: Core.Maybe VersioningConfiguration,
    -- | The name of the data set to update.
    datasetName :: Core.Text,
    -- | A list of @DatasetAction@ objects.
    actions :: Core.NonEmpty DatasetAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggers', 'updateDataset_triggers' - A list of @DatasetTrigger@ objects. The list can be empty or can contain
-- up to five @DatasetTrigger@ objects.
--
-- 'retentionPeriod', 'updateDataset_retentionPeriod' - How long, in days, dataset contents are kept for the dataset.
--
-- 'lateDataRules', 'updateDataset_lateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when
-- data arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'contentDeliveryRules', 'updateDataset_contentDeliveryRules' - When dataset contents are created, they are delivered to destinations
-- specified here.
--
-- 'versioningConfiguration', 'updateDataset_versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
--
-- 'datasetName', 'updateDataset_datasetName' - The name of the data set to update.
--
-- 'actions', 'updateDataset_actions' - A list of @DatasetAction@ objects.
newUpdateDataset ::
  -- | 'datasetName'
  Core.Text ->
  -- | 'actions'
  Core.NonEmpty DatasetAction ->
  UpdateDataset
newUpdateDataset pDatasetName_ pActions_ =
  UpdateDataset'
    { triggers = Core.Nothing,
      retentionPeriod = Core.Nothing,
      lateDataRules = Core.Nothing,
      contentDeliveryRules = Core.Nothing,
      versioningConfiguration = Core.Nothing,
      datasetName = pDatasetName_,
      actions = Lens._Coerce Lens.# pActions_
    }

-- | A list of @DatasetTrigger@ objects. The list can be empty or can contain
-- up to five @DatasetTrigger@ objects.
updateDataset_triggers :: Lens.Lens' UpdateDataset (Core.Maybe [DatasetTrigger])
updateDataset_triggers = Lens.lens (\UpdateDataset' {triggers} -> triggers) (\s@UpdateDataset' {} a -> s {triggers = a} :: UpdateDataset) Core.. Lens.mapping Lens._Coerce

-- | How long, in days, dataset contents are kept for the dataset.
updateDataset_retentionPeriod :: Lens.Lens' UpdateDataset (Core.Maybe RetentionPeriod)
updateDataset_retentionPeriod = Lens.lens (\UpdateDataset' {retentionPeriod} -> retentionPeriod) (\s@UpdateDataset' {} a -> s {retentionPeriod = a} :: UpdateDataset)

-- | A list of data rules that send notifications to Amazon CloudWatch, when
-- data arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
updateDataset_lateDataRules :: Lens.Lens' UpdateDataset (Core.Maybe (Core.NonEmpty LateDataRule))
updateDataset_lateDataRules = Lens.lens (\UpdateDataset' {lateDataRules} -> lateDataRules) (\s@UpdateDataset' {} a -> s {lateDataRules = a} :: UpdateDataset) Core.. Lens.mapping Lens._Coerce

-- | When dataset contents are created, they are delivered to destinations
-- specified here.
updateDataset_contentDeliveryRules :: Lens.Lens' UpdateDataset (Core.Maybe [DatasetContentDeliveryRule])
updateDataset_contentDeliveryRules = Lens.lens (\UpdateDataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@UpdateDataset' {} a -> s {contentDeliveryRules = a} :: UpdateDataset) Core.. Lens.mapping Lens._Coerce

-- | Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
updateDataset_versioningConfiguration :: Lens.Lens' UpdateDataset (Core.Maybe VersioningConfiguration)
updateDataset_versioningConfiguration = Lens.lens (\UpdateDataset' {versioningConfiguration} -> versioningConfiguration) (\s@UpdateDataset' {} a -> s {versioningConfiguration = a} :: UpdateDataset)

-- | The name of the data set to update.
updateDataset_datasetName :: Lens.Lens' UpdateDataset Core.Text
updateDataset_datasetName = Lens.lens (\UpdateDataset' {datasetName} -> datasetName) (\s@UpdateDataset' {} a -> s {datasetName = a} :: UpdateDataset)

-- | A list of @DatasetAction@ objects.
updateDataset_actions :: Lens.Lens' UpdateDataset (Core.NonEmpty DatasetAction)
updateDataset_actions = Lens.lens (\UpdateDataset' {actions} -> actions) (\s@UpdateDataset' {} a -> s {actions = a} :: UpdateDataset) Core.. Lens._Coerce

instance Core.AWSRequest UpdateDataset where
  type
    AWSResponse UpdateDataset =
      UpdateDatasetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdateDatasetResponse'

instance Core.Hashable UpdateDataset

instance Core.NFData UpdateDataset

instance Core.ToHeaders UpdateDataset where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateDataset where
  toJSON UpdateDataset' {..} =
    Core.object
      ( Core.catMaybes
          [ ("triggers" Core..=) Core.<$> triggers,
            ("retentionPeriod" Core..=) Core.<$> retentionPeriod,
            ("lateDataRules" Core..=) Core.<$> lateDataRules,
            ("contentDeliveryRules" Core..=)
              Core.<$> contentDeliveryRules,
            ("versioningConfiguration" Core..=)
              Core.<$> versioningConfiguration,
            Core.Just ("actions" Core..= actions)
          ]
      )

instance Core.ToPath UpdateDataset where
  toPath UpdateDataset' {..} =
    Core.mconcat ["/datasets/", Core.toBS datasetName]

instance Core.ToQuery UpdateDataset where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDatasetResponse' smart constructor.
data UpdateDatasetResponse = UpdateDatasetResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDatasetResponse ::
  UpdateDatasetResponse
newUpdateDatasetResponse = UpdateDatasetResponse'

instance Core.NFData UpdateDatasetResponse
