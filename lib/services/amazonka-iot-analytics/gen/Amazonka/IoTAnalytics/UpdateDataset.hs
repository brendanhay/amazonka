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
-- Module      : Amazonka.IoTAnalytics.UpdateDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a dataset.
module Amazonka.IoTAnalytics.UpdateDataset
  ( -- * Creating a Request
    UpdateDataset (..),
    newUpdateDataset,

    -- * Request Lenses
    updateDataset_versioningConfiguration,
    updateDataset_triggers,
    updateDataset_retentionPeriod,
    updateDataset_lateDataRules,
    updateDataset_contentDeliveryRules,
    updateDataset_datasetName,
    updateDataset_actions,

    -- * Destructuring the Response
    UpdateDatasetResponse (..),
    newUpdateDatasetResponse,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { -- | Optional. How many versions of dataset contents are kept. If not
    -- specified or set to null, only the latest version plus the latest
    -- succeeded version (if they are different) are kept for the time period
    -- specified by the @retentionPeriod@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
    -- in the /IoT Analytics User Guide/.
    versioningConfiguration :: Prelude.Maybe VersioningConfiguration,
    -- | A list of @DatasetTrigger@ objects. The list can be empty or can contain
    -- up to five @DatasetTrigger@ objects.
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | How long, in days, dataset contents are kept for the dataset.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | A list of data rules that send notifications to CloudWatch, when data
    -- arrives late. To specify @lateDataRules@, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    lateDataRules :: Prelude.Maybe (Prelude.NonEmpty LateDataRule),
    -- | When dataset contents are created, they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Prelude.Maybe [DatasetContentDeliveryRule],
    -- | The name of the dataset to update.
    datasetName :: Prelude.Text,
    -- | A list of @DatasetAction@ objects.
    actions :: Prelude.NonEmpty DatasetAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versioningConfiguration', 'updateDataset_versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
-- in the /IoT Analytics User Guide/.
--
-- 'triggers', 'updateDataset_triggers' - A list of @DatasetTrigger@ objects. The list can be empty or can contain
-- up to five @DatasetTrigger@ objects.
--
-- 'retentionPeriod', 'updateDataset_retentionPeriod' - How long, in days, dataset contents are kept for the dataset.
--
-- 'lateDataRules', 'updateDataset_lateDataRules' - A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'contentDeliveryRules', 'updateDataset_contentDeliveryRules' - When dataset contents are created, they are delivered to destinations
-- specified here.
--
-- 'datasetName', 'updateDataset_datasetName' - The name of the dataset to update.
--
-- 'actions', 'updateDataset_actions' - A list of @DatasetAction@ objects.
newUpdateDataset ::
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'actions'
  Prelude.NonEmpty DatasetAction ->
  UpdateDataset
newUpdateDataset pDatasetName_ pActions_ =
  UpdateDataset'
    { versioningConfiguration =
        Prelude.Nothing,
      triggers = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      lateDataRules = Prelude.Nothing,
      contentDeliveryRules = Prelude.Nothing,
      datasetName = pDatasetName_,
      actions = Lens.coerced Lens.# pActions_
    }

-- | Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of IoT Analytics datasets>
-- in the /IoT Analytics User Guide/.
updateDataset_versioningConfiguration :: Lens.Lens' UpdateDataset (Prelude.Maybe VersioningConfiguration)
updateDataset_versioningConfiguration = Lens.lens (\UpdateDataset' {versioningConfiguration} -> versioningConfiguration) (\s@UpdateDataset' {} a -> s {versioningConfiguration = a} :: UpdateDataset)

-- | A list of @DatasetTrigger@ objects. The list can be empty or can contain
-- up to five @DatasetTrigger@ objects.
updateDataset_triggers :: Lens.Lens' UpdateDataset (Prelude.Maybe [DatasetTrigger])
updateDataset_triggers = Lens.lens (\UpdateDataset' {triggers} -> triggers) (\s@UpdateDataset' {} a -> s {triggers = a} :: UpdateDataset) Prelude.. Lens.mapping Lens.coerced

-- | How long, in days, dataset contents are kept for the dataset.
updateDataset_retentionPeriod :: Lens.Lens' UpdateDataset (Prelude.Maybe RetentionPeriod)
updateDataset_retentionPeriod = Lens.lens (\UpdateDataset' {retentionPeriod} -> retentionPeriod) (\s@UpdateDataset' {} a -> s {retentionPeriod = a} :: UpdateDataset)

-- | A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
updateDataset_lateDataRules :: Lens.Lens' UpdateDataset (Prelude.Maybe (Prelude.NonEmpty LateDataRule))
updateDataset_lateDataRules = Lens.lens (\UpdateDataset' {lateDataRules} -> lateDataRules) (\s@UpdateDataset' {} a -> s {lateDataRules = a} :: UpdateDataset) Prelude.. Lens.mapping Lens.coerced

-- | When dataset contents are created, they are delivered to destinations
-- specified here.
updateDataset_contentDeliveryRules :: Lens.Lens' UpdateDataset (Prelude.Maybe [DatasetContentDeliveryRule])
updateDataset_contentDeliveryRules = Lens.lens (\UpdateDataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@UpdateDataset' {} a -> s {contentDeliveryRules = a} :: UpdateDataset) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset to update.
updateDataset_datasetName :: Lens.Lens' UpdateDataset Prelude.Text
updateDataset_datasetName = Lens.lens (\UpdateDataset' {datasetName} -> datasetName) (\s@UpdateDataset' {} a -> s {datasetName = a} :: UpdateDataset)

-- | A list of @DatasetAction@ objects.
updateDataset_actions :: Lens.Lens' UpdateDataset (Prelude.NonEmpty DatasetAction)
updateDataset_actions = Lens.lens (\UpdateDataset' {actions} -> actions) (\s@UpdateDataset' {} a -> s {actions = a} :: UpdateDataset) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDataset where
  type
    AWSResponse UpdateDataset =
      UpdateDatasetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdateDatasetResponse'

instance Prelude.Hashable UpdateDataset

instance Prelude.NFData UpdateDataset

instance Core.ToHeaders UpdateDataset where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateDataset where
  toJSON UpdateDataset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("versioningConfiguration" Core..=)
              Prelude.<$> versioningConfiguration,
            ("triggers" Core..=) Prelude.<$> triggers,
            ("retentionPeriod" Core..=)
              Prelude.<$> retentionPeriod,
            ("lateDataRules" Core..=) Prelude.<$> lateDataRules,
            ("contentDeliveryRules" Core..=)
              Prelude.<$> contentDeliveryRules,
            Prelude.Just ("actions" Core..= actions)
          ]
      )

instance Core.ToPath UpdateDataset where
  toPath UpdateDataset' {..} =
    Prelude.mconcat
      ["/datasets/", Core.toBS datasetName]

instance Core.ToQuery UpdateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatasetResponse' smart constructor.
data UpdateDatasetResponse = UpdateDatasetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDatasetResponse ::
  UpdateDatasetResponse
newUpdateDatasetResponse = UpdateDatasetResponse'

instance Prelude.NFData UpdateDatasetResponse
