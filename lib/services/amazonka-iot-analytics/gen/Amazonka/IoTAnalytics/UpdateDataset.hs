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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateDataset_lateDataRules,
    updateDataset_versioningConfiguration,
    updateDataset_triggers,
    updateDataset_contentDeliveryRules,
    updateDataset_retentionPeriod,
    updateDataset_datasetName,
    updateDataset_actions,

    -- * Destructuring the Response
    UpdateDatasetResponse (..),
    newUpdateDatasetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { -- | A list of data rules that send notifications to CloudWatch, when data
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
    -- | A list of @DatasetTrigger@ objects. The list can be empty or can contain
    -- up to five @DatasetTrigger@ objects.
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | When dataset contents are created, they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Prelude.Maybe [DatasetContentDeliveryRule],
    -- | How long, in days, dataset contents are kept for the dataset.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
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
-- 'lateDataRules', 'updateDataset_lateDataRules' - A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
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
-- 'contentDeliveryRules', 'updateDataset_contentDeliveryRules' - When dataset contents are created, they are delivered to destinations
-- specified here.
--
-- 'retentionPeriod', 'updateDataset_retentionPeriod' - How long, in days, dataset contents are kept for the dataset.
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
    { lateDataRules = Prelude.Nothing,
      versioningConfiguration = Prelude.Nothing,
      triggers = Prelude.Nothing,
      contentDeliveryRules = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      datasetName = pDatasetName_,
      actions = Lens.coerced Lens.# pActions_
    }

-- | A list of data rules that send notifications to CloudWatch, when data
-- arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
updateDataset_lateDataRules :: Lens.Lens' UpdateDataset (Prelude.Maybe (Prelude.NonEmpty LateDataRule))
updateDataset_lateDataRules = Lens.lens (\UpdateDataset' {lateDataRules} -> lateDataRules) (\s@UpdateDataset' {} a -> s {lateDataRules = a} :: UpdateDataset) Prelude.. Lens.mapping Lens.coerced

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

-- | When dataset contents are created, they are delivered to destinations
-- specified here.
updateDataset_contentDeliveryRules :: Lens.Lens' UpdateDataset (Prelude.Maybe [DatasetContentDeliveryRule])
updateDataset_contentDeliveryRules = Lens.lens (\UpdateDataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@UpdateDataset' {} a -> s {contentDeliveryRules = a} :: UpdateDataset) Prelude.. Lens.mapping Lens.coerced

-- | How long, in days, dataset contents are kept for the dataset.
updateDataset_retentionPeriod :: Lens.Lens' UpdateDataset (Prelude.Maybe RetentionPeriod)
updateDataset_retentionPeriod = Lens.lens (\UpdateDataset' {retentionPeriod} -> retentionPeriod) (\s@UpdateDataset' {} a -> s {retentionPeriod = a} :: UpdateDataset)

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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateDatasetResponse'

instance Prelude.Hashable UpdateDataset where
  hashWithSalt _salt UpdateDataset' {..} =
    _salt `Prelude.hashWithSalt` lateDataRules
      `Prelude.hashWithSalt` versioningConfiguration
      `Prelude.hashWithSalt` triggers
      `Prelude.hashWithSalt` contentDeliveryRules
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` actions

instance Prelude.NFData UpdateDataset where
  rnf UpdateDataset' {..} =
    Prelude.rnf lateDataRules
      `Prelude.seq` Prelude.rnf versioningConfiguration
      `Prelude.seq` Prelude.rnf triggers
      `Prelude.seq` Prelude.rnf contentDeliveryRules
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf actions

instance Data.ToHeaders UpdateDataset where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateDataset where
  toJSON UpdateDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lateDataRules" Data..=) Prelude.<$> lateDataRules,
            ("versioningConfiguration" Data..=)
              Prelude.<$> versioningConfiguration,
            ("triggers" Data..=) Prelude.<$> triggers,
            ("contentDeliveryRules" Data..=)
              Prelude.<$> contentDeliveryRules,
            ("retentionPeriod" Data..=)
              Prelude.<$> retentionPeriod,
            Prelude.Just ("actions" Data..= actions)
          ]
      )

instance Data.ToPath UpdateDataset where
  toPath UpdateDataset' {..} =
    Prelude.mconcat
      ["/datasets/", Data.toBS datasetName]

instance Data.ToQuery UpdateDataset where
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

instance Prelude.NFData UpdateDatasetResponse where
  rnf _ = ()
