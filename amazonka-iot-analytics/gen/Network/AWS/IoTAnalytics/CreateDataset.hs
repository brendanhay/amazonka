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
-- Module      : Network.AWS.IoTAnalytics.CreateDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dataset. A dataset stores data retrieved from a data store by
-- applying a @queryAction@ (a SQL query) or a @containerAction@ (executing
-- a containerized application). This operation creates the skeleton of a
-- dataset. The dataset can be populated manually by calling
-- @CreateDatasetContent@ or automatically according to a trigger you
-- specify.
module Network.AWS.IoTAnalytics.CreateDataset
  ( -- * Creating a Request
    CreateDataset (..),
    newCreateDataset,

    -- * Request Lenses
    createDataset_triggers,
    createDataset_retentionPeriod,
    createDataset_lateDataRules,
    createDataset_tags,
    createDataset_contentDeliveryRules,
    createDataset_versioningConfiguration,
    createDataset_datasetName,
    createDataset_actions,

    -- * Destructuring the Response
    CreateDatasetResponse (..),
    newCreateDatasetResponse,

    -- * Response Lenses
    createDatasetResponse_datasetArn,
    createDatasetResponse_retentionPeriod,
    createDatasetResponse_datasetName,
    createDatasetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | A list of triggers. A trigger causes data set contents to be populated
    -- at a specified time interval or when another data set\'s contents are
    -- created. The list of triggers can be empty or contain up to five
    -- @DataSetTrigger@ objects.
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | Optional. How long, in days, versions of dataset contents are kept for
    -- the dataset. If not specified or set to @null@, versions of dataset
    -- contents are retained for at most 90 days. The number of versions of
    -- dataset contents retained is determined by the @versioningConfiguration@
    -- parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
    -- in the /AWS IoT Analytics User Guide/.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | A list of data rules that send notifications to Amazon CloudWatch, when
    -- data arrives late. To specify @lateDataRules@, the dataset must use a
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
    -- filter.
    lateDataRules :: Prelude.Maybe (Prelude.NonEmpty LateDataRule),
    -- | Metadata which can be used to manage the data set.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | When dataset contents are created, they are delivered to destinations
    -- specified here.
    contentDeliveryRules :: Prelude.Maybe [DatasetContentDeliveryRule],
    -- | Optional. How many versions of dataset contents are kept. If not
    -- specified or set to null, only the latest version plus the latest
    -- succeeded version (if they are different) are kept for the time period
    -- specified by the @retentionPeriod@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
    -- in the /AWS IoT Analytics User Guide/.
    versioningConfiguration :: Prelude.Maybe VersioningConfiguration,
    -- | The name of the data set.
    datasetName :: Prelude.Text,
    -- | A list of actions that create the data set contents.
    actions :: Prelude.NonEmpty DatasetAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggers', 'createDataset_triggers' - A list of triggers. A trigger causes data set contents to be populated
-- at a specified time interval or when another data set\'s contents are
-- created. The list of triggers can be empty or contain up to five
-- @DataSetTrigger@ objects.
--
-- 'retentionPeriod', 'createDataset_retentionPeriod' - Optional. How long, in days, versions of dataset contents are kept for
-- the dataset. If not specified or set to @null@, versions of dataset
-- contents are retained for at most 90 days. The number of versions of
-- dataset contents retained is determined by the @versioningConfiguration@
-- parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
--
-- 'lateDataRules', 'createDataset_lateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when
-- data arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
--
-- 'tags', 'createDataset_tags' - Metadata which can be used to manage the data set.
--
-- 'contentDeliveryRules', 'createDataset_contentDeliveryRules' - When dataset contents are created, they are delivered to destinations
-- specified here.
--
-- 'versioningConfiguration', 'createDataset_versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
--
-- 'datasetName', 'createDataset_datasetName' - The name of the data set.
--
-- 'actions', 'createDataset_actions' - A list of actions that create the data set contents.
newCreateDataset ::
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'actions'
  Prelude.NonEmpty DatasetAction ->
  CreateDataset
newCreateDataset pDatasetName_ pActions_ =
  CreateDataset'
    { triggers = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      lateDataRules = Prelude.Nothing,
      tags = Prelude.Nothing,
      contentDeliveryRules = Prelude.Nothing,
      versioningConfiguration = Prelude.Nothing,
      datasetName = pDatasetName_,
      actions = Lens._Coerce Lens.# pActions_
    }

-- | A list of triggers. A trigger causes data set contents to be populated
-- at a specified time interval or when another data set\'s contents are
-- created. The list of triggers can be empty or contain up to five
-- @DataSetTrigger@ objects.
createDataset_triggers :: Lens.Lens' CreateDataset (Prelude.Maybe [DatasetTrigger])
createDataset_triggers = Lens.lens (\CreateDataset' {triggers} -> triggers) (\s@CreateDataset' {} a -> s {triggers = a} :: CreateDataset) Prelude.. Lens.mapping Lens._Coerce

-- | Optional. How long, in days, versions of dataset contents are kept for
-- the dataset. If not specified or set to @null@, versions of dataset
-- contents are retained for at most 90 days. The number of versions of
-- dataset contents retained is determined by the @versioningConfiguration@
-- parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
createDataset_retentionPeriod :: Lens.Lens' CreateDataset (Prelude.Maybe RetentionPeriod)
createDataset_retentionPeriod = Lens.lens (\CreateDataset' {retentionPeriod} -> retentionPeriod) (\s@CreateDataset' {} a -> s {retentionPeriod = a} :: CreateDataset)

-- | A list of data rules that send notifications to Amazon CloudWatch, when
-- data arrives late. To specify @lateDataRules@, the dataset must use a
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer>
-- filter.
createDataset_lateDataRules :: Lens.Lens' CreateDataset (Prelude.Maybe (Prelude.NonEmpty LateDataRule))
createDataset_lateDataRules = Lens.lens (\CreateDataset' {lateDataRules} -> lateDataRules) (\s@CreateDataset' {} a -> s {lateDataRules = a} :: CreateDataset) Prelude.. Lens.mapping Lens._Coerce

-- | Metadata which can be used to manage the data set.
createDataset_tags :: Lens.Lens' CreateDataset (Prelude.Maybe (Prelude.NonEmpty Tag))
createDataset_tags = Lens.lens (\CreateDataset' {tags} -> tags) (\s@CreateDataset' {} a -> s {tags = a} :: CreateDataset) Prelude.. Lens.mapping Lens._Coerce

-- | When dataset contents are created, they are delivered to destinations
-- specified here.
createDataset_contentDeliveryRules :: Lens.Lens' CreateDataset (Prelude.Maybe [DatasetContentDeliveryRule])
createDataset_contentDeliveryRules = Lens.lens (\CreateDataset' {contentDeliveryRules} -> contentDeliveryRules) (\s@CreateDataset' {} a -> s {contentDeliveryRules = a} :: CreateDataset) Prelude.. Lens.mapping Lens._Coerce

-- | Optional. How many versions of dataset contents are kept. If not
-- specified or set to null, only the latest version plus the latest
-- succeeded version (if they are different) are kept for the time period
-- specified by the @retentionPeriod@ parameter. For more information, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets>
-- in the /AWS IoT Analytics User Guide/.
createDataset_versioningConfiguration :: Lens.Lens' CreateDataset (Prelude.Maybe VersioningConfiguration)
createDataset_versioningConfiguration = Lens.lens (\CreateDataset' {versioningConfiguration} -> versioningConfiguration) (\s@CreateDataset' {} a -> s {versioningConfiguration = a} :: CreateDataset)

-- | The name of the data set.
createDataset_datasetName :: Lens.Lens' CreateDataset Prelude.Text
createDataset_datasetName = Lens.lens (\CreateDataset' {datasetName} -> datasetName) (\s@CreateDataset' {} a -> s {datasetName = a} :: CreateDataset)

-- | A list of actions that create the data set contents.
createDataset_actions :: Lens.Lens' CreateDataset (Prelude.NonEmpty DatasetAction)
createDataset_actions = Lens.lens (\CreateDataset' {actions} -> actions) (\s@CreateDataset' {} a -> s {actions = a} :: CreateDataset) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateDataset where
  type
    AWSResponse CreateDataset =
      CreateDatasetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            Prelude.<$> (x Core..?> "datasetArn")
            Prelude.<*> (x Core..?> "retentionPeriod")
            Prelude.<*> (x Core..?> "datasetName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataset

instance Prelude.NFData CreateDataset

instance Core.ToHeaders CreateDataset where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("triggers" Core..=) Prelude.<$> triggers,
            ("retentionPeriod" Core..=)
              Prelude.<$> retentionPeriod,
            ("lateDataRules" Core..=) Prelude.<$> lateDataRules,
            ("tags" Core..=) Prelude.<$> tags,
            ("contentDeliveryRules" Core..=)
              Prelude.<$> contentDeliveryRules,
            ("versioningConfiguration" Core..=)
              Prelude.<$> versioningConfiguration,
            Prelude.Just ("datasetName" Core..= datasetName),
            Prelude.Just ("actions" Core..= actions)
          ]
      )

instance Core.ToPath CreateDataset where
  toPath = Prelude.const "/datasets"

instance Core.ToQuery CreateDataset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | The ARN of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | How long, in days, dataset contents are kept for the dataset.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'createDatasetResponse_datasetArn' - The ARN of the dataset.
--
-- 'retentionPeriod', 'createDatasetResponse_retentionPeriod' - How long, in days, dataset contents are kept for the dataset.
--
-- 'datasetName', 'createDatasetResponse_datasetName' - The name of the dataset.
--
-- 'httpStatus', 'createDatasetResponse_httpStatus' - The response's http status code.
newCreateDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetResponse
newCreateDatasetResponse pHttpStatus_ =
  CreateDatasetResponse'
    { datasetArn =
        Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the dataset.
createDatasetResponse_datasetArn :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe Prelude.Text)
createDatasetResponse_datasetArn = Lens.lens (\CreateDatasetResponse' {datasetArn} -> datasetArn) (\s@CreateDatasetResponse' {} a -> s {datasetArn = a} :: CreateDatasetResponse)

-- | How long, in days, dataset contents are kept for the dataset.
createDatasetResponse_retentionPeriod :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe RetentionPeriod)
createDatasetResponse_retentionPeriod = Lens.lens (\CreateDatasetResponse' {retentionPeriod} -> retentionPeriod) (\s@CreateDatasetResponse' {} a -> s {retentionPeriod = a} :: CreateDatasetResponse)

-- | The name of the dataset.
createDatasetResponse_datasetName :: Lens.Lens' CreateDatasetResponse (Prelude.Maybe Prelude.Text)
createDatasetResponse_datasetName = Lens.lens (\CreateDatasetResponse' {datasetName} -> datasetName) (\s@CreateDatasetResponse' {} a -> s {datasetName = a} :: CreateDatasetResponse)

-- | The response's http status code.
createDatasetResponse_httpStatus :: Lens.Lens' CreateDatasetResponse Prelude.Int
createDatasetResponse_httpStatus = Lens.lens (\CreateDatasetResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetResponse' {} a -> s {httpStatus = a} :: CreateDatasetResponse)

instance Prelude.NFData CreateDatasetResponse
