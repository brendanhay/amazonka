{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dataset. A dataset stores data retrieved from a data store by applying a @queryAction@ (a SQL query) or a @containerAction@ (executing a containerized application). This operation creates the skeleton of a dataset. The dataset can be populated manually by calling @CreateDatasetContent@ or automatically according to a trigger you specify.
module Network.AWS.IoTAnalytics.CreateDataset
  ( -- * Creating a request
    CreateDataset (..),
    mkCreateDataset,

    -- ** Request lenses
    creVersioningConfiguration,
    creTriggers,
    creRetentionPeriod,
    creLateDataRules,
    creContentDeliveryRules,
    creTags,
    creDatasetName,
    creActions,

    -- * Destructuring the response
    CreateDatasetResponse (..),
    mkCreateDatasetResponse,

    -- ** Response lenses
    crsDatasetARN,
    crsRetentionPeriod,
    crsDatasetName,
    crsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { versioningConfiguration ::
      Lude.Maybe VersioningConfiguration,
    triggers :: Lude.Maybe [DatasetTrigger],
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    lateDataRules :: Lude.Maybe (Lude.NonEmpty LateDataRule),
    contentDeliveryRules :: Lude.Maybe [DatasetContentDeliveryRule],
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    datasetName :: Lude.Text,
    actions :: Lude.NonEmpty DatasetAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataset' with the minimum fields required to make a request.
--
-- * 'actions' - A list of actions that create the data set contents.
-- * 'contentDeliveryRules' - When dataset contents are created, they are delivered to destinations specified here.
-- * 'datasetName' - The name of the data set.
-- * 'lateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
-- * 'retentionPeriod' - Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
-- * 'tags' - Metadata which can be used to manage the data set.
-- * 'triggers' - A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
-- * 'versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
mkCreateDataset ::
  -- | 'datasetName'
  Lude.Text ->
  -- | 'actions'
  Lude.NonEmpty DatasetAction ->
  CreateDataset
mkCreateDataset pDatasetName_ pActions_ =
  CreateDataset'
    { versioningConfiguration = Lude.Nothing,
      triggers = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      lateDataRules = Lude.Nothing,
      contentDeliveryRules = Lude.Nothing,
      tags = Lude.Nothing,
      datasetName = pDatasetName_,
      actions = pActions_
    }

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creVersioningConfiguration :: Lens.Lens' CreateDataset (Lude.Maybe VersioningConfiguration)
creVersioningConfiguration = Lens.lens (versioningConfiguration :: CreateDataset -> Lude.Maybe VersioningConfiguration) (\s a -> s {versioningConfiguration = a} :: CreateDataset)
{-# DEPRECATED creVersioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead." #-}

-- | A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creTriggers :: Lens.Lens' CreateDataset (Lude.Maybe [DatasetTrigger])
creTriggers = Lens.lens (triggers :: CreateDataset -> Lude.Maybe [DatasetTrigger]) (\s a -> s {triggers = a} :: CreateDataset)
{-# DEPRECATED creTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creRetentionPeriod :: Lens.Lens' CreateDataset (Lude.Maybe RetentionPeriod)
creRetentionPeriod = Lens.lens (retentionPeriod :: CreateDataset -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: CreateDataset)
{-# DEPRECATED creRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creLateDataRules :: Lens.Lens' CreateDataset (Lude.Maybe (Lude.NonEmpty LateDataRule))
creLateDataRules = Lens.lens (lateDataRules :: CreateDataset -> Lude.Maybe (Lude.NonEmpty LateDataRule)) (\s a -> s {lateDataRules = a} :: CreateDataset)
{-# DEPRECATED creLateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead." #-}

-- | When dataset contents are created, they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creContentDeliveryRules :: Lens.Lens' CreateDataset (Lude.Maybe [DatasetContentDeliveryRule])
creContentDeliveryRules = Lens.lens (contentDeliveryRules :: CreateDataset -> Lude.Maybe [DatasetContentDeliveryRule]) (\s a -> s {contentDeliveryRules = a} :: CreateDataset)
{-# DEPRECATED creContentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead." #-}

-- | Metadata which can be used to manage the data set.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creTags :: Lens.Lens' CreateDataset (Lude.Maybe (Lude.NonEmpty Tag))
creTags = Lens.lens (tags :: CreateDataset -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateDataset)
{-# DEPRECATED creTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the data set.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creDatasetName :: Lens.Lens' CreateDataset Lude.Text
creDatasetName = Lens.lens (datasetName :: CreateDataset -> Lude.Text) (\s a -> s {datasetName = a} :: CreateDataset)
{-# DEPRECATED creDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | A list of actions that create the data set contents.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creActions :: Lens.Lens' CreateDataset (Lude.NonEmpty DatasetAction)
creActions = Lens.lens (actions :: CreateDataset -> Lude.NonEmpty DatasetAction) (\s a -> s {actions = a} :: CreateDataset)
{-# DEPRECATED creActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Lude.AWSRequest CreateDataset where
  type Rs CreateDataset = CreateDatasetResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            Lude.<$> (x Lude..?> "datasetArn")
            Lude.<*> (x Lude..?> "retentionPeriod")
            Lude.<*> (x Lude..?> "datasetName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDataset where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateDataset where
  toJSON CreateDataset' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("versioningConfiguration" Lude..=)
              Lude.<$> versioningConfiguration,
            ("triggers" Lude..=) Lude.<$> triggers,
            ("retentionPeriod" Lude..=) Lude.<$> retentionPeriod,
            ("lateDataRules" Lude..=) Lude.<$> lateDataRules,
            ("contentDeliveryRules" Lude..=) Lude.<$> contentDeliveryRules,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("datasetName" Lude..= datasetName),
            Lude.Just ("actions" Lude..= actions)
          ]
      )

instance Lude.ToPath CreateDataset where
  toPath = Lude.const "/datasets"

instance Lude.ToQuery CreateDataset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { datasetARN ::
      Lude.Maybe Lude.Text,
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    datasetName :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDatasetResponse' with the minimum fields required to make a request.
--
-- * 'datasetARN' - The ARN of the dataset.
-- * 'datasetName' - The name of the dataset.
-- * 'responseStatus' - The response status code.
-- * 'retentionPeriod' - How long, in days, dataset contents are kept for the dataset.
mkCreateDatasetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDatasetResponse
mkCreateDatasetResponse pResponseStatus_ =
  CreateDatasetResponse'
    { datasetARN = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      datasetName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the dataset.
--
-- /Note:/ Consider using 'datasetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDatasetARN :: Lens.Lens' CreateDatasetResponse (Lude.Maybe Lude.Text)
crsDatasetARN = Lens.lens (datasetARN :: CreateDatasetResponse -> Lude.Maybe Lude.Text) (\s a -> s {datasetARN = a} :: CreateDatasetResponse)
{-# DEPRECATED crsDatasetARN "Use generic-lens or generic-optics with 'datasetARN' instead." #-}

-- | How long, in days, dataset contents are kept for the dataset.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRetentionPeriod :: Lens.Lens' CreateDatasetResponse (Lude.Maybe RetentionPeriod)
crsRetentionPeriod = Lens.lens (retentionPeriod :: CreateDatasetResponse -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: CreateDatasetResponse)
{-# DEPRECATED crsRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the dataset.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDatasetName :: Lens.Lens' CreateDatasetResponse (Lude.Maybe Lude.Text)
crsDatasetName = Lens.lens (datasetName :: CreateDatasetResponse -> Lude.Maybe Lude.Text) (\s a -> s {datasetName = a} :: CreateDatasetResponse)
{-# DEPRECATED crsDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateDatasetResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateDatasetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDatasetResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
