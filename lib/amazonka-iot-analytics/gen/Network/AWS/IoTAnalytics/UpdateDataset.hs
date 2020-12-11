{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.UpdateDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a data set.
module Network.AWS.IoTAnalytics.UpdateDataset
  ( -- * Creating a request
    UpdateDataset (..),
    mkUpdateDataset,

    -- ** Request lenses
    udVersioningConfiguration,
    udTriggers,
    udRetentionPeriod,
    udLateDataRules,
    udContentDeliveryRules,
    udDatasetName,
    udActions,

    -- * Destructuring the response
    UpdateDatasetResponse (..),
    mkUpdateDatasetResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { versioningConfiguration ::
      Lude.Maybe VersioningConfiguration,
    triggers :: Lude.Maybe [DatasetTrigger],
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    lateDataRules :: Lude.Maybe (Lude.NonEmpty LateDataRule),
    contentDeliveryRules :: Lude.Maybe [DatasetContentDeliveryRule],
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

-- | Creates a value of 'UpdateDataset' with the minimum fields required to make a request.
--
-- * 'actions' - A list of @DatasetAction@ objects.
-- * 'contentDeliveryRules' - When dataset contents are created, they are delivered to destinations specified here.
-- * 'datasetName' - The name of the data set to update.
-- * 'lateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
-- * 'retentionPeriod' - How long, in days, dataset contents are kept for the dataset.
-- * 'triggers' - A list of @DatasetTrigger@ objects. The list can be empty or can contain up to five @DatasetTrigger@ objects.
-- * 'versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
mkUpdateDataset ::
  -- | 'datasetName'
  Lude.Text ->
  -- | 'actions'
  Lude.NonEmpty DatasetAction ->
  UpdateDataset
mkUpdateDataset pDatasetName_ pActions_ =
  UpdateDataset'
    { versioningConfiguration = Lude.Nothing,
      triggers = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      lateDataRules = Lude.Nothing,
      contentDeliveryRules = Lude.Nothing,
      datasetName = pDatasetName_,
      actions = pActions_
    }

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udVersioningConfiguration :: Lens.Lens' UpdateDataset (Lude.Maybe VersioningConfiguration)
udVersioningConfiguration = Lens.lens (versioningConfiguration :: UpdateDataset -> Lude.Maybe VersioningConfiguration) (\s a -> s {versioningConfiguration = a} :: UpdateDataset)
{-# DEPRECATED udVersioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead." #-}

-- | A list of @DatasetTrigger@ objects. The list can be empty or can contain up to five @DatasetTrigger@ objects.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udTriggers :: Lens.Lens' UpdateDataset (Lude.Maybe [DatasetTrigger])
udTriggers = Lens.lens (triggers :: UpdateDataset -> Lude.Maybe [DatasetTrigger]) (\s a -> s {triggers = a} :: UpdateDataset)
{-# DEPRECATED udTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | How long, in days, dataset contents are kept for the dataset.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udRetentionPeriod :: Lens.Lens' UpdateDataset (Lude.Maybe RetentionPeriod)
udRetentionPeriod = Lens.lens (retentionPeriod :: UpdateDataset -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: UpdateDataset)
{-# DEPRECATED udRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udLateDataRules :: Lens.Lens' UpdateDataset (Lude.Maybe (Lude.NonEmpty LateDataRule))
udLateDataRules = Lens.lens (lateDataRules :: UpdateDataset -> Lude.Maybe (Lude.NonEmpty LateDataRule)) (\s a -> s {lateDataRules = a} :: UpdateDataset)
{-# DEPRECATED udLateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead." #-}

-- | When dataset contents are created, they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udContentDeliveryRules :: Lens.Lens' UpdateDataset (Lude.Maybe [DatasetContentDeliveryRule])
udContentDeliveryRules = Lens.lens (contentDeliveryRules :: UpdateDataset -> Lude.Maybe [DatasetContentDeliveryRule]) (\s a -> s {contentDeliveryRules = a} :: UpdateDataset)
{-# DEPRECATED udContentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead." #-}

-- | The name of the data set to update.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDatasetName :: Lens.Lens' UpdateDataset Lude.Text
udDatasetName = Lens.lens (datasetName :: UpdateDataset -> Lude.Text) (\s a -> s {datasetName = a} :: UpdateDataset)
{-# DEPRECATED udDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | A list of @DatasetAction@ objects.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udActions :: Lens.Lens' UpdateDataset (Lude.NonEmpty DatasetAction)
udActions = Lens.lens (actions :: UpdateDataset -> Lude.NonEmpty DatasetAction) (\s a -> s {actions = a} :: UpdateDataset)
{-# DEPRECATED udActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Lude.AWSRequest UpdateDataset where
  type Rs UpdateDataset = UpdateDatasetResponse
  request = Req.putJSON ioTAnalyticsService
  response = Res.receiveNull UpdateDatasetResponse'

instance Lude.ToHeaders UpdateDataset where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateDataset where
  toJSON UpdateDataset' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("versioningConfiguration" Lude..=)
              Lude.<$> versioningConfiguration,
            ("triggers" Lude..=) Lude.<$> triggers,
            ("retentionPeriod" Lude..=) Lude.<$> retentionPeriod,
            ("lateDataRules" Lude..=) Lude.<$> lateDataRules,
            ("contentDeliveryRules" Lude..=) Lude.<$> contentDeliveryRules,
            Lude.Just ("actions" Lude..= actions)
          ]
      )

instance Lude.ToPath UpdateDataset where
  toPath UpdateDataset' {..} =
    Lude.mconcat ["/datasets/", Lude.toBS datasetName]

instance Lude.ToQuery UpdateDataset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDatasetResponse' smart constructor.
data UpdateDatasetResponse = UpdateDatasetResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDatasetResponse' with the minimum fields required to make a request.
mkUpdateDatasetResponse ::
  UpdateDatasetResponse
mkUpdateDatasetResponse = UpdateDatasetResponse'
