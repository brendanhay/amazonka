{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Dataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Dataset
  ( Dataset (..),

    -- * Smart constructor
    mkDataset,

    -- * Lenses
    dCreationTime,
    dStatus,
    dVersioningConfiguration,
    dArn,
    dActions,
    dTriggers,
    dRetentionPeriod,
    dLateDataRules,
    dName,
    dContentDeliveryRules,
    dLastUpdateTime,
  )
where

import Network.AWS.IoTAnalytics.Types.DatasetAction
import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import Network.AWS.IoTAnalytics.Types.LateDataRule
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import Network.AWS.IoTAnalytics.Types.VersioningConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a data set.
--
-- /See:/ 'mkDataset' smart constructor.
data Dataset = Dataset'
  { creationTime :: Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe DatasetStatus,
    versioningConfiguration :: Lude.Maybe VersioningConfiguration,
    arn :: Lude.Maybe Lude.Text,
    actions :: Lude.Maybe (Lude.NonEmpty DatasetAction),
    triggers :: Lude.Maybe [DatasetTrigger],
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    lateDataRules :: Lude.Maybe (Lude.NonEmpty LateDataRule),
    name :: Lude.Maybe Lude.Text,
    contentDeliveryRules :: Lude.Maybe [DatasetContentDeliveryRule],
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Dataset' with the minimum fields required to make a request.
--
-- * 'actions' - The @DatasetAction@ objects that automatically create the data set contents.
-- * 'arn' - The ARN of the data set.
-- * 'contentDeliveryRules' - When dataset contents are created they are delivered to destinations specified here.
-- * 'creationTime' - When the data set was created.
-- * 'lastUpdateTime' - The last time the data set was updated.
-- * 'lateDataRules' - A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
-- * 'name' - The name of the data set.
-- * 'retentionPeriod' - Optional. How long, in days, message data is kept for the data set.
-- * 'status' - The status of the data set.
-- * 'triggers' - The @DatasetTrigger@ objects that specify when the data set is automatically updated.
-- * 'versioningConfiguration' - Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
mkDataset ::
  Dataset
mkDataset =
  Dataset'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      versioningConfiguration = Lude.Nothing,
      arn = Lude.Nothing,
      actions = Lude.Nothing,
      triggers = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      lateDataRules = Lude.Nothing,
      name = Lude.Nothing,
      contentDeliveryRules = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | When the data set was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationTime :: Lens.Lens' Dataset (Lude.Maybe Lude.Timestamp)
dCreationTime = Lens.lens (creationTime :: Dataset -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Dataset)
{-# DEPRECATED dCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the data set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Dataset (Lude.Maybe DatasetStatus)
dStatus = Lens.lens (status :: Dataset -> Lude.Maybe DatasetStatus) (\s a -> s {status = a} :: Dataset)
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersioningConfiguration :: Lens.Lens' Dataset (Lude.Maybe VersioningConfiguration)
dVersioningConfiguration = Lens.lens (versioningConfiguration :: Dataset -> Lude.Maybe VersioningConfiguration) (\s a -> s {versioningConfiguration = a} :: Dataset)
{-# DEPRECATED dVersioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead." #-}

-- | The ARN of the data set.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Dataset (Lude.Maybe Lude.Text)
dArn = Lens.lens (arn :: Dataset -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Dataset)
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The @DatasetAction@ objects that automatically create the data set contents.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActions :: Lens.Lens' Dataset (Lude.Maybe (Lude.NonEmpty DatasetAction))
dActions = Lens.lens (actions :: Dataset -> Lude.Maybe (Lude.NonEmpty DatasetAction)) (\s a -> s {actions = a} :: Dataset)
{-# DEPRECATED dActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The @DatasetTrigger@ objects that specify when the data set is automatically updated.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTriggers :: Lens.Lens' Dataset (Lude.Maybe [DatasetTrigger])
dTriggers = Lens.lens (triggers :: Dataset -> Lude.Maybe [DatasetTrigger]) (\s a -> s {triggers = a} :: Dataset)
{-# DEPRECATED dTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | Optional. How long, in days, message data is kept for the data set.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetentionPeriod :: Lens.Lens' Dataset (Lude.Maybe RetentionPeriod)
dRetentionPeriod = Lens.lens (retentionPeriod :: Dataset -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: Dataset)
{-# DEPRECATED dRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLateDataRules :: Lens.Lens' Dataset (Lude.Maybe (Lude.NonEmpty LateDataRule))
dLateDataRules = Lens.lens (lateDataRules :: Dataset -> Lude.Maybe (Lude.NonEmpty LateDataRule)) (\s a -> s {lateDataRules = a} :: Dataset)
{-# DEPRECATED dLateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead." #-}

-- | The name of the data set.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Dataset (Lude.Maybe Lude.Text)
dName = Lens.lens (name :: Dataset -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Dataset)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When dataset contents are created they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContentDeliveryRules :: Lens.Lens' Dataset (Lude.Maybe [DatasetContentDeliveryRule])
dContentDeliveryRules = Lens.lens (contentDeliveryRules :: Dataset -> Lude.Maybe [DatasetContentDeliveryRule]) (\s a -> s {contentDeliveryRules = a} :: Dataset)
{-# DEPRECATED dContentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead." #-}

-- | The last time the data set was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastUpdateTime :: Lens.Lens' Dataset (Lude.Maybe Lude.Timestamp)
dLastUpdateTime = Lens.lens (lastUpdateTime :: Dataset -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: Dataset)
{-# DEPRECATED dLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON Dataset where
  parseJSON =
    Lude.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "versioningConfiguration")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "actions")
            Lude.<*> (x Lude..:? "triggers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "retentionPeriod")
            Lude.<*> (x Lude..:? "lateDataRules")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "contentDeliveryRules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
