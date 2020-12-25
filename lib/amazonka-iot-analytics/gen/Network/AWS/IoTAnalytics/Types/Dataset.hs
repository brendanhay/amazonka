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
    dActions,
    dArn,
    dContentDeliveryRules,
    dCreationTime,
    dLastUpdateTime,
    dLateDataRules,
    dName,
    dRetentionPeriod,
    dStatus,
    dTriggers,
    dVersioningConfiguration,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.DatasetAction as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetArn as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetStatus as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetTrigger as Types
import qualified Network.AWS.IoTAnalytics.Types.LateDataRule as Types
import qualified Network.AWS.IoTAnalytics.Types.Name as Types
import qualified Network.AWS.IoTAnalytics.Types.RetentionPeriod as Types
import qualified Network.AWS.IoTAnalytics.Types.VersioningConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a data set.
--
-- /See:/ 'mkDataset' smart constructor.
data Dataset = Dataset'
  { -- | The @DatasetAction@ objects that automatically create the data set contents.
    actions :: Core.Maybe (Core.NonEmpty Types.DatasetAction),
    -- | The ARN of the data set.
    arn :: Core.Maybe Types.DatasetArn,
    -- | When dataset contents are created they are delivered to destinations specified here.
    contentDeliveryRules :: Core.Maybe [Types.DatasetContentDeliveryRule],
    -- | When the data set was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last time the data set was updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
    lateDataRules :: Core.Maybe (Core.NonEmpty Types.LateDataRule),
    -- | The name of the data set.
    name :: Core.Maybe Types.Name,
    -- | Optional. How long, in days, message data is kept for the data set.
    retentionPeriod :: Core.Maybe Types.RetentionPeriod,
    -- | The status of the data set.
    status :: Core.Maybe Types.DatasetStatus,
    -- | The @DatasetTrigger@ objects that specify when the data set is automatically updated.
    triggers :: Core.Maybe [Types.DatasetTrigger],
    -- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
    versioningConfiguration :: Core.Maybe Types.VersioningConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Dataset' value with any optional fields omitted.
mkDataset ::
  Dataset
mkDataset =
  Dataset'
    { actions = Core.Nothing,
      arn = Core.Nothing,
      contentDeliveryRules = Core.Nothing,
      creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      lateDataRules = Core.Nothing,
      name = Core.Nothing,
      retentionPeriod = Core.Nothing,
      status = Core.Nothing,
      triggers = Core.Nothing,
      versioningConfiguration = Core.Nothing
    }

-- | The @DatasetAction@ objects that automatically create the data set contents.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActions :: Lens.Lens' Dataset (Core.Maybe (Core.NonEmpty Types.DatasetAction))
dActions = Lens.field @"actions"
{-# DEPRECATED dActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The ARN of the data set.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Dataset (Core.Maybe Types.DatasetArn)
dArn = Lens.field @"arn"
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When dataset contents are created they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContentDeliveryRules :: Lens.Lens' Dataset (Core.Maybe [Types.DatasetContentDeliveryRule])
dContentDeliveryRules = Lens.field @"contentDeliveryRules"
{-# DEPRECATED dContentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead." #-}

-- | When the data set was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationTime :: Lens.Lens' Dataset (Core.Maybe Core.NominalDiffTime)
dCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The last time the data set was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastUpdateTime :: Lens.Lens' Dataset (Core.Maybe Core.NominalDiffTime)
dLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED dLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLateDataRules :: Lens.Lens' Dataset (Core.Maybe (Core.NonEmpty Types.LateDataRule))
dLateDataRules = Lens.field @"lateDataRules"
{-# DEPRECATED dLateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead." #-}

-- | The name of the data set.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Dataset (Core.Maybe Types.Name)
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional. How long, in days, message data is kept for the data set.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetentionPeriod :: Lens.Lens' Dataset (Core.Maybe Types.RetentionPeriod)
dRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED dRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The status of the data set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Dataset (Core.Maybe Types.DatasetStatus)
dStatus = Lens.field @"status"
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @DatasetTrigger@ objects that specify when the data set is automatically updated.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTriggers :: Lens.Lens' Dataset (Core.Maybe [Types.DatasetTrigger])
dTriggers = Lens.field @"triggers"
{-# DEPRECATED dTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersioningConfiguration :: Lens.Lens' Dataset (Core.Maybe Types.VersioningConfiguration)
dVersioningConfiguration = Lens.field @"versioningConfiguration"
{-# DEPRECATED dVersioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead." #-}

instance Core.FromJSON Dataset where
  parseJSON =
    Core.withObject "Dataset" Core.$
      \x ->
        Dataset'
          Core.<$> (x Core..:? "actions")
          Core.<*> (x Core..:? "arn")
          Core.<*> (x Core..:? "contentDeliveryRules")
          Core.<*> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "lateDataRules")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "retentionPeriod")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "triggers")
          Core.<*> (x Core..:? "versioningConfiguration")
