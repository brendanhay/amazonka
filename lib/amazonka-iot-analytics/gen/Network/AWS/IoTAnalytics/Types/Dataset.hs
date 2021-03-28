{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Dataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.Dataset
  ( Dataset (..)
  -- * Smart constructor
  , mkDataset
  -- * Lenses
  , dActions
  , dArn
  , dContentDeliveryRules
  , dCreationTime
  , dLastUpdateTime
  , dLateDataRules
  , dName
  , dRetentionPeriod
  , dStatus
  , dTriggers
  , dVersioningConfiguration
  ) where

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
  { actions :: Core.Maybe (Core.NonEmpty Types.DatasetAction)
    -- ^ The @DatasetAction@ objects that automatically create the data set contents.
  , arn :: Core.Maybe Types.DatasetArn
    -- ^ The ARN of the data set.
  , contentDeliveryRules :: Core.Maybe [Types.DatasetContentDeliveryRule]
    -- ^ When dataset contents are created they are delivered to destinations specified here.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the data set was created.
  , lastUpdateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time the data set was updated.
  , lateDataRules :: Core.Maybe (Core.NonEmpty Types.LateDataRule)
    -- ^ A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the data set.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ Optional. How long, in days, message data is kept for the data set.
  , status :: Core.Maybe Types.DatasetStatus
    -- ^ The status of the data set.
  , triggers :: Core.Maybe [Types.DatasetTrigger]
    -- ^ The @DatasetTrigger@ objects that specify when the data set is automatically updated.
  , versioningConfiguration :: Core.Maybe Types.VersioningConfiguration
    -- ^ Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Dataset' value with any optional fields omitted.
mkDataset
    :: Dataset
mkDataset
  = Dataset'{actions = Core.Nothing, arn = Core.Nothing,
             contentDeliveryRules = Core.Nothing, creationTime = Core.Nothing,
             lastUpdateTime = Core.Nothing, lateDataRules = Core.Nothing,
             name = Core.Nothing, retentionPeriod = Core.Nothing,
             status = Core.Nothing, triggers = Core.Nothing,
             versioningConfiguration = Core.Nothing}

-- | The @DatasetAction@ objects that automatically create the data set contents.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActions :: Lens.Lens' Dataset (Core.Maybe (Core.NonEmpty Types.DatasetAction))
dActions = Lens.field @"actions"
{-# INLINEABLE dActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | The ARN of the data set.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Dataset (Core.Maybe Types.DatasetArn)
dArn = Lens.field @"arn"
{-# INLINEABLE dArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | When dataset contents are created they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContentDeliveryRules :: Lens.Lens' Dataset (Core.Maybe [Types.DatasetContentDeliveryRule])
dContentDeliveryRules = Lens.field @"contentDeliveryRules"
{-# INLINEABLE dContentDeliveryRules #-}
{-# DEPRECATED contentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead"  #-}

-- | When the data set was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationTime :: Lens.Lens' Dataset (Core.Maybe Core.NominalDiffTime)
dCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The last time the data set was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastUpdateTime :: Lens.Lens' Dataset (Core.Maybe Core.NominalDiffTime)
dLastUpdateTime = Lens.field @"lastUpdateTime"
{-# INLINEABLE dLastUpdateTime #-}
{-# DEPRECATED lastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead"  #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLateDataRules :: Lens.Lens' Dataset (Core.Maybe (Core.NonEmpty Types.LateDataRule))
dLateDataRules = Lens.field @"lateDataRules"
{-# INLINEABLE dLateDataRules #-}
{-# DEPRECATED lateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead"  #-}

-- | The name of the data set.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Dataset (Core.Maybe Types.Name)
dName = Lens.field @"name"
{-# INLINEABLE dName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Optional. How long, in days, message data is kept for the data set.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetentionPeriod :: Lens.Lens' Dataset (Core.Maybe Types.RetentionPeriod)
dRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE dRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | The status of the data set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Dataset (Core.Maybe Types.DatasetStatus)
dStatus = Lens.field @"status"
{-# INLINEABLE dStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The @DatasetTrigger@ objects that specify when the data set is automatically updated.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTriggers :: Lens.Lens' Dataset (Core.Maybe [Types.DatasetTrigger])
dTriggers = Lens.field @"triggers"
{-# INLINEABLE dTriggers #-}
{-# DEPRECATED triggers "Use generic-lens or generic-optics with 'triggers' instead"  #-}

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersioningConfiguration :: Lens.Lens' Dataset (Core.Maybe Types.VersioningConfiguration)
dVersioningConfiguration = Lens.field @"versioningConfiguration"
{-# INLINEABLE dVersioningConfiguration #-}
{-# DEPRECATED versioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead"  #-}

instance Core.FromJSON Dataset where
        parseJSON
          = Core.withObject "Dataset" Core.$
              \ x ->
                Dataset' Core.<$>
                  (x Core..:? "actions") Core.<*> x Core..:? "arn" Core.<*>
                    x Core..:? "contentDeliveryRules"
                    Core.<*> x Core..:? "creationTime"
                    Core.<*> x Core..:? "lastUpdateTime"
                    Core.<*> x Core..:? "lateDataRules"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "retentionPeriod"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "triggers"
                    Core.<*> x Core..:? "versioningConfiguration"
