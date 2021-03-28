{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateDataset (..)
    , mkUpdateDataset
    -- ** Request lenses
    , udDatasetName
    , udActions
    , udContentDeliveryRules
    , udLateDataRules
    , udRetentionPeriod
    , udTriggers
    , udVersioningConfiguration

    -- * Destructuring the response
    , UpdateDatasetResponse (..)
    , mkUpdateDatasetResponse
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDataset' smart constructor.
data UpdateDataset = UpdateDataset'
  { datasetName :: Types.DatasetName
    -- ^ The name of the data set to update.
  , actions :: Core.NonEmpty Types.DatasetAction
    -- ^ A list of @DatasetAction@ objects.
  , contentDeliveryRules :: Core.Maybe [Types.DatasetContentDeliveryRule]
    -- ^ When dataset contents are created, they are delivered to destinations specified here.
  , lateDataRules :: Core.Maybe (Core.NonEmpty Types.LateDataRule)
    -- ^ A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, dataset contents are kept for the dataset.
  , triggers :: Core.Maybe [Types.DatasetTrigger]
    -- ^ A list of @DatasetTrigger@ objects. The list can be empty or can contain up to five @DatasetTrigger@ objects.
  , versioningConfiguration :: Core.Maybe Types.VersioningConfiguration
    -- ^ Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataset' value with any optional fields omitted.
mkUpdateDataset
    :: Types.DatasetName -- ^ 'datasetName'
    -> Core.NonEmpty Types.DatasetAction -- ^ 'actions'
    -> UpdateDataset
mkUpdateDataset datasetName actions
  = UpdateDataset'{datasetName, actions,
                   contentDeliveryRules = Core.Nothing, lateDataRules = Core.Nothing,
                   retentionPeriod = Core.Nothing, triggers = Core.Nothing,
                   versioningConfiguration = Core.Nothing}

-- | The name of the data set to update.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDatasetName :: Lens.Lens' UpdateDataset Types.DatasetName
udDatasetName = Lens.field @"datasetName"
{-# INLINEABLE udDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | A list of @DatasetAction@ objects.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udActions :: Lens.Lens' UpdateDataset (Core.NonEmpty Types.DatasetAction)
udActions = Lens.field @"actions"
{-# INLINEABLE udActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | When dataset contents are created, they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udContentDeliveryRules :: Lens.Lens' UpdateDataset (Core.Maybe [Types.DatasetContentDeliveryRule])
udContentDeliveryRules = Lens.field @"contentDeliveryRules"
{-# INLINEABLE udContentDeliveryRules #-}
{-# DEPRECATED contentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead"  #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udLateDataRules :: Lens.Lens' UpdateDataset (Core.Maybe (Core.NonEmpty Types.LateDataRule))
udLateDataRules = Lens.field @"lateDataRules"
{-# INLINEABLE udLateDataRules #-}
{-# DEPRECATED lateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead"  #-}

-- | How long, in days, dataset contents are kept for the dataset.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udRetentionPeriod :: Lens.Lens' UpdateDataset (Core.Maybe Types.RetentionPeriod)
udRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE udRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | A list of @DatasetTrigger@ objects. The list can be empty or can contain up to five @DatasetTrigger@ objects.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udTriggers :: Lens.Lens' UpdateDataset (Core.Maybe [Types.DatasetTrigger])
udTriggers = Lens.field @"triggers"
{-# INLINEABLE udTriggers #-}
{-# DEPRECATED triggers "Use generic-lens or generic-optics with 'triggers' instead"  #-}

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udVersioningConfiguration :: Lens.Lens' UpdateDataset (Core.Maybe Types.VersioningConfiguration)
udVersioningConfiguration = Lens.field @"versioningConfiguration"
{-# INLINEABLE udVersioningConfiguration #-}
{-# DEPRECATED versioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead"  #-}

instance Core.ToQuery UpdateDataset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDataset where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateDataset where
        toJSON UpdateDataset{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("actions" Core..= actions),
                  ("contentDeliveryRules" Core..=) Core.<$> contentDeliveryRules,
                  ("lateDataRules" Core..=) Core.<$> lateDataRules,
                  ("retentionPeriod" Core..=) Core.<$> retentionPeriod,
                  ("triggers" Core..=) Core.<$> triggers,
                  ("versioningConfiguration" Core..=) Core.<$>
                    versioningConfiguration])

instance Core.AWSRequest UpdateDataset where
        type Rs UpdateDataset = UpdateDatasetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/datasets/" Core.<> Core.toText datasetName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateDatasetResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDatasetResponse' smart constructor.
data UpdateDatasetResponse = UpdateDatasetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDatasetResponse' value with any optional fields omitted.
mkUpdateDatasetResponse
    :: UpdateDatasetResponse
mkUpdateDatasetResponse = UpdateDatasetResponse'
