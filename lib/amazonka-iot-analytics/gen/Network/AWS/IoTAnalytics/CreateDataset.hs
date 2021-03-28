{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDataset (..)
    , mkCreateDataset
    -- ** Request lenses
    , cdfDatasetName
    , cdfActions
    , cdfContentDeliveryRules
    , cdfLateDataRules
    , cdfRetentionPeriod
    , cdfTags
    , cdfTriggers
    , cdfVersioningConfiguration

    -- * Destructuring the response
    , CreateDatasetResponse (..)
    , mkCreateDatasetResponse
    -- ** Response lenses
    , crsDatasetArn
    , crsDatasetName
    , crsRetentionPeriod
    , crsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { datasetName :: Types.DatasetName
    -- ^ The name of the data set.
  , actions :: Core.NonEmpty Types.DatasetAction
    -- ^ A list of actions that create the data set contents.
  , contentDeliveryRules :: Core.Maybe [Types.DatasetContentDeliveryRule]
    -- ^ When dataset contents are created, they are delivered to destinations specified here.
  , lateDataRules :: Core.Maybe (Core.NonEmpty Types.LateDataRule)
    -- ^ A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ Metadata which can be used to manage the data set.
  , triggers :: Core.Maybe [Types.DatasetTrigger]
    -- ^ A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
  , versioningConfiguration :: Core.Maybe Types.VersioningConfiguration
    -- ^ Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataset' value with any optional fields omitted.
mkCreateDataset
    :: Types.DatasetName -- ^ 'datasetName'
    -> Core.NonEmpty Types.DatasetAction -- ^ 'actions'
    -> CreateDataset
mkCreateDataset datasetName actions
  = CreateDataset'{datasetName, actions,
                   contentDeliveryRules = Core.Nothing, lateDataRules = Core.Nothing,
                   retentionPeriod = Core.Nothing, tags = Core.Nothing,
                   triggers = Core.Nothing, versioningConfiguration = Core.Nothing}

-- | The name of the data set.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfDatasetName :: Lens.Lens' CreateDataset Types.DatasetName
cdfDatasetName = Lens.field @"datasetName"
{-# INLINEABLE cdfDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | A list of actions that create the data set contents.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfActions :: Lens.Lens' CreateDataset (Core.NonEmpty Types.DatasetAction)
cdfActions = Lens.field @"actions"
{-# INLINEABLE cdfActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | When dataset contents are created, they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfContentDeliveryRules :: Lens.Lens' CreateDataset (Core.Maybe [Types.DatasetContentDeliveryRule])
cdfContentDeliveryRules = Lens.field @"contentDeliveryRules"
{-# INLINEABLE cdfContentDeliveryRules #-}
{-# DEPRECATED contentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead"  #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfLateDataRules :: Lens.Lens' CreateDataset (Core.Maybe (Core.NonEmpty Types.LateDataRule))
cdfLateDataRules = Lens.field @"lateDataRules"
{-# INLINEABLE cdfLateDataRules #-}
{-# DEPRECATED lateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead"  #-}

-- | Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfRetentionPeriod :: Lens.Lens' CreateDataset (Core.Maybe Types.RetentionPeriod)
cdfRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE cdfRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | Metadata which can be used to manage the data set.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfTags :: Lens.Lens' CreateDataset (Core.Maybe (Core.NonEmpty Types.Tag))
cdfTags = Lens.field @"tags"
{-# INLINEABLE cdfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfTriggers :: Lens.Lens' CreateDataset (Core.Maybe [Types.DatasetTrigger])
cdfTriggers = Lens.field @"triggers"
{-# INLINEABLE cdfTriggers #-}
{-# DEPRECATED triggers "Use generic-lens or generic-optics with 'triggers' instead"  #-}

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfVersioningConfiguration :: Lens.Lens' CreateDataset (Core.Maybe Types.VersioningConfiguration)
cdfVersioningConfiguration = Lens.field @"versioningConfiguration"
{-# INLINEABLE cdfVersioningConfiguration #-}
{-# DEPRECATED versioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead"  #-}

instance Core.ToQuery CreateDataset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDataset where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateDataset where
        toJSON CreateDataset{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("datasetName" Core..= datasetName),
                  Core.Just ("actions" Core..= actions),
                  ("contentDeliveryRules" Core..=) Core.<$> contentDeliveryRules,
                  ("lateDataRules" Core..=) Core.<$> lateDataRules,
                  ("retentionPeriod" Core..=) Core.<$> retentionPeriod,
                  ("tags" Core..=) Core.<$> tags,
                  ("triggers" Core..=) Core.<$> triggers,
                  ("versioningConfiguration" Core..=) Core.<$>
                    versioningConfiguration])

instance Core.AWSRequest CreateDataset where
        type Rs CreateDataset = CreateDatasetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/datasets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDatasetResponse' Core.<$>
                   (x Core..:? "datasetArn") Core.<*> x Core..:? "datasetName"
                     Core.<*> x Core..:? "retentionPeriod"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { datasetArn :: Core.Maybe Types.DatasetArn
    -- ^ The ARN of the dataset.
  , datasetName :: Core.Maybe Types.DatasetName
    -- ^ The name of the dataset.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, dataset contents are kept for the dataset.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatasetResponse' value with any optional fields omitted.
mkCreateDatasetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDatasetResponse
mkCreateDatasetResponse responseStatus
  = CreateDatasetResponse'{datasetArn = Core.Nothing,
                           datasetName = Core.Nothing, retentionPeriod = Core.Nothing,
                           responseStatus}

-- | The ARN of the dataset.
--
-- /Note:/ Consider using 'datasetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDatasetArn :: Lens.Lens' CreateDatasetResponse (Core.Maybe Types.DatasetArn)
crsDatasetArn = Lens.field @"datasetArn"
{-# INLINEABLE crsDatasetArn #-}
{-# DEPRECATED datasetArn "Use generic-lens or generic-optics with 'datasetArn' instead"  #-}

-- | The name of the dataset.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDatasetName :: Lens.Lens' CreateDatasetResponse (Core.Maybe Types.DatasetName)
crsDatasetName = Lens.field @"datasetName"
{-# INLINEABLE crsDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | How long, in days, dataset contents are kept for the dataset.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRetentionPeriod :: Lens.Lens' CreateDatasetResponse (Core.Maybe Types.RetentionPeriod)
crsRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE crsRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateDatasetResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
