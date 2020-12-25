{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cdfDatasetName,
    cdfActions,
    cdfContentDeliveryRules,
    cdfLateDataRules,
    cdfRetentionPeriod,
    cdfTags,
    cdfTriggers,
    cdfVersioningConfiguration,

    -- * Destructuring the response
    CreateDatasetResponse (..),
    mkCreateDatasetResponse,

    -- ** Response lenses
    crsDatasetArn,
    crsDatasetName,
    crsRetentionPeriod,
    crsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDataset' smart constructor.
data CreateDataset = CreateDataset'
  { -- | The name of the data set.
    datasetName :: Types.DatasetName,
    -- | A list of actions that create the data set contents.
    actions :: Core.NonEmpty Types.DatasetAction,
    -- | When dataset contents are created, they are delivered to destinations specified here.
    contentDeliveryRules :: Core.Maybe [Types.DatasetContentDeliveryRule],
    -- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
    lateDataRules :: Core.Maybe (Core.NonEmpty Types.LateDataRule),
    -- | Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
    retentionPeriod :: Core.Maybe Types.RetentionPeriod,
    -- | Metadata which can be used to manage the data set.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag),
    -- | A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
    triggers :: Core.Maybe [Types.DatasetTrigger],
    -- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
    versioningConfiguration :: Core.Maybe Types.VersioningConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataset' value with any optional fields omitted.
mkCreateDataset ::
  -- | 'datasetName'
  Types.DatasetName ->
  -- | 'actions'
  Core.NonEmpty Types.DatasetAction ->
  CreateDataset
mkCreateDataset datasetName actions =
  CreateDataset'
    { datasetName,
      actions,
      contentDeliveryRules = Core.Nothing,
      lateDataRules = Core.Nothing,
      retentionPeriod = Core.Nothing,
      tags = Core.Nothing,
      triggers = Core.Nothing,
      versioningConfiguration = Core.Nothing
    }

-- | The name of the data set.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfDatasetName :: Lens.Lens' CreateDataset Types.DatasetName
cdfDatasetName = Lens.field @"datasetName"
{-# DEPRECATED cdfDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | A list of actions that create the data set contents.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfActions :: Lens.Lens' CreateDataset (Core.NonEmpty Types.DatasetAction)
cdfActions = Lens.field @"actions"
{-# DEPRECATED cdfActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | When dataset contents are created, they are delivered to destinations specified here.
--
-- /Note:/ Consider using 'contentDeliveryRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfContentDeliveryRules :: Lens.Lens' CreateDataset (Core.Maybe [Types.DatasetContentDeliveryRule])
cdfContentDeliveryRules = Lens.field @"contentDeliveryRules"
{-# DEPRECATED cdfContentDeliveryRules "Use generic-lens or generic-optics with 'contentDeliveryRules' instead." #-}

-- | A list of data rules that send notifications to Amazon CloudWatch, when data arrives late. To specify @lateDataRules@ , the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'lateDataRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfLateDataRules :: Lens.Lens' CreateDataset (Core.Maybe (Core.NonEmpty Types.LateDataRule))
cdfLateDataRules = Lens.field @"lateDataRules"
{-# DEPRECATED cdfLateDataRules "Use generic-lens or generic-optics with 'lateDataRules' instead." #-}

-- | Optional. How long, in days, versions of dataset contents are kept for the dataset. If not specified or set to @null@ , versions of dataset contents are retained for at most 90 days. The number of versions of dataset contents retained is determined by the @versioningConfiguration@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfRetentionPeriod :: Lens.Lens' CreateDataset (Core.Maybe Types.RetentionPeriod)
cdfRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED cdfRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | Metadata which can be used to manage the data set.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfTags :: Lens.Lens' CreateDataset (Core.Maybe (Core.NonEmpty Types.Tag))
cdfTags = Lens.field @"tags"
{-# DEPRECATED cdfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of triggers. A trigger causes data set contents to be populated at a specified time interval or when another data set's contents are created. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfTriggers :: Lens.Lens' CreateDataset (Core.Maybe [Types.DatasetTrigger])
cdfTriggers = Lens.field @"triggers"
{-# DEPRECATED cdfTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | Optional. How many versions of dataset contents are kept. If not specified or set to null, only the latest version plus the latest succeeded version (if they are different) are kept for the time period specified by the @retentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/getting-started.html#aws-iot-analytics-dataset-versions Keeping Multiple Versions of AWS IoT Analytics Data Sets> in the /AWS IoT Analytics User Guide/ .
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfVersioningConfiguration :: Lens.Lens' CreateDataset (Core.Maybe Types.VersioningConfiguration)
cdfVersioningConfiguration = Lens.field @"versioningConfiguration"
{-# DEPRECATED cdfVersioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead." #-}

instance Core.FromJSON CreateDataset where
  toJSON CreateDataset {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("datasetName" Core..= datasetName),
            Core.Just ("actions" Core..= actions),
            ("contentDeliveryRules" Core..=) Core.<$> contentDeliveryRules,
            ("lateDataRules" Core..=) Core.<$> lateDataRules,
            ("retentionPeriod" Core..=) Core.<$> retentionPeriod,
            ("tags" Core..=) Core.<$> tags,
            ("triggers" Core..=) Core.<$> triggers,
            ("versioningConfiguration" Core..=)
              Core.<$> versioningConfiguration
          ]
      )

instance Core.AWSRequest CreateDataset where
  type Rs CreateDataset = CreateDatasetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/datasets",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetResponse'
            Core.<$> (x Core..:? "datasetArn")
            Core.<*> (x Core..:? "datasetName")
            Core.<*> (x Core..:? "retentionPeriod")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDatasetResponse' smart constructor.
data CreateDatasetResponse = CreateDatasetResponse'
  { -- | The ARN of the dataset.
    datasetArn :: Core.Maybe Types.DatasetArn,
    -- | The name of the dataset.
    datasetName :: Core.Maybe Types.DatasetName,
    -- | How long, in days, dataset contents are kept for the dataset.
    retentionPeriod :: Core.Maybe Types.RetentionPeriod,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatasetResponse' value with any optional fields omitted.
mkCreateDatasetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDatasetResponse
mkCreateDatasetResponse responseStatus =
  CreateDatasetResponse'
    { datasetArn = Core.Nothing,
      datasetName = Core.Nothing,
      retentionPeriod = Core.Nothing,
      responseStatus
    }

-- | The ARN of the dataset.
--
-- /Note:/ Consider using 'datasetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDatasetArn :: Lens.Lens' CreateDatasetResponse (Core.Maybe Types.DatasetArn)
crsDatasetArn = Lens.field @"datasetArn"
{-# DEPRECATED crsDatasetArn "Use generic-lens or generic-optics with 'datasetArn' instead." #-}

-- | The name of the dataset.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDatasetName :: Lens.Lens' CreateDatasetResponse (Core.Maybe Types.DatasetName)
crsDatasetName = Lens.field @"datasetName"
{-# DEPRECATED crsDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | How long, in days, dataset contents are kept for the dataset.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsRetentionPeriod :: Lens.Lens' CreateDatasetResponse (Core.Maybe Types.RetentionPeriod)
crsRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED crsRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateDatasetResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
