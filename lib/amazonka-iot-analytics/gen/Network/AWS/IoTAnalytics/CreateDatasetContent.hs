{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateDatasetContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the content of a data set by applying a @queryAction@ (a SQL query) or a @containerAction@ (executing a containerized application).
module Network.AWS.IoTAnalytics.CreateDatasetContent
    (
    -- * Creating a request
      CreateDatasetContent (..)
    , mkCreateDatasetContent
    -- ** Request lenses
    , cdcDatasetName
    , cdcVersionId

    -- * Destructuring the response
    , CreateDatasetContentResponse (..)
    , mkCreateDatasetContentResponse
    -- ** Response lenses
    , cdcrrsVersionId
    , cdcrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDatasetContent' smart constructor.
data CreateDatasetContent = CreateDatasetContent'
  { datasetName :: Types.DatasetName
    -- ^ The name of the dataset.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ The version ID of the dataset content. To specify @versionId@ for a dataset content, the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatasetContent' value with any optional fields omitted.
mkCreateDatasetContent
    :: Types.DatasetName -- ^ 'datasetName'
    -> CreateDatasetContent
mkCreateDatasetContent datasetName
  = CreateDatasetContent'{datasetName, versionId = Core.Nothing}

-- | The name of the dataset.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDatasetName :: Lens.Lens' CreateDatasetContent Types.DatasetName
cdcDatasetName = Lens.field @"datasetName"
{-# INLINEABLE cdcDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | The version ID of the dataset content. To specify @versionId@ for a dataset content, the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcVersionId :: Lens.Lens' CreateDatasetContent (Core.Maybe Types.VersionId)
cdcVersionId = Lens.field @"versionId"
{-# INLINEABLE cdcVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery CreateDatasetContent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDatasetContent where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateDatasetContent where
        toJSON CreateDatasetContent{..}
          = Core.object
              (Core.catMaybes [("versionId" Core..=) Core.<$> versionId])

instance Core.AWSRequest CreateDatasetContent where
        type Rs CreateDatasetContent = CreateDatasetContentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/datasets/" Core.<> Core.toText datasetName Core.<> "/content",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDatasetContentResponse' Core.<$>
                   (x Core..:? "versionId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDatasetContentResponse' smart constructor.
data CreateDatasetContentResponse = CreateDatasetContentResponse'
  { versionId :: Core.Maybe Types.DatasetContentVersion
    -- ^ The version ID of the dataset contents that are being created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDatasetContentResponse' value with any optional fields omitted.
mkCreateDatasetContentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDatasetContentResponse
mkCreateDatasetContentResponse responseStatus
  = CreateDatasetContentResponse'{versionId = Core.Nothing,
                                  responseStatus}

-- | The version ID of the dataset contents that are being created.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsVersionId :: Lens.Lens' CreateDatasetContentResponse (Core.Maybe Types.DatasetContentVersion)
cdcrrsVersionId = Lens.field @"versionId"
{-# INLINEABLE cdcrrsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsResponseStatus :: Lens.Lens' CreateDatasetContentResponse Core.Int
cdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
