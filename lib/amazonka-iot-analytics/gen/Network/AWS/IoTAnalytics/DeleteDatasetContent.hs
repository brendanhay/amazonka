{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteDatasetContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of the specified dataset.
module Network.AWS.IoTAnalytics.DeleteDatasetContent
    (
    -- * Creating a request
      DeleteDatasetContent (..)
    , mkDeleteDatasetContent
    -- ** Request lenses
    , ddcDatasetName
    , ddcVersionId

    -- * Destructuring the response
    , DeleteDatasetContentResponse (..)
    , mkDeleteDatasetContentResponse
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDatasetContent' smart constructor.
data DeleteDatasetContent = DeleteDatasetContent'
  { datasetName :: Types.DatasetName
    -- ^ The name of the dataset whose content is deleted.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ The version of the dataset whose content is deleted. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to delete the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDatasetContent' value with any optional fields omitted.
mkDeleteDatasetContent
    :: Types.DatasetName -- ^ 'datasetName'
    -> DeleteDatasetContent
mkDeleteDatasetContent datasetName
  = DeleteDatasetContent'{datasetName, versionId = Core.Nothing}

-- | The name of the dataset whose content is deleted.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDatasetName :: Lens.Lens' DeleteDatasetContent Types.DatasetName
ddcDatasetName = Lens.field @"datasetName"
{-# INLINEABLE ddcDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | The version of the dataset whose content is deleted. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to delete the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcVersionId :: Lens.Lens' DeleteDatasetContent (Core.Maybe Types.VersionId)
ddcVersionId = Lens.field @"versionId"
{-# INLINEABLE ddcVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery DeleteDatasetContent where
        toQuery DeleteDatasetContent{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId

instance Core.ToHeaders DeleteDatasetContent where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDatasetContent where
        type Rs DeleteDatasetContent = DeleteDatasetContentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/datasets/" Core.<> Core.toText datasetName Core.<> "/content",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteDatasetContentResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDatasetContentResponse' smart constructor.
data DeleteDatasetContentResponse = DeleteDatasetContentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDatasetContentResponse' value with any optional fields omitted.
mkDeleteDatasetContentResponse
    :: DeleteDatasetContentResponse
mkDeleteDatasetContentResponse = DeleteDatasetContentResponse'
