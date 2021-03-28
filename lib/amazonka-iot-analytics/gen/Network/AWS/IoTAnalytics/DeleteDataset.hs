{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified dataset.
--
-- You do not have to delete the content of the dataset before you perform this operation.
module Network.AWS.IoTAnalytics.DeleteDataset
    (
    -- * Creating a request
      DeleteDataset (..)
    , mkDeleteDataset
    -- ** Request lenses
    , dDatasetName

    -- * Destructuring the response
    , DeleteDatasetResponse (..)
    , mkDeleteDatasetResponse
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDataset' smart constructor.
newtype DeleteDataset = DeleteDataset'
  { datasetName :: Types.DatasetName
    -- ^ The name of the data set to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataset' value with any optional fields omitted.
mkDeleteDataset
    :: Types.DatasetName -- ^ 'datasetName'
    -> DeleteDataset
mkDeleteDataset datasetName = DeleteDataset'{datasetName}

-- | The name of the data set to delete.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDatasetName :: Lens.Lens' DeleteDataset Types.DatasetName
dDatasetName = Lens.field @"datasetName"
{-# INLINEABLE dDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

instance Core.ToQuery DeleteDataset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDataset where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDataset where
        type Rs DeleteDataset = DeleteDatasetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/datasets/" Core.<> Core.toText datasetName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteDatasetResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDatasetResponse' value with any optional fields omitted.
mkDeleteDatasetResponse
    :: DeleteDatasetResponse
mkDeleteDatasetResponse = DeleteDatasetResponse'
