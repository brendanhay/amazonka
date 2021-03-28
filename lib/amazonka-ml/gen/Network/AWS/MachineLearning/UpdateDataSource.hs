{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @DataSourceName@ of a @DataSource@ .
--
-- You can use the @GetDataSource@ operation to view the contents of the updated data element.
module Network.AWS.MachineLearning.UpdateDataSource
    (
    -- * Creating a request
      UpdateDataSource (..)
    , mkUpdateDataSource
    -- ** Request lenses
    , udsDataSourceId
    , udsDataSourceName

    -- * Destructuring the response
    , UpdateDataSourceResponse (..)
    , mkUpdateDataSourceResponse
    -- ** Response lenses
    , udsrrsDataSourceId
    , udsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { dataSourceId :: Types.DataSourceId
    -- ^ The ID assigned to the @DataSource@ during creation.
  , dataSourceName :: Types.EntityName
    -- ^ A new user-supplied name or description of the @DataSource@ that will replace the current description. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataSource' value with any optional fields omitted.
mkUpdateDataSource
    :: Types.DataSourceId -- ^ 'dataSourceId'
    -> Types.EntityName -- ^ 'dataSourceName'
    -> UpdateDataSource
mkUpdateDataSource dataSourceId dataSourceName
  = UpdateDataSource'{dataSourceId, dataSourceName}

-- | The ID assigned to the @DataSource@ during creation.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDataSourceId :: Lens.Lens' UpdateDataSource Types.DataSourceId
udsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE udsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | A new user-supplied name or description of the @DataSource@ that will replace the current description. 
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDataSourceName :: Lens.Lens' UpdateDataSource Types.EntityName
udsDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE udsDataSourceName #-}
{-# DEPRECATED dataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead"  #-}

instance Core.ToQuery UpdateDataSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDataSource where
        toHeaders UpdateDataSource{..}
          = Core.pure ("X-Amz-Target", "AmazonML_20141212.UpdateDataSource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDataSource where
        toJSON UpdateDataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DataSourceId" Core..= dataSourceId),
                  Core.Just ("DataSourceName" Core..= dataSourceName)])

instance Core.AWSRequest UpdateDataSource where
        type Rs UpdateDataSource = UpdateDataSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDataSourceResponse' Core.<$>
                   (x Core..:? "DataSourceId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of an @UpdateDataSource@ operation.
--
-- You can see the updated content by using the @GetBatchPrediction@ operation.
--
-- /See:/ 'mkUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { dataSourceId :: Core.Maybe Types.DataSourceId
    -- ^ The ID assigned to the @DataSource@ during creation. This value should be identical to the value of the @DataSourceID@ in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataSourceResponse' value with any optional fields omitted.
mkUpdateDataSourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDataSourceResponse
mkUpdateDataSourceResponse responseStatus
  = UpdateDataSourceResponse'{dataSourceId = Core.Nothing,
                              responseStatus}

-- | The ID assigned to the @DataSource@ during creation. This value should be identical to the value of the @DataSourceID@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsDataSourceId :: Lens.Lens' UpdateDataSourceResponse (Core.Maybe Types.DataSourceId)
udsrrsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE udsrrsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsResponseStatus :: Lens.Lens' UpdateDataSourceResponse Core.Int
udsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
