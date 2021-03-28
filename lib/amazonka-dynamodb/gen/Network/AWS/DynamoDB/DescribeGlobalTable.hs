{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeGlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified global table.
module Network.AWS.DynamoDB.DescribeGlobalTable
    (
    -- * Creating a request
      DescribeGlobalTable (..)
    , mkDescribeGlobalTable
    -- ** Request lenses
    , dgtGlobalTableName

    -- * Destructuring the response
    , DescribeGlobalTableResponse (..)
    , mkDescribeGlobalTableResponse
    -- ** Response lenses
    , dgtrrsGlobalTableDescription
    , dgtrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGlobalTable' smart constructor.
newtype DescribeGlobalTable = DescribeGlobalTable'
  { globalTableName :: Types.GlobalTableName
    -- ^ The name of the global table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalTable' value with any optional fields omitted.
mkDescribeGlobalTable
    :: Types.GlobalTableName -- ^ 'globalTableName'
    -> DescribeGlobalTable
mkDescribeGlobalTable globalTableName
  = DescribeGlobalTable'{globalTableName}

-- | The name of the global table.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtGlobalTableName :: Lens.Lens' DescribeGlobalTable Types.GlobalTableName
dgtGlobalTableName = Lens.field @"globalTableName"
{-# INLINEABLE dgtGlobalTableName #-}
{-# DEPRECATED globalTableName "Use generic-lens or generic-optics with 'globalTableName' instead"  #-}

instance Core.ToQuery DescribeGlobalTable where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeGlobalTable where
        toHeaders DescribeGlobalTable{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.DescribeGlobalTable")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeGlobalTable where
        toJSON DescribeGlobalTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GlobalTableName" Core..= globalTableName)])

instance Core.AWSRequest DescribeGlobalTable where
        type Rs DescribeGlobalTable = DescribeGlobalTableResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeGlobalTableResponse' Core.<$>
                   (x Core..:? "GlobalTableDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeGlobalTableResponse' smart constructor.
data DescribeGlobalTableResponse = DescribeGlobalTableResponse'
  { globalTableDescription :: Core.Maybe Types.GlobalTableDescription
    -- ^ Contains the details of the global table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeGlobalTableResponse' value with any optional fields omitted.
mkDescribeGlobalTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGlobalTableResponse
mkDescribeGlobalTableResponse responseStatus
  = DescribeGlobalTableResponse'{globalTableDescription =
                                   Core.Nothing,
                                 responseStatus}

-- | Contains the details of the global table.
--
-- /Note:/ Consider using 'globalTableDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtrrsGlobalTableDescription :: Lens.Lens' DescribeGlobalTableResponse (Core.Maybe Types.GlobalTableDescription)
dgtrrsGlobalTableDescription = Lens.field @"globalTableDescription"
{-# INLINEABLE dgtrrsGlobalTableDescription #-}
{-# DEPRECATED globalTableDescription "Use generic-lens or generic-optics with 'globalTableDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgtrrsResponseStatus :: Lens.Lens' DescribeGlobalTableResponse Core.Int
dgtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
