{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connection from the Data Catalog.
module Network.AWS.Glue.DeleteConnection
    (
    -- * Creating a request
      DeleteConnection (..)
    , mkDeleteConnection
    -- ** Request lenses
    , dcConnectionName
    , dcCatalogId

    -- * Destructuring the response
    , DeleteConnectionResponse (..)
    , mkDeleteConnectionResponse
    -- ** Response lenses
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { connectionName :: Types.NameString
    -- ^ The name of the connection to delete.
  , catalogId :: Core.Maybe Types.CatalogIdString
    -- ^ The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnection' value with any optional fields omitted.
mkDeleteConnection
    :: Types.NameString -- ^ 'connectionName'
    -> DeleteConnection
mkDeleteConnection connectionName
  = DeleteConnection'{connectionName, catalogId = Core.Nothing}

-- | The name of the connection to delete.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectionName :: Lens.Lens' DeleteConnection Types.NameString
dcConnectionName = Lens.field @"connectionName"
{-# INLINEABLE dcConnectionName #-}
{-# DEPRECATED connectionName "Use generic-lens or generic-optics with 'connectionName' instead"  #-}

-- | The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCatalogId :: Lens.Lens' DeleteConnection (Core.Maybe Types.CatalogIdString)
dcCatalogId = Lens.field @"catalogId"
{-# INLINEABLE dcCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery DeleteConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteConnection where
        toHeaders DeleteConnection{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteConnection") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteConnection where
        toJSON DeleteConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConnectionName" Core..= connectionName),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest DeleteConnection where
        type Rs DeleteConnection = DeleteConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteConnectionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteConnectionResponse' smart constructor.
newtype DeleteConnectionResponse = DeleteConnectionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectionResponse' value with any optional fields omitted.
mkDeleteConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteConnectionResponse
mkDeleteConnectionResponse responseStatus
  = DeleteConnectionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteConnectionResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
