{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing proxy.
module Network.AWS.RDS.DeleteDBProxy
    (
    -- * Creating a request
      DeleteDBProxy (..)
    , mkDeleteDBProxy
    -- ** Request lenses
    , ddbpDBProxyName

    -- * Destructuring the response
    , DeleteDBProxyResponse (..)
    , mkDeleteDBProxyResponse
    -- ** Response lenses
    , ddbprrsDBProxy
    , ddbprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDBProxy' smart constructor.
newtype DeleteDBProxy = DeleteDBProxy'
  { dBProxyName :: Core.Text
    -- ^ The name of the DB proxy to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBProxy' value with any optional fields omitted.
mkDeleteDBProxy
    :: Core.Text -- ^ 'dBProxyName'
    -> DeleteDBProxy
mkDeleteDBProxy dBProxyName = DeleteDBProxy'{dBProxyName}

-- | The name of the DB proxy to delete.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpDBProxyName :: Lens.Lens' DeleteDBProxy Core.Text
ddbpDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE ddbpDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

instance Core.ToQuery DeleteDBProxy where
        toQuery DeleteDBProxy{..}
          = Core.toQueryPair "Action" ("DeleteDBProxy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBProxyName" dBProxyName

instance Core.ToHeaders DeleteDBProxy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBProxy where
        type Rs DeleteDBProxy = DeleteDBProxyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeleteDBProxyResult"
              (\ s h x ->
                 DeleteDBProxyResponse' Core.<$>
                   (x Core..@? "DBProxy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBProxyResponse' smart constructor.
data DeleteDBProxyResponse = DeleteDBProxyResponse'
  { dBProxy :: Core.Maybe Types.DBProxy
    -- ^ The data structure representing the details of the DB proxy that you delete.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDBProxyResponse' value with any optional fields omitted.
mkDeleteDBProxyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDBProxyResponse
mkDeleteDBProxyResponse responseStatus
  = DeleteDBProxyResponse'{dBProxy = Core.Nothing, responseStatus}

-- | The data structure representing the details of the DB proxy that you delete.
--
-- /Note:/ Consider using 'dBProxy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprrsDBProxy :: Lens.Lens' DeleteDBProxyResponse (Core.Maybe Types.DBProxy)
ddbprrsDBProxy = Lens.field @"dBProxy"
{-# INLINEABLE ddbprrsDBProxy #-}
{-# DEPRECATED dBProxy "Use generic-lens or generic-optics with 'dBProxy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbprrsResponseStatus :: Lens.Lens' DeleteDBProxyResponse Core.Int
ddbprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
