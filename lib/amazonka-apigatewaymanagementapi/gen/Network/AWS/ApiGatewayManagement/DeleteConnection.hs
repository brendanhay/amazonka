{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGatewayManagement.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the connection with the provided id.
module Network.AWS.ApiGatewayManagement.DeleteConnection
    (
    -- * Creating a request
      DeleteConnection (..)
    , mkDeleteConnection
    -- ** Request lenses
    , dcConnectionId

    -- * Destructuring the response
    , DeleteConnectionResponse (..)
    , mkDeleteConnectionResponse
    ) where

import qualified Network.AWS.ApiGatewayManagement.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteConnection' smart constructor.
newtype DeleteConnection = DeleteConnection'
  { connectionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnection' value with any optional fields omitted.
mkDeleteConnection
    :: Core.Text -- ^ 'connectionId'
    -> DeleteConnection
mkDeleteConnection connectionId = DeleteConnection'{connectionId}

-- | Undocumented field.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectionId :: Lens.Lens' DeleteConnection Core.Text
dcConnectionId = Lens.field @"connectionId"
{-# INLINEABLE dcConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

instance Core.ToQuery DeleteConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteConnection where
        toHeaders DeleteConnection{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteConnection where
        type Rs DeleteConnection = DeleteConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/@connections/" Core.<> Core.toText connectionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteConnectionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectionResponse' value with any optional fields omitted.
mkDeleteConnectionResponse
    :: DeleteConnectionResponse
mkDeleteConnectionResponse = DeleteConnectionResponse'
