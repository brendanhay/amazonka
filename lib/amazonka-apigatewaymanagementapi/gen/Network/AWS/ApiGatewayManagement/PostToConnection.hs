{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGatewayManagement.PostToConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends the provided data to the specified connection.
module Network.AWS.ApiGatewayManagement.PostToConnection
    (
    -- * Creating a request
      PostToConnection (..)
    , mkPostToConnection
    -- ** Request lenses
    , ptcConnectionId
    , ptcData

    -- * Destructuring the response
    , PostToConnectionResponse (..)
    , mkPostToConnectionResponse
    ) where

import qualified Network.AWS.ApiGatewayManagement.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPostToConnection' smart constructor.
data PostToConnection = PostToConnection'
  { connectionId :: Core.Text
    -- ^ The identifier of the connection that a specific client is using.
  , data' :: Core.ByteString
    -- ^ The data to be sent to the client specified by its connection id.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostToConnection' value with any optional fields omitted.
mkPostToConnection
    :: Core.Text -- ^ 'connectionId'
    -> Core.ByteString -- ^ 'data\''
    -> PostToConnection
mkPostToConnection connectionId data'
  = PostToConnection'{connectionId, data'}

-- | The identifier of the connection that a specific client is using.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcConnectionId :: Lens.Lens' PostToConnection Core.Text
ptcConnectionId = Lens.field @"connectionId"
{-# INLINEABLE ptcConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The data to be sent to the client specified by its connection id.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcData :: Lens.Lens' PostToConnection Core.ByteString
ptcData = Lens.field @"data'"
{-# INLINEABLE ptcData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

instance Core.ToQuery PostToConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PostToConnection where
        toHeaders PostToConnection{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest PostToConnection where
        type Rs PostToConnection = PostToConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/@connections/" Core.<> Core.toText connectionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody data'}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PostToConnectionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPostToConnectionResponse' smart constructor.
data PostToConnectionResponse = PostToConnectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostToConnectionResponse' value with any optional fields omitted.
mkPostToConnectionResponse
    :: PostToConnectionResponse
mkPostToConnectionResponse = PostToConnectionResponse'
