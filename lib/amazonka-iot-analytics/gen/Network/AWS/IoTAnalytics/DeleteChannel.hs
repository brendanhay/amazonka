{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified channel.
module Network.AWS.IoTAnalytics.DeleteChannel
    (
    -- * Creating a request
      DeleteChannel (..)
    , mkDeleteChannel
    -- ** Request lenses
    , dcChannelName

    -- * Destructuring the response
    , DeleteChannelResponse (..)
    , mkDeleteChannelResponse
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel'
  { channelName :: Types.ChannelName
    -- ^ The name of the channel to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChannel' value with any optional fields omitted.
mkDeleteChannel
    :: Types.ChannelName -- ^ 'channelName'
    -> DeleteChannel
mkDeleteChannel channelName = DeleteChannel'{channelName}

-- | The name of the channel to delete.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcChannelName :: Lens.Lens' DeleteChannel Types.ChannelName
dcChannelName = Lens.field @"channelName"
{-# INLINEABLE dcChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

instance Core.ToQuery DeleteChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteChannel where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteChannel where
        type Rs DeleteChannel = DeleteChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/channels/" Core.<> Core.toText channelName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteChannelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChannelResponse' value with any optional fields omitted.
mkDeleteChannelResponse
    :: DeleteChannelResponse
mkDeleteChannelResponse = DeleteChannelResponse'
