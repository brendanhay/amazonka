{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a Channel.
module Network.AWS.MediaPackage.DescribeChannel
    (
    -- * Creating a request
      DescribeChannel (..)
    , mkDescribeChannel
    -- ** Request lenses
    , dId

    -- * Destructuring the response
    , DescribeChannelResponse (..)
    , mkDescribeChannelResponse
    -- ** Response lenses
    , dcrrsArn
    , dcrrsDescription
    , dcrrsEgressAccessLogs
    , dcrrsHlsIngest
    , dcrrsId
    , dcrrsIngressAccessLogs
    , dcrrsTags
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel'
  { id :: Core.Text
    -- ^ The ID of a Channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChannel' value with any optional fields omitted.
mkDescribeChannel
    :: Core.Text -- ^ 'id'
    -> DescribeChannel
mkDescribeChannel id = DescribeChannel'{id}

-- | The ID of a Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DescribeChannel Core.Text
dId = Lens.field @"id"
{-# INLINEABLE dId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DescribeChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeChannel where
        toHeaders DescribeChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeChannel where
        type Rs DescribeChannel = DescribeChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/channels/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeChannelResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "description" Core.<*>
                     x Core..:? "egressAccessLogs"
                     Core.<*> x Core..:? "hlsIngest"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "ingressAccessLogs"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) assigned to the Channel.
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the Channel.
  , egressAccessLogs :: Core.Maybe Types.EgressAccessLogs
  , hlsIngest :: Core.Maybe Types.HlsIngest
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the Channel.
  , ingressAccessLogs :: Core.Maybe Types.IngressAccessLogs
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChannelResponse' value with any optional fields omitted.
mkDescribeChannelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeChannelResponse
mkDescribeChannelResponse responseStatus
  = DescribeChannelResponse'{arn = Core.Nothing,
                             description = Core.Nothing, egressAccessLogs = Core.Nothing,
                             hlsIngest = Core.Nothing, id = Core.Nothing,
                             ingressAccessLogs = Core.Nothing, tags = Core.Nothing,
                             responseStatus}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsArn :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
dcrrsArn = Lens.field @"arn"
{-# INLINEABLE dcrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsDescription :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
dcrrsDescription = Lens.field @"description"
{-# INLINEABLE dcrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsEgressAccessLogs :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.EgressAccessLogs)
dcrrsEgressAccessLogs = Lens.field @"egressAccessLogs"
{-# INLINEABLE dcrrsEgressAccessLogs #-}
{-# DEPRECATED egressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsHlsIngest :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.HlsIngest)
dcrrsHlsIngest = Lens.field @"hlsIngest"
{-# INLINEABLE dcrrsHlsIngest #-}
{-# DEPRECATED hlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead"  #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsId :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
dcrrsId = Lens.field @"id"
{-# INLINEABLE dcrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsIngressAccessLogs :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.IngressAccessLogs)
dcrrsIngressAccessLogs = Lens.field @"ingressAccessLogs"
{-# INLINEABLE dcrrsIngressAccessLogs #-}
{-# DEPRECATED ingressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsTags :: Lens.Lens' DescribeChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dcrrsTags = Lens.field @"tags"
{-# INLINEABLE dcrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeChannelResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
