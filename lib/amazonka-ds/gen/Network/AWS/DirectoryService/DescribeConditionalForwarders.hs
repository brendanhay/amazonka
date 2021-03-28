{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeConditionalForwarders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the conditional forwarders for this account.
--
-- If no input parameters are provided for RemoteDomainNames, this request describes all conditional forwarders for the specified directory ID.
module Network.AWS.DirectoryService.DescribeConditionalForwarders
    (
    -- * Creating a request
      DescribeConditionalForwarders (..)
    , mkDescribeConditionalForwarders
    -- ** Request lenses
    , dcfDirectoryId
    , dcfRemoteDomainNames

    -- * Destructuring the response
    , DescribeConditionalForwardersResponse (..)
    , mkDescribeConditionalForwardersResponse
    -- ** Response lenses
    , dcfrrsConditionalForwarders
    , dcfrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a conditional forwarder.
--
-- /See:/ 'mkDescribeConditionalForwarders' smart constructor.
data DescribeConditionalForwarders = DescribeConditionalForwarders'
  { directoryId :: Types.DirectoryId
    -- ^ The directory ID for which to get the list of associated conditional forwarders.
  , remoteDomainNames :: Core.Maybe [Types.RemoteDomainName]
    -- ^ The fully qualified domain names (FQDN) of the remote domains for which to get the list of associated conditional forwarders. If this member is null, all conditional forwarders are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConditionalForwarders' value with any optional fields omitted.
mkDescribeConditionalForwarders
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DescribeConditionalForwarders
mkDescribeConditionalForwarders directoryId
  = DescribeConditionalForwarders'{directoryId,
                                   remoteDomainNames = Core.Nothing}

-- | The directory ID for which to get the list of associated conditional forwarders.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfDirectoryId :: Lens.Lens' DescribeConditionalForwarders Types.DirectoryId
dcfDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dcfDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The fully qualified domain names (FQDN) of the remote domains for which to get the list of associated conditional forwarders. If this member is null, all conditional forwarders are returned.
--
-- /Note:/ Consider using 'remoteDomainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfRemoteDomainNames :: Lens.Lens' DescribeConditionalForwarders (Core.Maybe [Types.RemoteDomainName])
dcfRemoteDomainNames = Lens.field @"remoteDomainNames"
{-# INLINEABLE dcfRemoteDomainNames #-}
{-# DEPRECATED remoteDomainNames "Use generic-lens or generic-optics with 'remoteDomainNames' instead"  #-}

instance Core.ToQuery DescribeConditionalForwarders where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConditionalForwarders where
        toHeaders DescribeConditionalForwarders{..}
          = Core.pure
              ("X-Amz-Target",
               "DirectoryService_20150416.DescribeConditionalForwarders")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeConditionalForwarders where
        toJSON DescribeConditionalForwarders{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("RemoteDomainNames" Core..=) Core.<$> remoteDomainNames])

instance Core.AWSRequest DescribeConditionalForwarders where
        type Rs DescribeConditionalForwarders =
             DescribeConditionalForwardersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConditionalForwardersResponse' Core.<$>
                   (x Core..:? "ConditionalForwarders") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a DescribeConditionalForwarder request.
--
-- /See:/ 'mkDescribeConditionalForwardersResponse' smart constructor.
data DescribeConditionalForwardersResponse = DescribeConditionalForwardersResponse'
  { conditionalForwarders :: Core.Maybe [Types.ConditionalForwarder]
    -- ^ The list of conditional forwarders that have been created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConditionalForwardersResponse' value with any optional fields omitted.
mkDescribeConditionalForwardersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConditionalForwardersResponse
mkDescribeConditionalForwardersResponse responseStatus
  = DescribeConditionalForwardersResponse'{conditionalForwarders =
                                             Core.Nothing,
                                           responseStatus}

-- | The list of conditional forwarders that have been created.
--
-- /Note:/ Consider using 'conditionalForwarders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrrsConditionalForwarders :: Lens.Lens' DescribeConditionalForwardersResponse (Core.Maybe [Types.ConditionalForwarder])
dcfrrsConditionalForwarders = Lens.field @"conditionalForwarders"
{-# INLINEABLE dcfrrsConditionalForwarders #-}
{-# DEPRECATED conditionalForwarders "Use generic-lens or generic-optics with 'conditionalForwarders' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrrsResponseStatus :: Lens.Lens' DescribeConditionalForwardersResponse Core.Int
dcfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
