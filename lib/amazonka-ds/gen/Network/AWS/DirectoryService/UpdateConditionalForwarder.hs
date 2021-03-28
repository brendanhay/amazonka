{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UpdateConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a conditional forwarder that has been set up for your AWS directory.
module Network.AWS.DirectoryService.UpdateConditionalForwarder
    (
    -- * Creating a request
      UpdateConditionalForwarder (..)
    , mkUpdateConditionalForwarder
    -- ** Request lenses
    , ucfDirectoryId
    , ucfRemoteDomainName
    , ucfDnsIpAddrs

    -- * Destructuring the response
    , UpdateConditionalForwarderResponse (..)
    , mkUpdateConditionalForwarderResponse
    -- ** Response lenses
    , ucfrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a conditional forwarder.
--
-- /See:/ 'mkUpdateConditionalForwarder' smart constructor.
data UpdateConditionalForwarder = UpdateConditionalForwarder'
  { directoryId :: Types.DirectoryId
    -- ^ The directory ID of the AWS directory for which to update the conditional forwarder.
  , remoteDomainName :: Types.RemoteDomainName
    -- ^ The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
  , dnsIpAddrs :: [Types.IpAddr]
    -- ^ The updated IP addresses of the remote DNS server associated with the conditional forwarder.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConditionalForwarder' value with any optional fields omitted.
mkUpdateConditionalForwarder
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.RemoteDomainName -- ^ 'remoteDomainName'
    -> UpdateConditionalForwarder
mkUpdateConditionalForwarder directoryId remoteDomainName
  = UpdateConditionalForwarder'{directoryId, remoteDomainName,
                                dnsIpAddrs = Core.mempty}

-- | The directory ID of the AWS directory for which to update the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfDirectoryId :: Lens.Lens' UpdateConditionalForwarder Types.DirectoryId
ucfDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE ucfDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfRemoteDomainName :: Lens.Lens' UpdateConditionalForwarder Types.RemoteDomainName
ucfRemoteDomainName = Lens.field @"remoteDomainName"
{-# INLINEABLE ucfRemoteDomainName #-}
{-# DEPRECATED remoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead"  #-}

-- | The updated IP addresses of the remote DNS server associated with the conditional forwarder.
--
-- /Note:/ Consider using 'dnsIpAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfDnsIpAddrs :: Lens.Lens' UpdateConditionalForwarder [Types.IpAddr]
ucfDnsIpAddrs = Lens.field @"dnsIpAddrs"
{-# INLINEABLE ucfDnsIpAddrs #-}
{-# DEPRECATED dnsIpAddrs "Use generic-lens or generic-optics with 'dnsIpAddrs' instead"  #-}

instance Core.ToQuery UpdateConditionalForwarder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateConditionalForwarder where
        toHeaders UpdateConditionalForwarder{..}
          = Core.pure
              ("X-Amz-Target",
               "DirectoryService_20150416.UpdateConditionalForwarder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateConditionalForwarder where
        toJSON UpdateConditionalForwarder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("RemoteDomainName" Core..= remoteDomainName),
                  Core.Just ("DnsIpAddrs" Core..= dnsIpAddrs)])

instance Core.AWSRequest UpdateConditionalForwarder where
        type Rs UpdateConditionalForwarder =
             UpdateConditionalForwarderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateConditionalForwarderResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The result of an UpdateConditionalForwarder request.
--
-- /See:/ 'mkUpdateConditionalForwarderResponse' smart constructor.
newtype UpdateConditionalForwarderResponse = UpdateConditionalForwarderResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConditionalForwarderResponse' value with any optional fields omitted.
mkUpdateConditionalForwarderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateConditionalForwarderResponse
mkUpdateConditionalForwarderResponse responseStatus
  = UpdateConditionalForwarderResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfrrsResponseStatus :: Lens.Lens' UpdateConditionalForwarderResponse Core.Int
ucfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
