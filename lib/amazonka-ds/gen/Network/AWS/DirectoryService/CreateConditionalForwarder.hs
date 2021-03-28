{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a conditional forwarder associated with your AWS directory. Conditional forwarders are required in order to set up a trust relationship with another domain. The conditional forwarder points to the trusted domain.
module Network.AWS.DirectoryService.CreateConditionalForwarder
    (
    -- * Creating a request
      CreateConditionalForwarder (..)
    , mkCreateConditionalForwarder
    -- ** Request lenses
    , ccfDirectoryId
    , ccfRemoteDomainName
    , ccfDnsIpAddrs

    -- * Destructuring the response
    , CreateConditionalForwarderResponse (..)
    , mkCreateConditionalForwarderResponse
    -- ** Response lenses
    , ccfrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
-- /See:/ 'mkCreateConditionalForwarder' smart constructor.
data CreateConditionalForwarder = CreateConditionalForwarder'
  { directoryId :: Types.DirectoryId
    -- ^ The directory ID of the AWS directory for which you are creating the conditional forwarder.
  , remoteDomainName :: Types.RemoteDomainName
    -- ^ The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
  , dnsIpAddrs :: [Types.IpAddr]
    -- ^ The IP addresses of the remote DNS server associated with RemoteDomainName.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConditionalForwarder' value with any optional fields omitted.
mkCreateConditionalForwarder
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.RemoteDomainName -- ^ 'remoteDomainName'
    -> CreateConditionalForwarder
mkCreateConditionalForwarder directoryId remoteDomainName
  = CreateConditionalForwarder'{directoryId, remoteDomainName,
                                dnsIpAddrs = Core.mempty}

-- | The directory ID of the AWS directory for which you are creating the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDirectoryId :: Lens.Lens' CreateConditionalForwarder Types.DirectoryId
ccfDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE ccfDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRemoteDomainName :: Lens.Lens' CreateConditionalForwarder Types.RemoteDomainName
ccfRemoteDomainName = Lens.field @"remoteDomainName"
{-# INLINEABLE ccfRemoteDomainName #-}
{-# DEPRECATED remoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead"  #-}

-- | The IP addresses of the remote DNS server associated with RemoteDomainName.
--
-- /Note:/ Consider using 'dnsIpAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDnsIpAddrs :: Lens.Lens' CreateConditionalForwarder [Types.IpAddr]
ccfDnsIpAddrs = Lens.field @"dnsIpAddrs"
{-# INLINEABLE ccfDnsIpAddrs #-}
{-# DEPRECATED dnsIpAddrs "Use generic-lens or generic-optics with 'dnsIpAddrs' instead"  #-}

instance Core.ToQuery CreateConditionalForwarder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateConditionalForwarder where
        toHeaders CreateConditionalForwarder{..}
          = Core.pure
              ("X-Amz-Target",
               "DirectoryService_20150416.CreateConditionalForwarder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateConditionalForwarder where
        toJSON CreateConditionalForwarder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("RemoteDomainName" Core..= remoteDomainName),
                  Core.Just ("DnsIpAddrs" Core..= dnsIpAddrs)])

instance Core.AWSRequest CreateConditionalForwarder where
        type Rs CreateConditionalForwarder =
             CreateConditionalForwarderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateConditionalForwarderResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The result of a CreateConditinalForwarder request.
--
-- /See:/ 'mkCreateConditionalForwarderResponse' smart constructor.
newtype CreateConditionalForwarderResponse = CreateConditionalForwarderResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConditionalForwarderResponse' value with any optional fields omitted.
mkCreateConditionalForwarderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateConditionalForwarderResponse
mkCreateConditionalForwarderResponse responseStatus
  = CreateConditionalForwarderResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrrsResponseStatus :: Lens.Lens' CreateConditionalForwarderResponse Core.Int
ccfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
