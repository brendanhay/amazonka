{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateConditionalForwarder (..),
    mkUpdateConditionalForwarder,

    -- ** Request lenses
    ucfDirectoryId,
    ucfRemoteDomainName,
    ucfDnsIpAddrs,

    -- * Destructuring the response
    UpdateConditionalForwarderResponse (..),
    mkUpdateConditionalForwarderResponse,

    -- ** Response lenses
    ucfrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a conditional forwarder.
--
-- /See:/ 'mkUpdateConditionalForwarder' smart constructor.
data UpdateConditionalForwarder = UpdateConditionalForwarder'
  { -- | The directory ID of the AWS directory for which to update the conditional forwarder.
    directoryId :: Types.DirectoryId,
    -- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
    remoteDomainName :: Types.RemoteDomainName,
    -- | The updated IP addresses of the remote DNS server associated with the conditional forwarder.
    dnsIpAddrs :: [Types.IpAddr]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConditionalForwarder' value with any optional fields omitted.
mkUpdateConditionalForwarder ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'remoteDomainName'
  Types.RemoteDomainName ->
  UpdateConditionalForwarder
mkUpdateConditionalForwarder directoryId remoteDomainName =
  UpdateConditionalForwarder'
    { directoryId,
      remoteDomainName,
      dnsIpAddrs = Core.mempty
    }

-- | The directory ID of the AWS directory for which to update the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfDirectoryId :: Lens.Lens' UpdateConditionalForwarder Types.DirectoryId
ucfDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED ucfDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfRemoteDomainName :: Lens.Lens' UpdateConditionalForwarder Types.RemoteDomainName
ucfRemoteDomainName = Lens.field @"remoteDomainName"
{-# DEPRECATED ucfRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

-- | The updated IP addresses of the remote DNS server associated with the conditional forwarder.
--
-- /Note:/ Consider using 'dnsIpAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfDnsIpAddrs :: Lens.Lens' UpdateConditionalForwarder [Types.IpAddr]
ucfDnsIpAddrs = Lens.field @"dnsIpAddrs"
{-# DEPRECATED ucfDnsIpAddrs "Use generic-lens or generic-optics with 'dnsIpAddrs' instead." #-}

instance Core.FromJSON UpdateConditionalForwarder where
  toJSON UpdateConditionalForwarder {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("RemoteDomainName" Core..= remoteDomainName),
            Core.Just ("DnsIpAddrs" Core..= dnsIpAddrs)
          ]
      )

instance Core.AWSRequest UpdateConditionalForwarder where
  type
    Rs UpdateConditionalForwarder =
      UpdateConditionalForwarderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "DirectoryService_20150416.UpdateConditionalForwarder"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConditionalForwarderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The result of an UpdateConditionalForwarder request.
--
-- /See:/ 'mkUpdateConditionalForwarderResponse' smart constructor.
newtype UpdateConditionalForwarderResponse = UpdateConditionalForwarderResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConditionalForwarderResponse' value with any optional fields omitted.
mkUpdateConditionalForwarderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateConditionalForwarderResponse
mkUpdateConditionalForwarderResponse responseStatus =
  UpdateConditionalForwarderResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfrrsResponseStatus :: Lens.Lens' UpdateConditionalForwarderResponse Core.Int
ucfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
