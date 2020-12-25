{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainNameservers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.
--
-- If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.UpdateDomainNameservers
  ( -- * Creating a request
    UpdateDomainNameservers (..),
    mkUpdateDomainNameservers,

    -- ** Request lenses
    udnDomainName,
    udnNameservers,
    udnFIAuthKey,

    -- * Destructuring the response
    UpdateDomainNameserversResponse (..),
    mkUpdateDomainNameserversResponse,

    -- ** Response lenses
    udnrrsOperationId,
    udnrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | Replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.
--
-- If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
--
-- /See:/ 'mkUpdateDomainNameservers' smart constructor.
data UpdateDomainNameservers = UpdateDomainNameservers'
  { -- | The name of the domain that you want to change name servers for.
    domainName :: Types.DomainName,
    -- | A list of new name servers for the domain.
    nameservers :: [Types.Nameserver],
    -- | The authorization key for .fi domains
    fIAuthKey :: Core.Maybe Types.FIAuthKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainNameservers' value with any optional fields omitted.
mkUpdateDomainNameservers ::
  -- | 'domainName'
  Types.DomainName ->
  UpdateDomainNameservers
mkUpdateDomainNameservers domainName =
  UpdateDomainNameservers'
    { domainName,
      nameservers = Core.mempty,
      fIAuthKey = Core.Nothing
    }

-- | The name of the domain that you want to change name servers for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnDomainName :: Lens.Lens' UpdateDomainNameservers Types.DomainName
udnDomainName = Lens.field @"domainName"
{-# DEPRECATED udnDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A list of new name servers for the domain.
--
-- /Note:/ Consider using 'nameservers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnNameservers :: Lens.Lens' UpdateDomainNameservers [Types.Nameserver]
udnNameservers = Lens.field @"nameservers"
{-# DEPRECATED udnNameservers "Use generic-lens or generic-optics with 'nameservers' instead." #-}

-- | The authorization key for .fi domains
--
-- /Note:/ Consider using 'fIAuthKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnFIAuthKey :: Lens.Lens' UpdateDomainNameservers (Core.Maybe Types.FIAuthKey)
udnFIAuthKey = Lens.field @"fIAuthKey"
{-# DEPRECATED udnFIAuthKey "Use generic-lens or generic-optics with 'fIAuthKey' instead." #-}

instance Core.FromJSON UpdateDomainNameservers where
  toJSON UpdateDomainNameservers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            Core.Just ("Nameservers" Core..= nameservers),
            ("FIAuthKey" Core..=) Core.<$> fIAuthKey
          ]
      )

instance Core.AWSRequest UpdateDomainNameservers where
  type Rs UpdateDomainNameservers = UpdateDomainNameserversResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Route53Domains_v20140515.UpdateDomainNameservers"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainNameserversResponse'
            Core.<$> (x Core..: "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The UpdateDomainNameservers response includes the following element.
--
-- /See:/ 'mkUpdateDomainNameserversResponse' smart constructor.
data UpdateDomainNameserversResponse = UpdateDomainNameserversResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainNameserversResponse' value with any optional fields omitted.
mkUpdateDomainNameserversResponse ::
  -- | 'operationId'
  Types.OperationId ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateDomainNameserversResponse
mkUpdateDomainNameserversResponse operationId responseStatus =
  UpdateDomainNameserversResponse' {operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnrrsOperationId :: Lens.Lens' UpdateDomainNameserversResponse Types.OperationId
udnrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED udnrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnrrsResponseStatus :: Lens.Lens' UpdateDomainNameserversResponse Core.Int
udnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
