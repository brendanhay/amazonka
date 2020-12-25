{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the contact information for a particular domain. You must specify information for at least one contact: registrant, administrator, or technical.
--
-- If the update is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.UpdateDomainContact
  ( -- * Creating a request
    UpdateDomainContact (..),
    mkUpdateDomainContact,

    -- ** Request lenses
    udcDomainName,
    udcAdminContact,
    udcRegistrantContact,
    udcTechContact,

    -- * Destructuring the response
    UpdateDomainContactResponse (..),
    mkUpdateDomainContactResponse,

    -- ** Response lenses
    udcrrsOperationId,
    udcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The UpdateDomainContact request includes the following elements.
--
-- /See:/ 'mkUpdateDomainContact' smart constructor.
data UpdateDomainContact = UpdateDomainContact'
  { -- | The name of the domain that you want to update contact information for.
    domainName :: Types.DomainName,
    -- | Provides detailed contact information.
    adminContact :: Core.Maybe Types.ContactDetail,
    -- | Provides detailed contact information.
    registrantContact :: Core.Maybe Types.ContactDetail,
    -- | Provides detailed contact information.
    techContact :: Core.Maybe Types.ContactDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainContact' value with any optional fields omitted.
mkUpdateDomainContact ::
  -- | 'domainName'
  Types.DomainName ->
  UpdateDomainContact
mkUpdateDomainContact domainName =
  UpdateDomainContact'
    { domainName,
      adminContact = Core.Nothing,
      registrantContact = Core.Nothing,
      techContact = Core.Nothing
    }

-- | The name of the domain that you want to update contact information for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDomainName :: Lens.Lens' UpdateDomainContact Types.DomainName
udcDomainName = Lens.field @"domainName"
{-# DEPRECATED udcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcAdminContact :: Lens.Lens' UpdateDomainContact (Core.Maybe Types.ContactDetail)
udcAdminContact = Lens.field @"adminContact"
{-# DEPRECATED udcAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcRegistrantContact :: Lens.Lens' UpdateDomainContact (Core.Maybe Types.ContactDetail)
udcRegistrantContact = Lens.field @"registrantContact"
{-# DEPRECATED udcRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcTechContact :: Lens.Lens' UpdateDomainContact (Core.Maybe Types.ContactDetail)
udcTechContact = Lens.field @"techContact"
{-# DEPRECATED udcTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}

instance Core.FromJSON UpdateDomainContact where
  toJSON UpdateDomainContact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            ("AdminContact" Core..=) Core.<$> adminContact,
            ("RegistrantContact" Core..=) Core.<$> registrantContact,
            ("TechContact" Core..=) Core.<$> techContact
          ]
      )

instance Core.AWSRequest UpdateDomainContact where
  type Rs UpdateDomainContact = UpdateDomainContactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53Domains_v20140515.UpdateDomainContact")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainContactResponse'
            Core.<$> (x Core..: "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The UpdateDomainContact response includes the following element.
--
-- /See:/ 'mkUpdateDomainContactResponse' smart constructor.
data UpdateDomainContactResponse = UpdateDomainContactResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainContactResponse' value with any optional fields omitted.
mkUpdateDomainContactResponse ::
  -- | 'operationId'
  Types.OperationId ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateDomainContactResponse
mkUpdateDomainContactResponse operationId responseStatus =
  UpdateDomainContactResponse' {operationId, responseStatus}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsOperationId :: Lens.Lens' UpdateDomainContactResponse Types.OperationId
udcrrsOperationId = Lens.field @"operationId"
{-# DEPRECATED udcrrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsResponseStatus :: Lens.Lens' UpdateDomainContactResponse Core.Int
udcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
