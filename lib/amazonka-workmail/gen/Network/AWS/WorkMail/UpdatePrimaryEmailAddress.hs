{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdatePrimaryEmailAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the primary email for a user, group, or resource. The current email is moved into the list of aliases (or swapped between an existing alias and the current primary email), and the email provided in the input is promoted as the primary.
module Network.AWS.WorkMail.UpdatePrimaryEmailAddress
  ( -- * Creating a request
    UpdatePrimaryEmailAddress (..),
    mkUpdatePrimaryEmailAddress,

    -- ** Request lenses
    upeaOrganizationId,
    upeaEntityId,
    upeaEmail,

    -- * Destructuring the response
    UpdatePrimaryEmailAddressResponse (..),
    mkUpdatePrimaryEmailAddressResponse,

    -- ** Response lenses
    upearrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkUpdatePrimaryEmailAddress' smart constructor.
data UpdatePrimaryEmailAddress = UpdatePrimaryEmailAddress'
  { -- | The organization that contains the user, group, or resource to update.
    organizationId :: Types.OrganizationId,
    -- | The user, group, or resource to update.
    entityId :: Types.WorkMailIdentifier,
    -- | The value of the email to be updated as primary.
    email :: Types.EmailAddress
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePrimaryEmailAddress' value with any optional fields omitted.
mkUpdatePrimaryEmailAddress ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'entityId'
  Types.WorkMailIdentifier ->
  -- | 'email'
  Types.EmailAddress ->
  UpdatePrimaryEmailAddress
mkUpdatePrimaryEmailAddress organizationId entityId email =
  UpdatePrimaryEmailAddress' {organizationId, entityId, email}

-- | The organization that contains the user, group, or resource to update.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upeaOrganizationId :: Lens.Lens' UpdatePrimaryEmailAddress Types.OrganizationId
upeaOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED upeaOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The user, group, or resource to update.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upeaEntityId :: Lens.Lens' UpdatePrimaryEmailAddress Types.WorkMailIdentifier
upeaEntityId = Lens.field @"entityId"
{-# DEPRECATED upeaEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The value of the email to be updated as primary.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upeaEmail :: Lens.Lens' UpdatePrimaryEmailAddress Types.EmailAddress
upeaEmail = Lens.field @"email"
{-# DEPRECATED upeaEmail "Use generic-lens or generic-optics with 'email' instead." #-}

instance Core.FromJSON UpdatePrimaryEmailAddress where
  toJSON UpdatePrimaryEmailAddress {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("Email" Core..= email)
          ]
      )

instance Core.AWSRequest UpdatePrimaryEmailAddress where
  type
    Rs UpdatePrimaryEmailAddress =
      UpdatePrimaryEmailAddressResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkMailService.UpdatePrimaryEmailAddress")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePrimaryEmailAddressResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePrimaryEmailAddressResponse' smart constructor.
newtype UpdatePrimaryEmailAddressResponse = UpdatePrimaryEmailAddressResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePrimaryEmailAddressResponse' value with any optional fields omitted.
mkUpdatePrimaryEmailAddressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdatePrimaryEmailAddressResponse
mkUpdatePrimaryEmailAddressResponse responseStatus =
  UpdatePrimaryEmailAddressResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upearrsResponseStatus :: Lens.Lens' UpdatePrimaryEmailAddressResponse Core.Int
upearrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED upearrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
