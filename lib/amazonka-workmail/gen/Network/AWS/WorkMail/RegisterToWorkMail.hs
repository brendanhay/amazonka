{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.RegisterToWorkMail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an existing and disabled user, group, or resource for Amazon WorkMail use by associating a mailbox and calendaring capabilities. It performs no change if the user, group, or resource is enabled and fails if the user, group, or resource is deleted. This operation results in the accumulation of costs. For more information, see <https://aws.amazon.com/workmail/pricing Pricing> . The equivalent console functionality for this operation is /Enable/ .
--
-- Users can either be created by calling the 'CreateUser' API operation or they can be synchronized from your directory. For more information, see 'DeregisterFromWorkMail' .
module Network.AWS.WorkMail.RegisterToWorkMail
  ( -- * Creating a request
    RegisterToWorkMail (..),
    mkRegisterToWorkMail,

    -- ** Request lenses
    rtwmOrganizationId,
    rtwmEntityId,
    rtwmEmail,

    -- * Destructuring the response
    RegisterToWorkMailResponse (..),
    mkRegisterToWorkMailResponse,

    -- ** Response lenses
    rtwmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkRegisterToWorkMail' smart constructor.
data RegisterToWorkMail = RegisterToWorkMail'
  { -- | The identifier for the organization under which the user, group, or resource exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier for the user, group, or resource to be updated.
    entityId :: Types.WorkMailIdentifier,
    -- | The email for the user, group, or resource to be updated.
    email :: Types.EmailAddress
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterToWorkMail' value with any optional fields omitted.
mkRegisterToWorkMail ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'entityId'
  Types.WorkMailIdentifier ->
  -- | 'email'
  Types.EmailAddress ->
  RegisterToWorkMail
mkRegisterToWorkMail organizationId entityId email =
  RegisterToWorkMail' {organizationId, entityId, email}

-- | The identifier for the organization under which the user, group, or resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmOrganizationId :: Lens.Lens' RegisterToWorkMail Types.OrganizationId
rtwmOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED rtwmOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the user, group, or resource to be updated.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmEntityId :: Lens.Lens' RegisterToWorkMail Types.WorkMailIdentifier
rtwmEntityId = Lens.field @"entityId"
{-# DEPRECATED rtwmEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The email for the user, group, or resource to be updated.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmEmail :: Lens.Lens' RegisterToWorkMail Types.EmailAddress
rtwmEmail = Lens.field @"email"
{-# DEPRECATED rtwmEmail "Use generic-lens or generic-optics with 'email' instead." #-}

instance Core.FromJSON RegisterToWorkMail where
  toJSON RegisterToWorkMail {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("Email" Core..= email)
          ]
      )

instance Core.AWSRequest RegisterToWorkMail where
  type Rs RegisterToWorkMail = RegisterToWorkMailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.RegisterToWorkMail")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterToWorkMailResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterToWorkMailResponse' smart constructor.
newtype RegisterToWorkMailResponse = RegisterToWorkMailResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterToWorkMailResponse' value with any optional fields omitted.
mkRegisterToWorkMailResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterToWorkMailResponse
mkRegisterToWorkMailResponse responseStatus =
  RegisterToWorkMailResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmrrsResponseStatus :: Lens.Lens' RegisterToWorkMailResponse Core.Int
rtwmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtwmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
