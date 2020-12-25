{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutInvitationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the email template for the user enrollment invitation with the specified attributes.
module Network.AWS.AlexaBusiness.PutInvitationConfiguration
  ( -- * Creating a request
    PutInvitationConfiguration (..),
    mkPutInvitationConfiguration,

    -- ** Request lenses
    picOrganizationName,
    picContactEmail,
    picPrivateSkillIds,

    -- * Destructuring the response
    PutInvitationConfigurationResponse (..),
    mkPutInvitationConfigurationResponse,

    -- ** Response lenses
    picrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutInvitationConfiguration' smart constructor.
data PutInvitationConfiguration = PutInvitationConfiguration'
  { -- | The name of the organization sending the enrollment invite to a user.
    organizationName :: Types.OrganizationName,
    -- | The email ID of the organization or individual contact that the enrolled user can use.
    contactEmail :: Core.Maybe Types.Email,
    -- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
    privateSkillIds :: Core.Maybe [Types.SkillId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInvitationConfiguration' value with any optional fields omitted.
mkPutInvitationConfiguration ::
  -- | 'organizationName'
  Types.OrganizationName ->
  PutInvitationConfiguration
mkPutInvitationConfiguration organizationName =
  PutInvitationConfiguration'
    { organizationName,
      contactEmail = Core.Nothing,
      privateSkillIds = Core.Nothing
    }

-- | The name of the organization sending the enrollment invite to a user.
--
-- /Note:/ Consider using 'organizationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picOrganizationName :: Lens.Lens' PutInvitationConfiguration Types.OrganizationName
picOrganizationName = Lens.field @"organizationName"
{-# DEPRECATED picOrganizationName "Use generic-lens or generic-optics with 'organizationName' instead." #-}

-- | The email ID of the organization or individual contact that the enrolled user can use.
--
-- /Note:/ Consider using 'contactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picContactEmail :: Lens.Lens' PutInvitationConfiguration (Core.Maybe Types.Email)
picContactEmail = Lens.field @"contactEmail"
{-# DEPRECATED picContactEmail "Use generic-lens or generic-optics with 'contactEmail' instead." #-}

-- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
--
-- /Note:/ Consider using 'privateSkillIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picPrivateSkillIds :: Lens.Lens' PutInvitationConfiguration (Core.Maybe [Types.SkillId])
picPrivateSkillIds = Lens.field @"privateSkillIds"
{-# DEPRECATED picPrivateSkillIds "Use generic-lens or generic-optics with 'privateSkillIds' instead." #-}

instance Core.FromJSON PutInvitationConfiguration where
  toJSON PutInvitationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationName" Core..= organizationName),
            ("ContactEmail" Core..=) Core.<$> contactEmail,
            ("PrivateSkillIds" Core..=) Core.<$> privateSkillIds
          ]
      )

instance Core.AWSRequest PutInvitationConfiguration where
  type
    Rs PutInvitationConfiguration =
      PutInvitationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.PutInvitationConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutInvitationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutInvitationConfigurationResponse' smart constructor.
newtype PutInvitationConfigurationResponse = PutInvitationConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutInvitationConfigurationResponse' value with any optional fields omitted.
mkPutInvitationConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutInvitationConfigurationResponse
mkPutInvitationConfigurationResponse responseStatus =
  PutInvitationConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picrrsResponseStatus :: Lens.Lens' PutInvitationConfigurationResponse Core.Int
picrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED picrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
