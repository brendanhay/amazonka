{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetInvitationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configured values for the user enrollment invitation email template.
module Network.AWS.AlexaBusiness.GetInvitationConfiguration
    (
    -- * Creating a request
      GetInvitationConfiguration (..)
    , mkGetInvitationConfiguration

    -- * Destructuring the response
    , GetInvitationConfigurationResponse (..)
    , mkGetInvitationConfigurationResponse
    -- ** Response lenses
    , gicrrsContactEmail
    , gicrrsOrganizationName
    , gicrrsPrivateSkillIds
    , gicrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInvitationConfiguration' smart constructor.
data GetInvitationConfiguration = GetInvitationConfiguration'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInvitationConfiguration' value with any optional fields omitted.
mkGetInvitationConfiguration
    :: GetInvitationConfiguration
mkGetInvitationConfiguration = GetInvitationConfiguration'

instance Core.ToQuery GetInvitationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInvitationConfiguration where
        toHeaders GetInvitationConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.GetInvitationConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInvitationConfiguration where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetInvitationConfiguration where
        type Rs GetInvitationConfiguration =
             GetInvitationConfigurationResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInvitationConfigurationResponse' Core.<$>
                   (x Core..:? "ContactEmail") Core.<*> x Core..:? "OrganizationName"
                     Core.<*> x Core..:? "PrivateSkillIds"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInvitationConfigurationResponse' smart constructor.
data GetInvitationConfigurationResponse = GetInvitationConfigurationResponse'
  { contactEmail :: Core.Maybe Types.Email
    -- ^ The email ID of the organization or individual contact that the enrolled user can use. 
  , organizationName :: Core.Maybe Types.OrganizationName
    -- ^ The name of the organization sending the enrollment invite to a user.
  , privateSkillIds :: Core.Maybe [Types.SkillId]
    -- ^ The list of private skill IDs that you want to recommend to the user to enable in the invitation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInvitationConfigurationResponse' value with any optional fields omitted.
mkGetInvitationConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInvitationConfigurationResponse
mkGetInvitationConfigurationResponse responseStatus
  = GetInvitationConfigurationResponse'{contactEmail = Core.Nothing,
                                        organizationName = Core.Nothing,
                                        privateSkillIds = Core.Nothing, responseStatus}

-- | The email ID of the organization or individual contact that the enrolled user can use. 
--
-- /Note:/ Consider using 'contactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsContactEmail :: Lens.Lens' GetInvitationConfigurationResponse (Core.Maybe Types.Email)
gicrrsContactEmail = Lens.field @"contactEmail"
{-# INLINEABLE gicrrsContactEmail #-}
{-# DEPRECATED contactEmail "Use generic-lens or generic-optics with 'contactEmail' instead"  #-}

-- | The name of the organization sending the enrollment invite to a user.
--
-- /Note:/ Consider using 'organizationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsOrganizationName :: Lens.Lens' GetInvitationConfigurationResponse (Core.Maybe Types.OrganizationName)
gicrrsOrganizationName = Lens.field @"organizationName"
{-# INLINEABLE gicrrsOrganizationName #-}
{-# DEPRECATED organizationName "Use generic-lens or generic-optics with 'organizationName' instead"  #-}

-- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
--
-- /Note:/ Consider using 'privateSkillIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsPrivateSkillIds :: Lens.Lens' GetInvitationConfigurationResponse (Core.Maybe [Types.SkillId])
gicrrsPrivateSkillIds = Lens.field @"privateSkillIds"
{-# INLINEABLE gicrrsPrivateSkillIds #-}
{-# DEPRECATED privateSkillIds "Use generic-lens or generic-optics with 'privateSkillIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsResponseStatus :: Lens.Lens' GetInvitationConfigurationResponse Core.Int
gicrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gicrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
