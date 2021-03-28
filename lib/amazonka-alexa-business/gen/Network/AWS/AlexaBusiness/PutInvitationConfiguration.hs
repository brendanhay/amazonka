{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutInvitationConfiguration (..)
    , mkPutInvitationConfiguration
    -- ** Request lenses
    , picOrganizationName
    , picContactEmail
    , picPrivateSkillIds

    -- * Destructuring the response
    , PutInvitationConfigurationResponse (..)
    , mkPutInvitationConfigurationResponse
    -- ** Response lenses
    , picrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutInvitationConfiguration' smart constructor.
data PutInvitationConfiguration = PutInvitationConfiguration'
  { organizationName :: Types.OrganizationName
    -- ^ The name of the organization sending the enrollment invite to a user.
  , contactEmail :: Core.Maybe Types.Email
    -- ^ The email ID of the organization or individual contact that the enrolled user can use. 
  , privateSkillIds :: Core.Maybe [Types.SkillId]
    -- ^ The list of private skill IDs that you want to recommend to the user to enable in the invitation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInvitationConfiguration' value with any optional fields omitted.
mkPutInvitationConfiguration
    :: Types.OrganizationName -- ^ 'organizationName'
    -> PutInvitationConfiguration
mkPutInvitationConfiguration organizationName
  = PutInvitationConfiguration'{organizationName,
                                contactEmail = Core.Nothing, privateSkillIds = Core.Nothing}

-- | The name of the organization sending the enrollment invite to a user.
--
-- /Note:/ Consider using 'organizationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picOrganizationName :: Lens.Lens' PutInvitationConfiguration Types.OrganizationName
picOrganizationName = Lens.field @"organizationName"
{-# INLINEABLE picOrganizationName #-}
{-# DEPRECATED organizationName "Use generic-lens or generic-optics with 'organizationName' instead"  #-}

-- | The email ID of the organization or individual contact that the enrolled user can use. 
--
-- /Note:/ Consider using 'contactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picContactEmail :: Lens.Lens' PutInvitationConfiguration (Core.Maybe Types.Email)
picContactEmail = Lens.field @"contactEmail"
{-# INLINEABLE picContactEmail #-}
{-# DEPRECATED contactEmail "Use generic-lens or generic-optics with 'contactEmail' instead"  #-}

-- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
--
-- /Note:/ Consider using 'privateSkillIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picPrivateSkillIds :: Lens.Lens' PutInvitationConfiguration (Core.Maybe [Types.SkillId])
picPrivateSkillIds = Lens.field @"privateSkillIds"
{-# INLINEABLE picPrivateSkillIds #-}
{-# DEPRECATED privateSkillIds "Use generic-lens or generic-optics with 'privateSkillIds' instead"  #-}

instance Core.ToQuery PutInvitationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutInvitationConfiguration where
        toHeaders PutInvitationConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.PutInvitationConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutInvitationConfiguration where
        toJSON PutInvitationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationName" Core..= organizationName),
                  ("ContactEmail" Core..=) Core.<$> contactEmail,
                  ("PrivateSkillIds" Core..=) Core.<$> privateSkillIds])

instance Core.AWSRequest PutInvitationConfiguration where
        type Rs PutInvitationConfiguration =
             PutInvitationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutInvitationConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutInvitationConfigurationResponse' smart constructor.
newtype PutInvitationConfigurationResponse = PutInvitationConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutInvitationConfigurationResponse' value with any optional fields omitted.
mkPutInvitationConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutInvitationConfigurationResponse
mkPutInvitationConfigurationResponse responseStatus
  = PutInvitationConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picrrsResponseStatus :: Lens.Lens' PutInvitationConfigurationResponse Core.Int
picrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE picrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
