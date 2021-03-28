{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserPoolMfaConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user pool multi-factor authentication (MFA) configuration.
module Network.AWS.CognitoIdentityProvider.GetUserPoolMfaConfig
    (
    -- * Creating a request
      GetUserPoolMfaConfig (..)
    , mkGetUserPoolMfaConfig
    -- ** Request lenses
    , gupmcUserPoolId

    -- * Destructuring the response
    , GetUserPoolMfaConfigResponse (..)
    , mkGetUserPoolMfaConfigResponse
    -- ** Response lenses
    , gupmcrrsMfaConfiguration
    , gupmcrrsSmsMfaConfiguration
    , gupmcrrsSoftwareTokenMfaConfiguration
    , gupmcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUserPoolMfaConfig' smart constructor.
newtype GetUserPoolMfaConfig = GetUserPoolMfaConfig'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserPoolMfaConfig' value with any optional fields omitted.
mkGetUserPoolMfaConfig
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> GetUserPoolMfaConfig
mkGetUserPoolMfaConfig userPoolId
  = GetUserPoolMfaConfig'{userPoolId}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcUserPoolId :: Lens.Lens' GetUserPoolMfaConfig Types.UserPoolId
gupmcUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE gupmcUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.ToQuery GetUserPoolMfaConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetUserPoolMfaConfig where
        toHeaders GetUserPoolMfaConfig{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.GetUserPoolMfaConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetUserPoolMfaConfig where
        toJSON GetUserPoolMfaConfig{..}
          = Core.object
              (Core.catMaybes [Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest GetUserPoolMfaConfig where
        type Rs GetUserPoolMfaConfig = GetUserPoolMfaConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetUserPoolMfaConfigResponse' Core.<$>
                   (x Core..:? "MfaConfiguration") Core.<*>
                     x Core..:? "SmsMfaConfiguration"
                     Core.<*> x Core..:? "SoftwareTokenMfaConfiguration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetUserPoolMfaConfigResponse' smart constructor.
data GetUserPoolMfaConfigResponse = GetUserPoolMfaConfigResponse'
  { mfaConfiguration :: Core.Maybe Types.UserPoolMfaType
    -- ^ The multi-factor (MFA) configuration. Valid values include:
--
--
--     * @OFF@ MFA will not be used for any users.
--
--
--     * @ON@ MFA is required for all users to sign in.
--
--
--     * @OPTIONAL@ MFA will be required only for individual users who have an MFA factor enabled.
--
--
  , smsMfaConfiguration :: Core.Maybe Types.SmsMfaConfigType
    -- ^ The SMS text message multi-factor (MFA) configuration.
  , softwareTokenMfaConfiguration :: Core.Maybe Types.SoftwareTokenMfaConfigType
    -- ^ The software token multi-factor (MFA) configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserPoolMfaConfigResponse' value with any optional fields omitted.
mkGetUserPoolMfaConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetUserPoolMfaConfigResponse
mkGetUserPoolMfaConfigResponse responseStatus
  = GetUserPoolMfaConfigResponse'{mfaConfiguration = Core.Nothing,
                                  smsMfaConfiguration = Core.Nothing,
                                  softwareTokenMfaConfiguration = Core.Nothing, responseStatus}

-- | The multi-factor (MFA) configuration. Valid values include:
--
--
--     * @OFF@ MFA will not be used for any users.
--
--
--     * @ON@ MFA is required for all users to sign in.
--
--
--     * @OPTIONAL@ MFA will be required only for individual users who have an MFA factor enabled.
--
--
--
-- /Note:/ Consider using 'mfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrrsMfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Core.Maybe Types.UserPoolMfaType)
gupmcrrsMfaConfiguration = Lens.field @"mfaConfiguration"
{-# INLINEABLE gupmcrrsMfaConfiguration #-}
{-# DEPRECATED mfaConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead"  #-}

-- | The SMS text message multi-factor (MFA) configuration.
--
-- /Note:/ Consider using 'smsMfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrrsSmsMfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Core.Maybe Types.SmsMfaConfigType)
gupmcrrsSmsMfaConfiguration = Lens.field @"smsMfaConfiguration"
{-# INLINEABLE gupmcrrsSmsMfaConfiguration #-}
{-# DEPRECATED smsMfaConfiguration "Use generic-lens or generic-optics with 'smsMfaConfiguration' instead"  #-}

-- | The software token multi-factor (MFA) configuration.
--
-- /Note:/ Consider using 'softwareTokenMfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrrsSoftwareTokenMfaConfiguration :: Lens.Lens' GetUserPoolMfaConfigResponse (Core.Maybe Types.SoftwareTokenMfaConfigType)
gupmcrrsSoftwareTokenMfaConfiguration = Lens.field @"softwareTokenMfaConfiguration"
{-# INLINEABLE gupmcrrsSoftwareTokenMfaConfiguration #-}
{-# DEPRECATED softwareTokenMfaConfiguration "Use generic-lens or generic-optics with 'softwareTokenMfaConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrrsResponseStatus :: Lens.Lens' GetUserPoolMfaConfigResponse Core.Int
gupmcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gupmcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
