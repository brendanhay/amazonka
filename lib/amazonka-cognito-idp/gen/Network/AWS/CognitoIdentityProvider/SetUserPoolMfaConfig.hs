{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserPoolMfaConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user pool multi-factor authentication (MFA) configuration.
module Network.AWS.CognitoIdentityProvider.SetUserPoolMfaConfig
    (
    -- * Creating a request
      SetUserPoolMfaConfig (..)
    , mkSetUserPoolMfaConfig
    -- ** Request lenses
    , supmcUserPoolId
    , supmcMfaConfiguration
    , supmcSmsMfaConfiguration
    , supmcSoftwareTokenMfaConfiguration

    -- * Destructuring the response
    , SetUserPoolMfaConfigResponse (..)
    , mkSetUserPoolMfaConfigResponse
    -- ** Response lenses
    , supmcrrsMfaConfiguration
    , supmcrrsSmsMfaConfiguration
    , supmcrrsSoftwareTokenMfaConfiguration
    , supmcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetUserPoolMfaConfig' smart constructor.
data SetUserPoolMfaConfig = SetUserPoolMfaConfig'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID.
  , mfaConfiguration :: Core.Maybe Types.UserPoolMfaType
    -- ^ The MFA configuration. Valid values include:
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
    -- ^ The SMS text message MFA configuration.
  , softwareTokenMfaConfiguration :: Core.Maybe Types.SoftwareTokenMfaConfigType
    -- ^ The software token MFA configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetUserPoolMfaConfig' value with any optional fields omitted.
mkSetUserPoolMfaConfig
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> SetUserPoolMfaConfig
mkSetUserPoolMfaConfig userPoolId
  = SetUserPoolMfaConfig'{userPoolId,
                          mfaConfiguration = Core.Nothing,
                          smsMfaConfiguration = Core.Nothing,
                          softwareTokenMfaConfiguration = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcUserPoolId :: Lens.Lens' SetUserPoolMfaConfig Types.UserPoolIdType
supmcUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE supmcUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The MFA configuration. Valid values include:
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
supmcMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Core.Maybe Types.UserPoolMfaType)
supmcMfaConfiguration = Lens.field @"mfaConfiguration"
{-# INLINEABLE supmcMfaConfiguration #-}
{-# DEPRECATED mfaConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead"  #-}

-- | The SMS text message MFA configuration.
--
-- /Note:/ Consider using 'smsMfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcSmsMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Core.Maybe Types.SmsMfaConfigType)
supmcSmsMfaConfiguration = Lens.field @"smsMfaConfiguration"
{-# INLINEABLE supmcSmsMfaConfiguration #-}
{-# DEPRECATED smsMfaConfiguration "Use generic-lens or generic-optics with 'smsMfaConfiguration' instead"  #-}

-- | The software token MFA configuration.
--
-- /Note:/ Consider using 'softwareTokenMfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcSoftwareTokenMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfig (Core.Maybe Types.SoftwareTokenMfaConfigType)
supmcSoftwareTokenMfaConfiguration = Lens.field @"softwareTokenMfaConfiguration"
{-# INLINEABLE supmcSoftwareTokenMfaConfiguration #-}
{-# DEPRECATED softwareTokenMfaConfiguration "Use generic-lens or generic-optics with 'softwareTokenMfaConfiguration' instead"  #-}

instance Core.ToQuery SetUserPoolMfaConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetUserPoolMfaConfig where
        toHeaders SetUserPoolMfaConfig{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.SetUserPoolMfaConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetUserPoolMfaConfig where
        toJSON SetUserPoolMfaConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  ("MfaConfiguration" Core..=) Core.<$> mfaConfiguration,
                  ("SmsMfaConfiguration" Core..=) Core.<$> smsMfaConfiguration,
                  ("SoftwareTokenMfaConfiguration" Core..=) Core.<$>
                    softwareTokenMfaConfiguration])

instance Core.AWSRequest SetUserPoolMfaConfig where
        type Rs SetUserPoolMfaConfig = SetUserPoolMfaConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SetUserPoolMfaConfigResponse' Core.<$>
                   (x Core..:? "MfaConfiguration") Core.<*>
                     x Core..:? "SmsMfaConfiguration"
                     Core.<*> x Core..:? "SoftwareTokenMfaConfiguration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetUserPoolMfaConfigResponse' smart constructor.
data SetUserPoolMfaConfigResponse = SetUserPoolMfaConfigResponse'
  { mfaConfiguration :: Core.Maybe Types.UserPoolMfaType
    -- ^ The MFA configuration. Valid values include:
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
    -- ^ The SMS text message MFA configuration.
  , softwareTokenMfaConfiguration :: Core.Maybe Types.SoftwareTokenMfaConfigType
    -- ^ The software token MFA configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetUserPoolMfaConfigResponse' value with any optional fields omitted.
mkSetUserPoolMfaConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetUserPoolMfaConfigResponse
mkSetUserPoolMfaConfigResponse responseStatus
  = SetUserPoolMfaConfigResponse'{mfaConfiguration = Core.Nothing,
                                  smsMfaConfiguration = Core.Nothing,
                                  softwareTokenMfaConfiguration = Core.Nothing, responseStatus}

-- | The MFA configuration. Valid values include:
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
supmcrrsMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Core.Maybe Types.UserPoolMfaType)
supmcrrsMfaConfiguration = Lens.field @"mfaConfiguration"
{-# INLINEABLE supmcrrsMfaConfiguration #-}
{-# DEPRECATED mfaConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead"  #-}

-- | The SMS text message MFA configuration.
--
-- /Note:/ Consider using 'smsMfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcrrsSmsMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Core.Maybe Types.SmsMfaConfigType)
supmcrrsSmsMfaConfiguration = Lens.field @"smsMfaConfiguration"
{-# INLINEABLE supmcrrsSmsMfaConfiguration #-}
{-# DEPRECATED smsMfaConfiguration "Use generic-lens or generic-optics with 'smsMfaConfiguration' instead"  #-}

-- | The software token MFA configuration.
--
-- /Note:/ Consider using 'softwareTokenMfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcrrsSoftwareTokenMfaConfiguration :: Lens.Lens' SetUserPoolMfaConfigResponse (Core.Maybe Types.SoftwareTokenMfaConfigType)
supmcrrsSoftwareTokenMfaConfiguration = Lens.field @"softwareTokenMfaConfiguration"
{-# INLINEABLE supmcrrsSoftwareTokenMfaConfiguration #-}
{-# DEPRECATED softwareTokenMfaConfiguration "Use generic-lens or generic-optics with 'softwareTokenMfaConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcrrsResponseStatus :: Lens.Lens' SetUserPoolMfaConfigResponse Core.Int
supmcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE supmcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
