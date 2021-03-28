{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a network profile by the network profile ARN.
module Network.AWS.AlexaBusiness.UpdateNetworkProfile
    (
    -- * Creating a request
      UpdateNetworkProfile (..)
    , mkUpdateNetworkProfile
    -- ** Request lenses
    , unpNetworkProfileArn
    , unpCertificateAuthorityArn
    , unpCurrentPassword
    , unpDescription
    , unpNetworkProfileName
    , unpNextPassword
    , unpTrustAnchors

    -- * Destructuring the response
    , UpdateNetworkProfileResponse (..)
    , mkUpdateNetworkProfileResponse
    -- ** Response lenses
    , unprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { networkProfileArn :: Types.Arn
    -- ^ The ARN of the network profile associated with a device.
  , certificateAuthorityArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices. 
  , currentPassword :: Core.Maybe Types.CurrentWiFiPassword
    -- ^ The current password of the Wi-Fi network.
  , description :: Core.Maybe Types.NetworkProfileDescription
    -- ^ Detailed information about a device's network profile.
  , networkProfileName :: Core.Maybe Types.NetworkProfileName
    -- ^ The name of the network profile associated with a device.
  , nextPassword :: Core.Maybe Types.NextWiFiPassword
    -- ^ The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword. 
  , trustAnchors :: Core.Maybe (Core.NonEmpty Types.TrustAnchor)
    -- ^ The root certificate(s) of your authentication server that will be installed on your devices and used to trust your authentication server during EAP negotiation. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNetworkProfile' value with any optional fields omitted.
mkUpdateNetworkProfile
    :: Types.Arn -- ^ 'networkProfileArn'
    -> UpdateNetworkProfile
mkUpdateNetworkProfile networkProfileArn
  = UpdateNetworkProfile'{networkProfileArn,
                          certificateAuthorityArn = Core.Nothing,
                          currentPassword = Core.Nothing, description = Core.Nothing,
                          networkProfileName = Core.Nothing, nextPassword = Core.Nothing,
                          trustAnchors = Core.Nothing}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpNetworkProfileArn :: Lens.Lens' UpdateNetworkProfile Types.Arn
unpNetworkProfileArn = Lens.field @"networkProfileArn"
{-# INLINEABLE unpNetworkProfileArn #-}
{-# DEPRECATED networkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead"  #-}

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices. 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpCertificateAuthorityArn :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.Arn)
unpCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE unpCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The current password of the Wi-Fi network.
--
-- /Note:/ Consider using 'currentPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpCurrentPassword :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.CurrentWiFiPassword)
unpCurrentPassword = Lens.field @"currentPassword"
{-# INLINEABLE unpCurrentPassword #-}
{-# DEPRECATED currentPassword "Use generic-lens or generic-optics with 'currentPassword' instead"  #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDescription :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.NetworkProfileDescription)
unpDescription = Lens.field @"description"
{-# INLINEABLE unpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpNetworkProfileName :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.NetworkProfileName)
unpNetworkProfileName = Lens.field @"networkProfileName"
{-# INLINEABLE unpNetworkProfileName #-}
{-# DEPRECATED networkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead"  #-}

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword. 
--
-- /Note:/ Consider using 'nextPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpNextPassword :: Lens.Lens' UpdateNetworkProfile (Core.Maybe Types.NextWiFiPassword)
unpNextPassword = Lens.field @"nextPassword"
{-# INLINEABLE unpNextPassword #-}
{-# DEPRECATED nextPassword "Use generic-lens or generic-optics with 'nextPassword' instead"  #-}

-- | The root certificate(s) of your authentication server that will be installed on your devices and used to trust your authentication server during EAP negotiation. 
--
-- /Note:/ Consider using 'trustAnchors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpTrustAnchors :: Lens.Lens' UpdateNetworkProfile (Core.Maybe (Core.NonEmpty Types.TrustAnchor))
unpTrustAnchors = Lens.field @"trustAnchors"
{-# INLINEABLE unpTrustAnchors #-}
{-# DEPRECATED trustAnchors "Use generic-lens or generic-optics with 'trustAnchors' instead"  #-}

instance Core.ToQuery UpdateNetworkProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateNetworkProfile where
        toHeaders UpdateNetworkProfile{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.UpdateNetworkProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateNetworkProfile where
        toJSON UpdateNetworkProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NetworkProfileArn" Core..= networkProfileArn),
                  ("CertificateAuthorityArn" Core..=) Core.<$>
                    certificateAuthorityArn,
                  ("CurrentPassword" Core..=) Core.<$> currentPassword,
                  ("Description" Core..=) Core.<$> description,
                  ("NetworkProfileName" Core..=) Core.<$> networkProfileName,
                  ("NextPassword" Core..=) Core.<$> nextPassword,
                  ("TrustAnchors" Core..=) Core.<$> trustAnchors])

instance Core.AWSRequest UpdateNetworkProfile where
        type Rs UpdateNetworkProfile = UpdateNetworkProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateNetworkProfileResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateNetworkProfileResponse' smart constructor.
newtype UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNetworkProfileResponse' value with any optional fields omitted.
mkUpdateNetworkProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateNetworkProfileResponse
mkUpdateNetworkProfileResponse responseStatus
  = UpdateNetworkProfileResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unprrsResponseStatus :: Lens.Lens' UpdateNetworkProfileResponse Core.Int
unprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE unprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
