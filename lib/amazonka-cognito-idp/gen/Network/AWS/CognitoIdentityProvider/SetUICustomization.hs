{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetUICustomization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the UI customization information for a user pool's built-in app UI.
--
-- You can specify app UI customization settings for a single client (with a specific @clientId@ ) or for all clients (by setting the @clientId@ to @ALL@ ). If you specify @ALL@ , the default configuration will be used for every client that has no UI customization set previously. If you specify UI customization settings for a particular client, it will no longer fall back to the @ALL@ configuration. 
module Network.AWS.CognitoIdentityProvider.SetUICustomization
    (
    -- * Creating a request
      SetUICustomization (..)
    , mkSetUICustomization
    -- ** Request lenses
    , suicUserPoolId
    , suicCSS
    , suicClientId
    , suicImageFile

    -- * Destructuring the response
    , SetUICustomizationResponse (..)
    , mkSetUICustomizationResponse
    -- ** Response lenses
    , suicrrsUICustomization
    , suicrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetUICustomization' smart constructor.
data SetUICustomization = SetUICustomization'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool.
  , css :: Core.Maybe Types.CSS
    -- ^ The CSS values in the UI customization.
  , clientId :: Core.Maybe Types.ClientId
    -- ^ The client ID for the client app.
  , imageFile :: Core.Maybe Core.Base64
    -- ^ The uploaded logo image for the UI customization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetUICustomization' value with any optional fields omitted.
mkSetUICustomization
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> SetUICustomization
mkSetUICustomization userPoolId
  = SetUICustomization'{userPoolId, css = Core.Nothing,
                        clientId = Core.Nothing, imageFile = Core.Nothing}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suicUserPoolId :: Lens.Lens' SetUICustomization Types.UserPoolId
suicUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE suicUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The CSS values in the UI customization.
--
-- /Note:/ Consider using 'css' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suicCSS :: Lens.Lens' SetUICustomization (Core.Maybe Types.CSS)
suicCSS = Lens.field @"css"
{-# INLINEABLE suicCSS #-}
{-# DEPRECATED css "Use generic-lens or generic-optics with 'css' instead"  #-}

-- | The client ID for the client app.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suicClientId :: Lens.Lens' SetUICustomization (Core.Maybe Types.ClientId)
suicClientId = Lens.field @"clientId"
{-# INLINEABLE suicClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The uploaded logo image for the UI customization.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'imageFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suicImageFile :: Lens.Lens' SetUICustomization (Core.Maybe Core.Base64)
suicImageFile = Lens.field @"imageFile"
{-# INLINEABLE suicImageFile #-}
{-# DEPRECATED imageFile "Use generic-lens or generic-optics with 'imageFile' instead"  #-}

instance Core.ToQuery SetUICustomization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetUICustomization where
        toHeaders SetUICustomization{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.SetUICustomization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetUICustomization where
        toJSON SetUICustomization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  ("CSS" Core..=) Core.<$> css,
                  ("ClientId" Core..=) Core.<$> clientId,
                  ("ImageFile" Core..=) Core.<$> imageFile])

instance Core.AWSRequest SetUICustomization where
        type Rs SetUICustomization = SetUICustomizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SetUICustomizationResponse' Core.<$>
                   (x Core..: "UICustomization") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetUICustomizationResponse' smart constructor.
data SetUICustomizationResponse = SetUICustomizationResponse'
  { uICustomization :: Types.UICustomizationType
    -- ^ The UI customization information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SetUICustomizationResponse' value with any optional fields omitted.
mkSetUICustomizationResponse
    :: Types.UICustomizationType -- ^ 'uICustomization'
    -> Core.Int -- ^ 'responseStatus'
    -> SetUICustomizationResponse
mkSetUICustomizationResponse uICustomization responseStatus
  = SetUICustomizationResponse'{uICustomization, responseStatus}

-- | The UI customization information.
--
-- /Note:/ Consider using 'uICustomization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suicrrsUICustomization :: Lens.Lens' SetUICustomizationResponse Types.UICustomizationType
suicrrsUICustomization = Lens.field @"uICustomization"
{-# INLINEABLE suicrrsUICustomization #-}
{-# DEPRECATED uICustomization "Use generic-lens or generic-optics with 'uICustomization' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suicrrsResponseStatus :: Lens.Lens' SetUICustomizationResponse Core.Int
suicrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE suicrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
