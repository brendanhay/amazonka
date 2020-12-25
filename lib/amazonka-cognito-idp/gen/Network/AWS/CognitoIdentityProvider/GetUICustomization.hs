{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetUICustomization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the UI Customization information for a particular app client's app UI, if there is something set. If nothing is set for the particular client, but there is an existing pool level customization (app @clientId@ will be @ALL@ ), then that is returned. If nothing is present, then an empty shape is returned.
module Network.AWS.CognitoIdentityProvider.GetUICustomization
  ( -- * Creating a request
    GetUICustomization (..),
    mkGetUICustomization,

    -- ** Request lenses
    guicUserPoolId,
    guicClientId,

    -- * Destructuring the response
    GetUICustomizationResponse (..),
    mkGetUICustomizationResponse,

    -- ** Response lenses
    guicrrsUICustomization,
    guicrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUICustomization' smart constructor.
data GetUICustomization = GetUICustomization'
  { -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId,
    -- | The client ID for the client app.
    clientId :: Core.Maybe Types.ClientId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUICustomization' value with any optional fields omitted.
mkGetUICustomization ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  GetUICustomization
mkGetUICustomization userPoolId =
  GetUICustomization' {userPoolId, clientId = Core.Nothing}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicUserPoolId :: Lens.Lens' GetUICustomization Types.UserPoolId
guicUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED guicUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The client ID for the client app.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicClientId :: Lens.Lens' GetUICustomization (Core.Maybe Types.ClientId)
guicClientId = Lens.field @"clientId"
{-# DEPRECATED guicClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

instance Core.FromJSON GetUICustomization where
  toJSON GetUICustomization {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            ("ClientId" Core..=) Core.<$> clientId
          ]
      )

instance Core.AWSRequest GetUICustomization where
  type Rs GetUICustomization = GetUICustomizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.GetUICustomization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUICustomizationResponse'
            Core.<$> (x Core..: "UICustomization")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetUICustomizationResponse' smart constructor.
data GetUICustomizationResponse = GetUICustomizationResponse'
  { -- | The UI customization information.
    uICustomization :: Types.UICustomizationType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetUICustomizationResponse' value with any optional fields omitted.
mkGetUICustomizationResponse ::
  -- | 'uICustomization'
  Types.UICustomizationType ->
  -- | 'responseStatus'
  Core.Int ->
  GetUICustomizationResponse
mkGetUICustomizationResponse uICustomization responseStatus =
  GetUICustomizationResponse' {uICustomization, responseStatus}

-- | The UI customization information.
--
-- /Note:/ Consider using 'uICustomization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicrrsUICustomization :: Lens.Lens' GetUICustomizationResponse Types.UICustomizationType
guicrrsUICustomization = Lens.field @"uICustomization"
{-# DEPRECATED guicrrsUICustomization "Use generic-lens or generic-optics with 'uICustomization' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicrrsResponseStatus :: Lens.Lens' GetUICustomizationResponse Core.Int
guicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED guicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
