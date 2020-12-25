{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates identity provider information for a user pool.
module Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
  ( -- * Creating a request
    UpdateIdentityProvider (..),
    mkUpdateIdentityProvider,

    -- ** Request lenses
    uipUserPoolId,
    uipProviderName,
    uipAttributeMapping,
    uipIdpIdentifiers,
    uipProviderDetails,

    -- * Destructuring the response
    UpdateIdentityProviderResponse (..),
    mkUpdateIdentityProviderResponse,

    -- ** Response lenses
    uiprrsIdentityProvider,
    uiprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolId,
    -- | The identity provider name.
    providerName :: Types.ProviderNameType,
    -- | The identity provider attribute mapping to be changed.
    attributeMapping :: Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType),
    -- | A list of identity provider identifiers.
    idpIdentifiers :: Core.Maybe [Types.IdpIdentifierType],
    -- | The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
    providerDetails :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIdentityProvider' value with any optional fields omitted.
mkUpdateIdentityProvider ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'providerName'
  Types.ProviderNameType ->
  UpdateIdentityProvider
mkUpdateIdentityProvider userPoolId providerName =
  UpdateIdentityProvider'
    { userPoolId,
      providerName,
      attributeMapping = Core.Nothing,
      idpIdentifiers = Core.Nothing,
      providerDetails = Core.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipUserPoolId :: Lens.Lens' UpdateIdentityProvider Types.UserPoolId
uipUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED uipUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipProviderName :: Lens.Lens' UpdateIdentityProvider Types.ProviderNameType
uipProviderName = Lens.field @"providerName"
{-# DEPRECATED uipProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The identity provider attribute mapping to be changed.
--
-- /Note:/ Consider using 'attributeMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipAttributeMapping :: Lens.Lens' UpdateIdentityProvider (Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType))
uipAttributeMapping = Lens.field @"attributeMapping"
{-# DEPRECATED uipAttributeMapping "Use generic-lens or generic-optics with 'attributeMapping' instead." #-}

-- | A list of identity provider identifiers.
--
-- /Note:/ Consider using 'idpIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdpIdentifiers :: Lens.Lens' UpdateIdentityProvider (Core.Maybe [Types.IdpIdentifierType])
uipIdpIdentifiers = Lens.field @"idpIdentifiers"
{-# DEPRECATED uipIdpIdentifiers "Use generic-lens or generic-optics with 'idpIdentifiers' instead." #-}

-- | The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
--
-- /Note:/ Consider using 'providerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipProviderDetails :: Lens.Lens' UpdateIdentityProvider (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
uipProviderDetails = Lens.field @"providerDetails"
{-# DEPRECATED uipProviderDetails "Use generic-lens or generic-optics with 'providerDetails' instead." #-}

instance Core.FromJSON UpdateIdentityProvider where
  toJSON UpdateIdentityProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName),
            ("AttributeMapping" Core..=) Core.<$> attributeMapping,
            ("IdpIdentifiers" Core..=) Core.<$> idpIdentifiers,
            ("ProviderDetails" Core..=) Core.<$> providerDetails
          ]
      )

instance Core.AWSRequest UpdateIdentityProvider where
  type Rs UpdateIdentityProvider = UpdateIdentityProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.UpdateIdentityProvider"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIdentityProviderResponse'
            Core.<$> (x Core..: "IdentityProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { -- | The identity provider object.
    identityProvider :: Types.IdentityProviderType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateIdentityProviderResponse' value with any optional fields omitted.
mkUpdateIdentityProviderResponse ::
  -- | 'identityProvider'
  Types.IdentityProviderType ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateIdentityProviderResponse
mkUpdateIdentityProviderResponse identityProvider responseStatus =
  UpdateIdentityProviderResponse' {identityProvider, responseStatus}

-- | The identity provider object.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprrsIdentityProvider :: Lens.Lens' UpdateIdentityProviderResponse Types.IdentityProviderType
uiprrsIdentityProvider = Lens.field @"identityProvider"
{-# DEPRECATED uiprrsIdentityProvider "Use generic-lens or generic-optics with 'identityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprrsResponseStatus :: Lens.Lens' UpdateIdentityProviderResponse Core.Int
uiprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uiprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
