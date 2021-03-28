{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateIdentityProvider (..)
    , mkUpdateIdentityProvider
    -- ** Request lenses
    , uipUserPoolId
    , uipProviderName
    , uipAttributeMapping
    , uipIdpIdentifiers
    , uipProviderDetails

    -- * Destructuring the response
    , UpdateIdentityProviderResponse (..)
    , mkUpdateIdentityProviderResponse
    -- ** Response lenses
    , uiprrsIdentityProvider
    , uiprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , providerName :: Types.ProviderNameType
    -- ^ The identity provider name.
  , attributeMapping :: Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType)
    -- ^ The identity provider attribute mapping to be changed.
  , idpIdentifiers :: Core.Maybe [Types.IdpIdentifierType]
    -- ^ A list of identity provider identifiers.
  , providerDetails :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIdentityProvider' value with any optional fields omitted.
mkUpdateIdentityProvider
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.ProviderNameType -- ^ 'providerName'
    -> UpdateIdentityProvider
mkUpdateIdentityProvider userPoolId providerName
  = UpdateIdentityProvider'{userPoolId, providerName,
                            attributeMapping = Core.Nothing, idpIdentifiers = Core.Nothing,
                            providerDetails = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipUserPoolId :: Lens.Lens' UpdateIdentityProvider Types.UserPoolId
uipUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE uipUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipProviderName :: Lens.Lens' UpdateIdentityProvider Types.ProviderNameType
uipProviderName = Lens.field @"providerName"
{-# INLINEABLE uipProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

-- | The identity provider attribute mapping to be changed.
--
-- /Note:/ Consider using 'attributeMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipAttributeMapping :: Lens.Lens' UpdateIdentityProvider (Core.Maybe (Core.HashMap Types.AttributeMappingKeyType Types.StringType))
uipAttributeMapping = Lens.field @"attributeMapping"
{-# INLINEABLE uipAttributeMapping #-}
{-# DEPRECATED attributeMapping "Use generic-lens or generic-optics with 'attributeMapping' instead"  #-}

-- | A list of identity provider identifiers.
--
-- /Note:/ Consider using 'idpIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdpIdentifiers :: Lens.Lens' UpdateIdentityProvider (Core.Maybe [Types.IdpIdentifierType])
uipIdpIdentifiers = Lens.field @"idpIdentifiers"
{-# INLINEABLE uipIdpIdentifiers #-}
{-# DEPRECATED idpIdentifiers "Use generic-lens or generic-optics with 'idpIdentifiers' instead"  #-}

-- | The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
--
-- /Note:/ Consider using 'providerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipProviderDetails :: Lens.Lens' UpdateIdentityProvider (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
uipProviderDetails = Lens.field @"providerDetails"
{-# INLINEABLE uipProviderDetails #-}
{-# DEPRECATED providerDetails "Use generic-lens or generic-optics with 'providerDetails' instead"  #-}

instance Core.ToQuery UpdateIdentityProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateIdentityProvider where
        toHeaders UpdateIdentityProvider{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.UpdateIdentityProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateIdentityProvider where
        toJSON UpdateIdentityProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ProviderName" Core..= providerName),
                  ("AttributeMapping" Core..=) Core.<$> attributeMapping,
                  ("IdpIdentifiers" Core..=) Core.<$> idpIdentifiers,
                  ("ProviderDetails" Core..=) Core.<$> providerDetails])

instance Core.AWSRequest UpdateIdentityProvider where
        type Rs UpdateIdentityProvider = UpdateIdentityProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateIdentityProviderResponse' Core.<$>
                   (x Core..: "IdentityProvider") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { identityProvider :: Types.IdentityProviderType
    -- ^ The identity provider object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateIdentityProviderResponse' value with any optional fields omitted.
mkUpdateIdentityProviderResponse
    :: Types.IdentityProviderType -- ^ 'identityProvider'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateIdentityProviderResponse
mkUpdateIdentityProviderResponse identityProvider responseStatus
  = UpdateIdentityProviderResponse'{identityProvider, responseStatus}

-- | The identity provider object.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprrsIdentityProvider :: Lens.Lens' UpdateIdentityProviderResponse Types.IdentityProviderType
uiprrsIdentityProvider = Lens.field @"identityProvider"
{-# INLINEABLE uiprrsIdentityProvider #-}
{-# DEPRECATED identityProvider "Use generic-lens or generic-optics with 'identityProvider' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprrsResponseStatus :: Lens.Lens' UpdateIdentityProviderResponse Core.Int
uiprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uiprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
