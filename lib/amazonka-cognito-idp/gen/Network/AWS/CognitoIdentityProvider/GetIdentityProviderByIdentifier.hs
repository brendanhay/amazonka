{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified identity provider.
module Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
    (
    -- * Creating a request
      GetIdentityProviderByIdentifier (..)
    , mkGetIdentityProviderByIdentifier
    -- ** Request lenses
    , gipbiUserPoolId
    , gipbiIdpIdentifier

    -- * Destructuring the response
    , GetIdentityProviderByIdentifierResponse (..)
    , mkGetIdentityProviderByIdentifierResponse
    -- ** Response lenses
    , gipbirrsIdentityProvider
    , gipbirrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIdentityProviderByIdentifier' smart constructor.
data GetIdentityProviderByIdentifier = GetIdentityProviderByIdentifier'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , idpIdentifier :: Types.IdpIdentifier
    -- ^ The identity provider ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityProviderByIdentifier' value with any optional fields omitted.
mkGetIdentityProviderByIdentifier
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.IdpIdentifier -- ^ 'idpIdentifier'
    -> GetIdentityProviderByIdentifier
mkGetIdentityProviderByIdentifier userPoolId idpIdentifier
  = GetIdentityProviderByIdentifier'{userPoolId, idpIdentifier}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbiUserPoolId :: Lens.Lens' GetIdentityProviderByIdentifier Types.UserPoolId
gipbiUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE gipbiUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The identity provider ID.
--
-- /Note:/ Consider using 'idpIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbiIdpIdentifier :: Lens.Lens' GetIdentityProviderByIdentifier Types.IdpIdentifier
gipbiIdpIdentifier = Lens.field @"idpIdentifier"
{-# INLINEABLE gipbiIdpIdentifier #-}
{-# DEPRECATED idpIdentifier "Use generic-lens or generic-optics with 'idpIdentifier' instead"  #-}

instance Core.ToQuery GetIdentityProviderByIdentifier where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetIdentityProviderByIdentifier where
        toHeaders GetIdentityProviderByIdentifier{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.GetIdentityProviderByIdentifier")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetIdentityProviderByIdentifier where
        toJSON GetIdentityProviderByIdentifier{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("IdpIdentifier" Core..= idpIdentifier)])

instance Core.AWSRequest GetIdentityProviderByIdentifier where
        type Rs GetIdentityProviderByIdentifier =
             GetIdentityProviderByIdentifierResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetIdentityProviderByIdentifierResponse' Core.<$>
                   (x Core..: "IdentityProvider") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetIdentityProviderByIdentifierResponse' smart constructor.
data GetIdentityProviderByIdentifierResponse = GetIdentityProviderByIdentifierResponse'
  { identityProvider :: Types.IdentityProviderType
    -- ^ The identity provider object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetIdentityProviderByIdentifierResponse' value with any optional fields omitted.
mkGetIdentityProviderByIdentifierResponse
    :: Types.IdentityProviderType -- ^ 'identityProvider'
    -> Core.Int -- ^ 'responseStatus'
    -> GetIdentityProviderByIdentifierResponse
mkGetIdentityProviderByIdentifierResponse identityProvider
  responseStatus
  = GetIdentityProviderByIdentifierResponse'{identityProvider,
                                             responseStatus}

-- | The identity provider object.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbirrsIdentityProvider :: Lens.Lens' GetIdentityProviderByIdentifierResponse Types.IdentityProviderType
gipbirrsIdentityProvider = Lens.field @"identityProvider"
{-# INLINEABLE gipbirrsIdentityProvider #-}
{-# DEPRECATED identityProvider "Use generic-lens or generic-optics with 'identityProvider' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipbirrsResponseStatus :: Lens.Lens' GetIdentityProviderByIdentifierResponse Core.Int
gipbirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gipbirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
