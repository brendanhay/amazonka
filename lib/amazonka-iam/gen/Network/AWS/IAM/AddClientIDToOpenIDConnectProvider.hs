{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new client ID (also known as audience) to the list of client IDs already registered for the specified IAM OpenID Connect (OIDC) provider resource.
--
-- This operation is idempotent; it does not fail or return an error if you add an existing client ID to the provider.
module Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
    (
    -- * Creating a request
      AddClientIDToOpenIDConnectProvider (..)
    , mkAddClientIDToOpenIDConnectProvider
    -- ** Request lenses
    , acidtoidcpOpenIDConnectProviderArn
    , acidtoidcpClientID

    -- * Destructuring the response
    , AddClientIDToOpenIDConnectProviderResponse (..)
    , mkAddClientIDToOpenIDConnectProviderResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddClientIDToOpenIDConnectProvider' smart constructor.
data AddClientIDToOpenIDConnectProvider = AddClientIDToOpenIDConnectProvider'
  { openIDConnectProviderArn :: Types.OpenIDConnectProviderArn
    -- ^ The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider resource to add the client ID to. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
  , clientID :: Types.ClientID
    -- ^ The client ID (also known as audience) to add to the IAM OpenID Connect provider resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddClientIDToOpenIDConnectProvider' value with any optional fields omitted.
mkAddClientIDToOpenIDConnectProvider
    :: Types.OpenIDConnectProviderArn -- ^ 'openIDConnectProviderArn'
    -> Types.ClientID -- ^ 'clientID'
    -> AddClientIDToOpenIDConnectProvider
mkAddClientIDToOpenIDConnectProvider openIDConnectProviderArn
  clientID
  = AddClientIDToOpenIDConnectProvider'{openIDConnectProviderArn,
                                        clientID}

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider resource to add the client ID to. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- /Note:/ Consider using 'openIDConnectProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acidtoidcpOpenIDConnectProviderArn :: Lens.Lens' AddClientIDToOpenIDConnectProvider Types.OpenIDConnectProviderArn
acidtoidcpOpenIDConnectProviderArn = Lens.field @"openIDConnectProviderArn"
{-# INLINEABLE acidtoidcpOpenIDConnectProviderArn #-}
{-# DEPRECATED openIDConnectProviderArn "Use generic-lens or generic-optics with 'openIDConnectProviderArn' instead"  #-}

-- | The client ID (also known as audience) to add to the IAM OpenID Connect provider resource.
--
-- /Note:/ Consider using 'clientID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acidtoidcpClientID :: Lens.Lens' AddClientIDToOpenIDConnectProvider Types.ClientID
acidtoidcpClientID = Lens.field @"clientID"
{-# INLINEABLE acidtoidcpClientID #-}
{-# DEPRECATED clientID "Use generic-lens or generic-optics with 'clientID' instead"  #-}

instance Core.ToQuery AddClientIDToOpenIDConnectProvider where
        toQuery AddClientIDToOpenIDConnectProvider{..}
          = Core.toQueryPair "Action"
              ("AddClientIDToOpenIDConnectProvider" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "OpenIDConnectProviderArn"
                openIDConnectProviderArn
              Core.<> Core.toQueryPair "ClientID" clientID

instance Core.ToHeaders AddClientIDToOpenIDConnectProvider where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddClientIDToOpenIDConnectProvider where
        type Rs AddClientIDToOpenIDConnectProvider =
             AddClientIDToOpenIDConnectProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AddClientIDToOpenIDConnectProviderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddClientIDToOpenIDConnectProviderResponse' smart constructor.
data AddClientIDToOpenIDConnectProviderResponse = AddClientIDToOpenIDConnectProviderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddClientIDToOpenIDConnectProviderResponse' value with any optional fields omitted.
mkAddClientIDToOpenIDConnectProviderResponse
    :: AddClientIDToOpenIDConnectProviderResponse
mkAddClientIDToOpenIDConnectProviderResponse
  = AddClientIDToOpenIDConnectProviderResponse'
