{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified client ID (also known as audience) from the list of client IDs registered for the specified IAM OpenID Connect (OIDC) provider resource object.
--
-- This operation is idempotent; it does not fail or return an error if you try to remove a client ID that does not exist.
module Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
    (
    -- * Creating a request
      RemoveClientIDFromOpenIDConnectProvider (..)
    , mkRemoveClientIDFromOpenIDConnectProvider
    -- ** Request lenses
    , rcidfoidcpOpenIDConnectProviderArn
    , rcidfoidcpClientID

    -- * Destructuring the response
    , RemoveClientIDFromOpenIDConnectProviderResponse (..)
    , mkRemoveClientIDFromOpenIDConnectProviderResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveClientIDFromOpenIDConnectProvider' smart constructor.
data RemoveClientIDFromOpenIDConnectProvider = RemoveClientIDFromOpenIDConnectProvider'
  { openIDConnectProviderArn :: Types.OpenIDConnectProviderArn
    -- ^ The Amazon Resource Name (ARN) of the IAM OIDC provider resource to remove the client ID from. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , clientID :: Types.ClientID
    -- ^ The client ID (also known as audience) to remove from the IAM OIDC provider resource. For more information about client IDs, see 'CreateOpenIDConnectProvider' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveClientIDFromOpenIDConnectProvider' value with any optional fields omitted.
mkRemoveClientIDFromOpenIDConnectProvider
    :: Types.OpenIDConnectProviderArn -- ^ 'openIDConnectProviderArn'
    -> Types.ClientID -- ^ 'clientID'
    -> RemoveClientIDFromOpenIDConnectProvider
mkRemoveClientIDFromOpenIDConnectProvider openIDConnectProviderArn
  clientID
  = RemoveClientIDFromOpenIDConnectProvider'{openIDConnectProviderArn,
                                             clientID}

-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource to remove the client ID from. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'openIDConnectProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcidfoidcpOpenIDConnectProviderArn :: Lens.Lens' RemoveClientIDFromOpenIDConnectProvider Types.OpenIDConnectProviderArn
rcidfoidcpOpenIDConnectProviderArn = Lens.field @"openIDConnectProviderArn"
{-# INLINEABLE rcidfoidcpOpenIDConnectProviderArn #-}
{-# DEPRECATED openIDConnectProviderArn "Use generic-lens or generic-optics with 'openIDConnectProviderArn' instead"  #-}

-- | The client ID (also known as audience) to remove from the IAM OIDC provider resource. For more information about client IDs, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'clientID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcidfoidcpClientID :: Lens.Lens' RemoveClientIDFromOpenIDConnectProvider Types.ClientID
rcidfoidcpClientID = Lens.field @"clientID"
{-# INLINEABLE rcidfoidcpClientID #-}
{-# DEPRECATED clientID "Use generic-lens or generic-optics with 'clientID' instead"  #-}

instance Core.ToQuery RemoveClientIDFromOpenIDConnectProvider where
        toQuery RemoveClientIDFromOpenIDConnectProvider{..}
          = Core.toQueryPair "Action"
              ("RemoveClientIDFromOpenIDConnectProvider" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "OpenIDConnectProviderArn"
                openIDConnectProviderArn
              Core.<> Core.toQueryPair "ClientID" clientID

instance Core.ToHeaders RemoveClientIDFromOpenIDConnectProvider
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemoveClientIDFromOpenIDConnectProvider
         where
        type Rs RemoveClientIDFromOpenIDConnectProvider =
             RemoveClientIDFromOpenIDConnectProviderResponse
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
          = Response.receiveNull
              RemoveClientIDFromOpenIDConnectProviderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveClientIDFromOpenIDConnectProviderResponse' smart constructor.
data RemoveClientIDFromOpenIDConnectProviderResponse = RemoveClientIDFromOpenIDConnectProviderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveClientIDFromOpenIDConnectProviderResponse' value with any optional fields omitted.
mkRemoveClientIDFromOpenIDConnectProviderResponse
    :: RemoveClientIDFromOpenIDConnectProviderResponse
mkRemoveClientIDFromOpenIDConnectProviderResponse
  = RemoveClientIDFromOpenIDConnectProviderResponse'
