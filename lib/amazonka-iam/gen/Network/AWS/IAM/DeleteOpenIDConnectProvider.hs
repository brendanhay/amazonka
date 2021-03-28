{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteOpenIDConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenID Connect identity provider (IdP) resource object in IAM.
--
-- Deleting an IAM OIDC provider resource does not update any roles that reference the provider as a principal in their trust policies. Any attempt to assume a role that references a deleted provider fails.
-- This operation is idempotent; it does not fail or return an error if you call the operation for a provider that does not exist.
module Network.AWS.IAM.DeleteOpenIDConnectProvider
    (
    -- * Creating a request
      DeleteOpenIDConnectProvider (..)
    , mkDeleteOpenIDConnectProvider
    -- ** Request lenses
    , doidcpOpenIDConnectProviderArn

    -- * Destructuring the response
    , DeleteOpenIDConnectProviderResponse (..)
    , mkDeleteOpenIDConnectProviderResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOpenIDConnectProvider' smart constructor.
newtype DeleteOpenIDConnectProvider = DeleteOpenIDConnectProvider'
  { openIDConnectProviderArn :: Types.OpenIDConnectProviderArn
    -- ^ The Amazon Resource Name (ARN) of the IAM OpenID Connect provider resource object to delete. You can get a list of OpenID Connect provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOpenIDConnectProvider' value with any optional fields omitted.
mkDeleteOpenIDConnectProvider
    :: Types.OpenIDConnectProviderArn -- ^ 'openIDConnectProviderArn'
    -> DeleteOpenIDConnectProvider
mkDeleteOpenIDConnectProvider openIDConnectProviderArn
  = DeleteOpenIDConnectProvider'{openIDConnectProviderArn}

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider resource object to delete. You can get a list of OpenID Connect provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- /Note:/ Consider using 'openIDConnectProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doidcpOpenIDConnectProviderArn :: Lens.Lens' DeleteOpenIDConnectProvider Types.OpenIDConnectProviderArn
doidcpOpenIDConnectProviderArn = Lens.field @"openIDConnectProviderArn"
{-# INLINEABLE doidcpOpenIDConnectProviderArn #-}
{-# DEPRECATED openIDConnectProviderArn "Use generic-lens or generic-optics with 'openIDConnectProviderArn' instead"  #-}

instance Core.ToQuery DeleteOpenIDConnectProvider where
        toQuery DeleteOpenIDConnectProvider{..}
          = Core.toQueryPair "Action"
              ("DeleteOpenIDConnectProvider" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "OpenIDConnectProviderArn"
                openIDConnectProviderArn

instance Core.ToHeaders DeleteOpenIDConnectProvider where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteOpenIDConnectProvider where
        type Rs DeleteOpenIDConnectProvider =
             DeleteOpenIDConnectProviderResponse
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
          = Response.receiveNull DeleteOpenIDConnectProviderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOpenIDConnectProviderResponse' smart constructor.
data DeleteOpenIDConnectProviderResponse = DeleteOpenIDConnectProviderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOpenIDConnectProviderResponse' value with any optional fields omitted.
mkDeleteOpenIDConnectProviderResponse
    :: DeleteOpenIDConnectProviderResponse
mkDeleteOpenIDConnectProviderResponse
  = DeleteOpenIDConnectProviderResponse'
