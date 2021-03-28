{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific identity provider.
module Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
    (
    -- * Creating a request
      DescribeIdentityProvider (..)
    , mkDescribeIdentityProvider
    -- ** Request lenses
    , dipUserPoolId
    , dipProviderName

    -- * Destructuring the response
    , DescribeIdentityProviderResponse (..)
    , mkDescribeIdentityProviderResponse
    -- ** Response lenses
    , diprrsIdentityProvider
    , diprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeIdentityProvider' smart constructor.
data DescribeIdentityProvider = DescribeIdentityProvider'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID.
  , providerName :: Types.ProviderNameType
    -- ^ The identity provider name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIdentityProvider' value with any optional fields omitted.
mkDescribeIdentityProvider
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> Types.ProviderNameType -- ^ 'providerName'
    -> DescribeIdentityProvider
mkDescribeIdentityProvider userPoolId providerName
  = DescribeIdentityProvider'{userPoolId, providerName}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipUserPoolId :: Lens.Lens' DescribeIdentityProvider Types.UserPoolIdType
dipUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE dipUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipProviderName :: Lens.Lens' DescribeIdentityProvider Types.ProviderNameType
dipProviderName = Lens.field @"providerName"
{-# INLINEABLE dipProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

instance Core.ToQuery DescribeIdentityProvider where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeIdentityProvider where
        toHeaders DescribeIdentityProvider{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DescribeIdentityProvider")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeIdentityProvider where
        toJSON DescribeIdentityProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ProviderName" Core..= providerName)])

instance Core.AWSRequest DescribeIdentityProvider where
        type Rs DescribeIdentityProvider = DescribeIdentityProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeIdentityProviderResponse' Core.<$>
                   (x Core..: "IdentityProvider") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeIdentityProviderResponse' smart constructor.
data DescribeIdentityProviderResponse = DescribeIdentityProviderResponse'
  { identityProvider :: Types.IdentityProviderType
    -- ^ The identity provider that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeIdentityProviderResponse' value with any optional fields omitted.
mkDescribeIdentityProviderResponse
    :: Types.IdentityProviderType -- ^ 'identityProvider'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeIdentityProviderResponse
mkDescribeIdentityProviderResponse identityProvider responseStatus
  = DescribeIdentityProviderResponse'{identityProvider,
                                      responseStatus}

-- | The identity provider that was deleted.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsIdentityProvider :: Lens.Lens' DescribeIdentityProviderResponse Types.IdentityProviderType
diprrsIdentityProvider = Lens.field @"identityProvider"
{-# INLINEABLE diprrsIdentityProvider #-}
{-# DEPRECATED identityProvider "Use generic-lens or generic-optics with 'identityProvider' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsResponseStatus :: Lens.Lens' DescribeIdentityProviderResponse Core.Int
diprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
