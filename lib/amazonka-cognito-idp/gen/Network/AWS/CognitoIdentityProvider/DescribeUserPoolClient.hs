{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Client method for returning the configuration information and metadata of the specified user pool app client.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
    (
    -- * Creating a request
      DescribeUserPoolClient (..)
    , mkDescribeUserPoolClient
    -- ** Request lenses
    , dupcfUserPoolId
    , dupcfClientId

    -- * Destructuring the response
    , DescribeUserPoolClientResponse (..)
    , mkDescribeUserPoolClientResponse
    -- ** Response lenses
    , dupcrrsUserPoolClient
    , dupcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe a user pool client.
--
-- /See:/ 'mkDescribeUserPoolClient' smart constructor.
data DescribeUserPoolClient = DescribeUserPoolClient'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool you want to describe.
  , clientId :: Types.ClientIdType
    -- ^ The app client ID of the app associated with the user pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserPoolClient' value with any optional fields omitted.
mkDescribeUserPoolClient
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.ClientIdType -- ^ 'clientId'
    -> DescribeUserPoolClient
mkDescribeUserPoolClient userPoolId clientId
  = DescribeUserPoolClient'{userPoolId, clientId}

-- | The user pool ID for the user pool you want to describe.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcfUserPoolId :: Lens.Lens' DescribeUserPoolClient Types.UserPoolId
dupcfUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE dupcfUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcfClientId :: Lens.Lens' DescribeUserPoolClient Types.ClientIdType
dupcfClientId = Lens.field @"clientId"
{-# INLINEABLE dupcfClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

instance Core.ToQuery DescribeUserPoolClient where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeUserPoolClient where
        toHeaders DescribeUserPoolClient{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DescribeUserPoolClient")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeUserPoolClient where
        toJSON DescribeUserPoolClient{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ClientId" Core..= clientId)])

instance Core.AWSRequest DescribeUserPoolClient where
        type Rs DescribeUserPoolClient = DescribeUserPoolClientResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeUserPoolClientResponse' Core.<$>
                   (x Core..:? "UserPoolClient") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server from a request to describe the user pool client.
--
-- /See:/ 'mkDescribeUserPoolClientResponse' smart constructor.
data DescribeUserPoolClientResponse = DescribeUserPoolClientResponse'
  { userPoolClient :: Core.Maybe Types.UserPoolClientType
    -- ^ The user pool client from a server response to describe the user pool client.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUserPoolClientResponse' value with any optional fields omitted.
mkDescribeUserPoolClientResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUserPoolClientResponse
mkDescribeUserPoolClientResponse responseStatus
  = DescribeUserPoolClientResponse'{userPoolClient = Core.Nothing,
                                    responseStatus}

-- | The user pool client from a server response to describe the user pool client.
--
-- /Note:/ Consider using 'userPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcrrsUserPoolClient :: Lens.Lens' DescribeUserPoolClientResponse (Core.Maybe Types.UserPoolClientType)
dupcrrsUserPoolClient = Lens.field @"userPoolClient"
{-# INLINEABLE dupcrrsUserPoolClient #-}
{-# DEPRECATED userPoolClient "Use generic-lens or generic-optics with 'userPoolClient' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcrrsResponseStatus :: Lens.Lens' DescribeUserPoolClientResponse Core.Int
dupcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dupcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
