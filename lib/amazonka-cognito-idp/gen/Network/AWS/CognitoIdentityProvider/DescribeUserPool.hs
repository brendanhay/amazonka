{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration information and metadata of the specified user pool.
module Network.AWS.CognitoIdentityProvider.DescribeUserPool
    (
    -- * Creating a request
      DescribeUserPool (..)
    , mkDescribeUserPool
    -- ** Request lenses
    , dUserPoolId

    -- * Destructuring the response
    , DescribeUserPoolResponse (..)
    , mkDescribeUserPoolResponse
    -- ** Response lenses
    , duprrsUserPool
    , duprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe the user pool.
--
-- /See:/ 'mkDescribeUserPool' smart constructor.
newtype DescribeUserPool = DescribeUserPool'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID for the user pool you want to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserPool' value with any optional fields omitted.
mkDescribeUserPool
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> DescribeUserPool
mkDescribeUserPool userPoolId = DescribeUserPool'{userPoolId}

-- | The user pool ID for the user pool you want to describe.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserPoolId :: Lens.Lens' DescribeUserPool Types.UserPoolIdType
dUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE dUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.ToQuery DescribeUserPool where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeUserPool where
        toHeaders DescribeUserPool{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DescribeUserPool")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeUserPool where
        toJSON DescribeUserPool{..}
          = Core.object
              (Core.catMaybes [Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest DescribeUserPool where
        type Rs DescribeUserPool = DescribeUserPoolResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeUserPoolResponse' Core.<$>
                   (x Core..:? "UserPool") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response to describe the user pool.
--
-- /See:/ 'mkDescribeUserPoolResponse' smart constructor.
data DescribeUserPoolResponse = DescribeUserPoolResponse'
  { userPool :: Core.Maybe Types.UserPoolType
    -- ^ The container of metadata returned by the server to describe the pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUserPoolResponse' value with any optional fields omitted.
mkDescribeUserPoolResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUserPoolResponse
mkDescribeUserPoolResponse responseStatus
  = DescribeUserPoolResponse'{userPool = Core.Nothing,
                              responseStatus}

-- | The container of metadata returned by the server to describe the pool.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserPool :: Lens.Lens' DescribeUserPoolResponse (Core.Maybe Types.UserPoolType)
duprrsUserPool = Lens.field @"userPool"
{-# INLINEABLE duprrsUserPool #-}
{-# DEPRECATED userPool "Use generic-lens or generic-optics with 'userPool' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsResponseStatus :: Lens.Lens' DescribeUserPoolResponse Core.Int
duprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE duprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
