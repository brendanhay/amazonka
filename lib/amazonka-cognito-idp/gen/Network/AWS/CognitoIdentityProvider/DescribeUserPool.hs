{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeUserPool (..),
    mkDescribeUserPool,

    -- ** Request lenses
    dUserPoolId,

    -- * Destructuring the response
    DescribeUserPoolResponse (..),
    mkDescribeUserPoolResponse,

    -- ** Response lenses
    duprrsUserPool,
    duprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to describe the user pool.
--
-- /See:/ 'mkDescribeUserPool' smart constructor.
newtype DescribeUserPool = DescribeUserPool'
  { -- | The user pool ID for the user pool you want to describe.
    userPoolId :: Types.UserPoolIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserPool' value with any optional fields omitted.
mkDescribeUserPool ::
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  DescribeUserPool
mkDescribeUserPool userPoolId = DescribeUserPool' {userPoolId}

-- | The user pool ID for the user pool you want to describe.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserPoolId :: Lens.Lens' DescribeUserPool Types.UserPoolIdType
dUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED dUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON DescribeUserPool where
  toJSON DescribeUserPool {..} =
    Core.object
      (Core.catMaybes [Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest DescribeUserPool where
  type Rs DescribeUserPool = DescribeUserPoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DescribeUserPool"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserPoolResponse'
            Core.<$> (x Core..:? "UserPool") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response to describe the user pool.
--
-- /See:/ 'mkDescribeUserPoolResponse' smart constructor.
data DescribeUserPoolResponse = DescribeUserPoolResponse'
  { -- | The container of metadata returned by the server to describe the pool.
    userPool :: Core.Maybe Types.UserPoolType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUserPoolResponse' value with any optional fields omitted.
mkDescribeUserPoolResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserPoolResponse
mkDescribeUserPoolResponse responseStatus =
  DescribeUserPoolResponse'
    { userPool = Core.Nothing,
      responseStatus
    }

-- | The container of metadata returned by the server to describe the pool.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserPool :: Lens.Lens' DescribeUserPoolResponse (Core.Maybe Types.UserPoolType)
duprrsUserPool = Lens.field @"userPool"
{-# DEPRECATED duprrsUserPool "Use generic-lens or generic-optics with 'userPool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsResponseStatus :: Lens.Lens' DescribeUserPoolResponse Core.Int
duprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED duprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
