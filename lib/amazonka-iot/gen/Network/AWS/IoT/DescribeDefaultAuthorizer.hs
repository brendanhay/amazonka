{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeDefaultAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default authorizer.
module Network.AWS.IoT.DescribeDefaultAuthorizer
  ( -- * Creating a request
    DescribeDefaultAuthorizer (..),
    mkDescribeDefaultAuthorizer,

    -- * Destructuring the response
    DescribeDefaultAuthorizerResponse (..),
    mkDescribeDefaultAuthorizerResponse,

    -- ** Response lenses
    ddarrsAuthorizerDescription,
    ddarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDefaultAuthorizer' smart constructor.
data DescribeDefaultAuthorizer = DescribeDefaultAuthorizer'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDefaultAuthorizer' value with any optional fields omitted.
mkDescribeDefaultAuthorizer ::
  DescribeDefaultAuthorizer
mkDescribeDefaultAuthorizer = DescribeDefaultAuthorizer'

instance Core.AWSRequest DescribeDefaultAuthorizer where
  type
    Rs DescribeDefaultAuthorizer =
      DescribeDefaultAuthorizerResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/default-authorizer",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDefaultAuthorizerResponse'
            Core.<$> (x Core..:? "authorizerDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDefaultAuthorizerResponse' smart constructor.
data DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse'
  { -- | The default authorizer's description.
    authorizerDescription :: Core.Maybe Types.AuthorizerDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDefaultAuthorizerResponse' value with any optional fields omitted.
mkDescribeDefaultAuthorizerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDefaultAuthorizerResponse
mkDescribeDefaultAuthorizerResponse responseStatus =
  DescribeDefaultAuthorizerResponse'
    { authorizerDescription =
        Core.Nothing,
      responseStatus
    }

-- | The default authorizer's description.
--
-- /Note:/ Consider using 'authorizerDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarrsAuthorizerDescription :: Lens.Lens' DescribeDefaultAuthorizerResponse (Core.Maybe Types.AuthorizerDescription)
ddarrsAuthorizerDescription = Lens.field @"authorizerDescription"
{-# DEPRECATED ddarrsAuthorizerDescription "Use generic-lens or generic-optics with 'authorizerDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarrsResponseStatus :: Lens.Lens' DescribeDefaultAuthorizerResponse Core.Int
ddarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
