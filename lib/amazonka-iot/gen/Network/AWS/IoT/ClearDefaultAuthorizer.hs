{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ClearDefaultAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears the default authorizer.
module Network.AWS.IoT.ClearDefaultAuthorizer
  ( -- * Creating a request
    ClearDefaultAuthorizer (..),
    mkClearDefaultAuthorizer,

    -- * Destructuring the response
    ClearDefaultAuthorizerResponse (..),
    mkClearDefaultAuthorizerResponse,

    -- ** Response lenses
    cdarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkClearDefaultAuthorizer' smart constructor.
data ClearDefaultAuthorizer = ClearDefaultAuthorizer'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClearDefaultAuthorizer' value with any optional fields omitted.
mkClearDefaultAuthorizer ::
  ClearDefaultAuthorizer
mkClearDefaultAuthorizer = ClearDefaultAuthorizer'

instance Core.AWSRequest ClearDefaultAuthorizer where
  type Rs ClearDefaultAuthorizer = ClearDefaultAuthorizerResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath = Core.rawPath "/default-authorizer",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ClearDefaultAuthorizerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkClearDefaultAuthorizerResponse' smart constructor.
newtype ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ClearDefaultAuthorizerResponse' value with any optional fields omitted.
mkClearDefaultAuthorizerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ClearDefaultAuthorizerResponse
mkClearDefaultAuthorizerResponse responseStatus =
  ClearDefaultAuthorizerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarrsResponseStatus :: Lens.Lens' ClearDefaultAuthorizerResponse Core.Int
cdarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
