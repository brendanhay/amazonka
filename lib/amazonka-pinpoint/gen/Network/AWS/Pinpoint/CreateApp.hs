{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Network.AWS.Pinpoint.CreateApp
  ( -- * Creating a request
    CreateApp (..),
    mkCreateApp,

    -- ** Request lenses
    caCreateApplicationRequest,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    carrsApplicationResponse,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateApp' smart constructor.
newtype CreateApp = CreateApp'
  { createApplicationRequest :: Types.CreateApplicationRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApp' value with any optional fields omitted.
mkCreateApp ::
  -- | 'createApplicationRequest'
  Types.CreateApplicationRequest ->
  CreateApp
mkCreateApp createApplicationRequest =
  CreateApp' {createApplicationRequest}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createApplicationRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreateApplicationRequest :: Lens.Lens' CreateApp Types.CreateApplicationRequest
caCreateApplicationRequest = Lens.field @"createApplicationRequest"
{-# DEPRECATED caCreateApplicationRequest "Use generic-lens or generic-optics with 'createApplicationRequest' instead." #-}

instance Core.FromJSON CreateApp where
  toJSON CreateApp {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CreateApplicationRequest" Core..= createApplicationRequest)
          ]
      )

instance Core.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/apps",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { applicationResponse :: Types.ApplicationResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppResponse' value with any optional fields omitted.
mkCreateAppResponse ::
  -- | 'applicationResponse'
  Types.ApplicationResponse ->
  -- | 'responseStatus'
  Core.Int ->
  CreateAppResponse
mkCreateAppResponse applicationResponse responseStatus =
  CreateAppResponse' {applicationResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsApplicationResponse :: Lens.Lens' CreateAppResponse Types.ApplicationResponse
carrsApplicationResponse = Lens.field @"applicationResponse"
{-# DEPRECATED carrsApplicationResponse "Use generic-lens or generic-optics with 'applicationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAppResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
