{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.TerminateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the stack for the specified application.
module Network.AWS.SMS.TerminateApp
  ( -- * Creating a request
    TerminateApp (..),
    mkTerminateApp,

    -- ** Request lenses
    taAppId,

    -- * Destructuring the response
    TerminateAppResponse (..),
    mkTerminateAppResponse,

    -- ** Response lenses
    tarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkTerminateApp' smart constructor.
newtype TerminateApp = TerminateApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Types.AppId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateApp' value with any optional fields omitted.
mkTerminateApp ::
  TerminateApp
mkTerminateApp = TerminateApp' {appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAppId :: Lens.Lens' TerminateApp (Core.Maybe Types.AppId)
taAppId = Lens.field @"appId"
{-# DEPRECATED taAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Core.FromJSON TerminateApp where
  toJSON TerminateApp {..} =
    Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest TerminateApp where
  type Rs TerminateApp = TerminateAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.TerminateApp"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          TerminateAppResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTerminateAppResponse' smart constructor.
newtype TerminateAppResponse = TerminateAppResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateAppResponse' value with any optional fields omitted.
mkTerminateAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TerminateAppResponse
mkTerminateAppResponse responseStatus =
  TerminateAppResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarrsResponseStatus :: Lens.Lens' TerminateAppResponse Core.Int
tarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
