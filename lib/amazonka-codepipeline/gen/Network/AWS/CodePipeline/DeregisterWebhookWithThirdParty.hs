{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the connection between the webhook that was created by CodePipeline and the external tool with events to be detected. Currently supported only for webhooks that target an action type of GitHub.
module Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
  ( -- * Creating a request
    DeregisterWebhookWithThirdParty (..),
    mkDeregisterWebhookWithThirdParty,

    -- ** Request lenses
    dwwtpWebhookName,

    -- * Destructuring the response
    DeregisterWebhookWithThirdPartyResponse (..),
    mkDeregisterWebhookWithThirdPartyResponse,

    -- ** Response lenses
    dwwtprrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterWebhookWithThirdParty' smart constructor.
newtype DeregisterWebhookWithThirdParty = DeregisterWebhookWithThirdParty'
  { -- | The name of the webhook you want to deregister.
    webhookName :: Core.Maybe Types.WebhookName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterWebhookWithThirdParty' value with any optional fields omitted.
mkDeregisterWebhookWithThirdParty ::
  DeregisterWebhookWithThirdParty
mkDeregisterWebhookWithThirdParty =
  DeregisterWebhookWithThirdParty' {webhookName = Core.Nothing}

-- | The name of the webhook you want to deregister.
--
-- /Note:/ Consider using 'webhookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwwtpWebhookName :: Lens.Lens' DeregisterWebhookWithThirdParty (Core.Maybe Types.WebhookName)
dwwtpWebhookName = Lens.field @"webhookName"
{-# DEPRECATED dwwtpWebhookName "Use generic-lens or generic-optics with 'webhookName' instead." #-}

instance Core.FromJSON DeregisterWebhookWithThirdParty where
  toJSON DeregisterWebhookWithThirdParty {..} =
    Core.object
      (Core.catMaybes [("webhookName" Core..=) Core.<$> webhookName])

instance Core.AWSRequest DeregisterWebhookWithThirdParty where
  type
    Rs DeregisterWebhookWithThirdParty =
      DeregisterWebhookWithThirdPartyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodePipeline_20150709.DeregisterWebhookWithThirdParty"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterWebhookWithThirdPartyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterWebhookWithThirdPartyResponse' smart constructor.
newtype DeregisterWebhookWithThirdPartyResponse = DeregisterWebhookWithThirdPartyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterWebhookWithThirdPartyResponse' value with any optional fields omitted.
mkDeregisterWebhookWithThirdPartyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterWebhookWithThirdPartyResponse
mkDeregisterWebhookWithThirdPartyResponse responseStatus =
  DeregisterWebhookWithThirdPartyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwwtprrsResponseStatus :: Lens.Lens' DeregisterWebhookWithThirdPartyResponse Core.Int
dwwtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwwtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
