{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a connection between the webhook that was created and the external tool with events to be detected.
module Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
  ( -- * Creating a request
    RegisterWebhookWithThirdParty (..),
    mkRegisterWebhookWithThirdParty,

    -- ** Request lenses
    rwwtpWebhookName,

    -- * Destructuring the response
    RegisterWebhookWithThirdPartyResponse (..),
    mkRegisterWebhookWithThirdPartyResponse,

    -- ** Response lenses
    rwwtprrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterWebhookWithThirdParty' smart constructor.
newtype RegisterWebhookWithThirdParty = RegisterWebhookWithThirdParty'
  { -- | The name of an existing webhook created with PutWebhook to register with a supported third party.
    webhookName :: Core.Maybe Types.WebhookName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterWebhookWithThirdParty' value with any optional fields omitted.
mkRegisterWebhookWithThirdParty ::
  RegisterWebhookWithThirdParty
mkRegisterWebhookWithThirdParty =
  RegisterWebhookWithThirdParty' {webhookName = Core.Nothing}

-- | The name of an existing webhook created with PutWebhook to register with a supported third party.
--
-- /Note:/ Consider using 'webhookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwwtpWebhookName :: Lens.Lens' RegisterWebhookWithThirdParty (Core.Maybe Types.WebhookName)
rwwtpWebhookName = Lens.field @"webhookName"
{-# DEPRECATED rwwtpWebhookName "Use generic-lens or generic-optics with 'webhookName' instead." #-}

instance Core.FromJSON RegisterWebhookWithThirdParty where
  toJSON RegisterWebhookWithThirdParty {..} =
    Core.object
      (Core.catMaybes [("webhookName" Core..=) Core.<$> webhookName])

instance Core.AWSRequest RegisterWebhookWithThirdParty where
  type
    Rs RegisterWebhookWithThirdParty =
      RegisterWebhookWithThirdPartyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodePipeline_20150709.RegisterWebhookWithThirdParty"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterWebhookWithThirdPartyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterWebhookWithThirdPartyResponse' smart constructor.
newtype RegisterWebhookWithThirdPartyResponse = RegisterWebhookWithThirdPartyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterWebhookWithThirdPartyResponse' value with any optional fields omitted.
mkRegisterWebhookWithThirdPartyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterWebhookWithThirdPartyResponse
mkRegisterWebhookWithThirdPartyResponse responseStatus =
  RegisterWebhookWithThirdPartyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwwtprrsResponseStatus :: Lens.Lens' RegisterWebhookWithThirdPartyResponse Core.Int
rwwtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rwwtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
