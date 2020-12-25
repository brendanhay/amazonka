{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SendUsersMessageRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SendUsersMessageRequest
  ( SendUsersMessageRequest (..),

    -- * Smart constructor
    mkSendUsersMessageRequest,

    -- * Lenses
    sumrMessageConfiguration,
    sumrUsers,
    sumrContext,
    sumrTemplateConfiguration,
    sumrTraceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.DirectMessageConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.EndpointSendConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.TemplateConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration and other settings for a message to send to all the endpoints that are associated with a list of users.
--
-- /See:/ 'mkSendUsersMessageRequest' smart constructor.
data SendUsersMessageRequest = SendUsersMessageRequest'
  { -- | The settings and content for the default message and any default messages that you defined for specific channels.
    messageConfiguration :: Types.DirectMessageConfiguration,
    -- | A map that associates user IDs with <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> objects. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for a user by specifying settings such as content overrides and message variables.
    users :: Core.HashMap Core.Text Types.EndpointSendConfiguration,
    -- | A map of custom attribute-value pairs. For a push notification, Amazon Pinpoint adds these attributes to the data.pinpoint object in the body of the notification payload. Amazon Pinpoint also provides these attributes in the events that it generates for users-messages deliveries.
    context :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The message template to use for the message.
    templateConfiguration :: Core.Maybe Types.TemplateConfiguration,
    -- | The unique identifier for tracing the message. This identifier is visible to message recipients.
    traceId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendUsersMessageRequest' value with any optional fields omitted.
mkSendUsersMessageRequest ::
  -- | 'messageConfiguration'
  Types.DirectMessageConfiguration ->
  SendUsersMessageRequest
mkSendUsersMessageRequest messageConfiguration =
  SendUsersMessageRequest'
    { messageConfiguration,
      users = Core.mempty,
      context = Core.Nothing,
      templateConfiguration = Core.Nothing,
      traceId = Core.Nothing
    }

-- | The settings and content for the default message and any default messages that you defined for specific channels.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrMessageConfiguration :: Lens.Lens' SendUsersMessageRequest Types.DirectMessageConfiguration
sumrMessageConfiguration = Lens.field @"messageConfiguration"
{-# DEPRECATED sumrMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

-- | A map that associates user IDs with <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> objects. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for a user by specifying settings such as content overrides and message variables.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrUsers :: Lens.Lens' SendUsersMessageRequest (Core.HashMap Core.Text Types.EndpointSendConfiguration)
sumrUsers = Lens.field @"users"
{-# DEPRECATED sumrUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | A map of custom attribute-value pairs. For a push notification, Amazon Pinpoint adds these attributes to the data.pinpoint object in the body of the notification payload. Amazon Pinpoint also provides these attributes in the events that it generates for users-messages deliveries.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrContext :: Lens.Lens' SendUsersMessageRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
sumrContext = Lens.field @"context"
{-# DEPRECATED sumrContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | The message template to use for the message.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrTemplateConfiguration :: Lens.Lens' SendUsersMessageRequest (Core.Maybe Types.TemplateConfiguration)
sumrTemplateConfiguration = Lens.field @"templateConfiguration"
{-# DEPRECATED sumrTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | The unique identifier for tracing the message. This identifier is visible to message recipients.
--
-- /Note:/ Consider using 'traceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrTraceId :: Lens.Lens' SendUsersMessageRequest (Core.Maybe Core.Text)
sumrTraceId = Lens.field @"traceId"
{-# DEPRECATED sumrTraceId "Use generic-lens or generic-optics with 'traceId' instead." #-}

instance Core.FromJSON SendUsersMessageRequest where
  toJSON SendUsersMessageRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MessageConfiguration" Core..= messageConfiguration),
            Core.Just ("Users" Core..= users),
            ("Context" Core..=) Core.<$> context,
            ("TemplateConfiguration" Core..=) Core.<$> templateConfiguration,
            ("TraceId" Core..=) Core.<$> traceId
          ]
      )
