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
    sumrTraceId,
    sumrContext,
    sumrTemplateConfiguration,
    sumrMessageConfiguration,
    sumrUsers,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DirectMessageConfiguration
import Network.AWS.Pinpoint.Types.EndpointSendConfiguration
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration and other settings for a message to send to all the endpoints that are associated with a list of users.
--
-- /See:/ 'mkSendUsersMessageRequest' smart constructor.
data SendUsersMessageRequest = SendUsersMessageRequest'
  { traceId ::
      Lude.Maybe Lude.Text,
    context ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    templateConfiguration ::
      Lude.Maybe TemplateConfiguration,
    messageConfiguration ::
      DirectMessageConfiguration,
    users ::
      Lude.HashMap
        Lude.Text
        (EndpointSendConfiguration)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendUsersMessageRequest' with the minimum fields required to make a request.
--
-- * 'context' - A map of custom attribute-value pairs. For a push notification, Amazon Pinpoint adds these attributes to the data.pinpoint object in the body of the notification payload. Amazon Pinpoint also provides these attributes in the events that it generates for users-messages deliveries.
-- * 'messageConfiguration' - The settings and content for the default message and any default messages that you defined for specific channels.
-- * 'templateConfiguration' - The message template to use for the message.
-- * 'traceId' - The unique identifier for tracing the message. This identifier is visible to message recipients.
-- * 'users' - A map that associates user IDs with <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> objects. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for a user by specifying settings such as content overrides and message variables.
mkSendUsersMessageRequest ::
  -- | 'messageConfiguration'
  DirectMessageConfiguration ->
  SendUsersMessageRequest
mkSendUsersMessageRequest pMessageConfiguration_ =
  SendUsersMessageRequest'
    { traceId = Lude.Nothing,
      context = Lude.Nothing,
      templateConfiguration = Lude.Nothing,
      messageConfiguration = pMessageConfiguration_,
      users = Lude.mempty
    }

-- | The unique identifier for tracing the message. This identifier is visible to message recipients.
--
-- /Note:/ Consider using 'traceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrTraceId :: Lens.Lens' SendUsersMessageRequest (Lude.Maybe Lude.Text)
sumrTraceId = Lens.lens (traceId :: SendUsersMessageRequest -> Lude.Maybe Lude.Text) (\s a -> s {traceId = a} :: SendUsersMessageRequest)
{-# DEPRECATED sumrTraceId "Use generic-lens or generic-optics with 'traceId' instead." #-}

-- | A map of custom attribute-value pairs. For a push notification, Amazon Pinpoint adds these attributes to the data.pinpoint object in the body of the notification payload. Amazon Pinpoint also provides these attributes in the events that it generates for users-messages deliveries.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrContext :: Lens.Lens' SendUsersMessageRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sumrContext = Lens.lens (context :: SendUsersMessageRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {context = a} :: SendUsersMessageRequest)
{-# DEPRECATED sumrContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | The message template to use for the message.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrTemplateConfiguration :: Lens.Lens' SendUsersMessageRequest (Lude.Maybe TemplateConfiguration)
sumrTemplateConfiguration = Lens.lens (templateConfiguration :: SendUsersMessageRequest -> Lude.Maybe TemplateConfiguration) (\s a -> s {templateConfiguration = a} :: SendUsersMessageRequest)
{-# DEPRECATED sumrTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | The settings and content for the default message and any default messages that you defined for specific channels.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrMessageConfiguration :: Lens.Lens' SendUsersMessageRequest DirectMessageConfiguration
sumrMessageConfiguration = Lens.lens (messageConfiguration :: SendUsersMessageRequest -> DirectMessageConfiguration) (\s a -> s {messageConfiguration = a} :: SendUsersMessageRequest)
{-# DEPRECATED sumrMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

-- | A map that associates user IDs with <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> objects. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for a user by specifying settings such as content overrides and message variables.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrUsers :: Lens.Lens' SendUsersMessageRequest (Lude.HashMap Lude.Text (EndpointSendConfiguration))
sumrUsers = Lens.lens (users :: SendUsersMessageRequest -> Lude.HashMap Lude.Text (EndpointSendConfiguration)) (\s a -> s {users = a} :: SendUsersMessageRequest)
{-# DEPRECATED sumrUsers "Use generic-lens or generic-optics with 'users' instead." #-}

instance Lude.ToJSON SendUsersMessageRequest where
  toJSON SendUsersMessageRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TraceId" Lude..=) Lude.<$> traceId,
            ("Context" Lude..=) Lude.<$> context,
            ("TemplateConfiguration" Lude..=) Lude.<$> templateConfiguration,
            Lude.Just ("MessageConfiguration" Lude..= messageConfiguration),
            Lude.Just ("Users" Lude..= users)
          ]
      )
