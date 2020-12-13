{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StartChatContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a contact flow to start a new chat for the customer. Response of this API provides a token required to obtain credentials from the <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> API in the Amazon Connect Participant Service.
--
-- When a new chat contact is successfully created, clients need to subscribe to the participant’s connection for the created chat within 5 minutes. This is achieved by invoking <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> with WEBSOCKET and CONNECTION_CREDENTIALS.
-- A 429 error occurs in two situations:
--
--     * API rate limit is exceeded. API TPS throttling returns a @TooManyRequests@ exception from the API Gateway.
--
--
--     * The <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html quota for concurrent active chats> is exceeded. Active chat throttling returns a @LimitExceededException@ .
--
--
-- For more information about how chat works, see <https://docs.aws.amazon.com/connect/latest/adminguide/chat.html Chat> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.StartChatContact
  ( -- * Creating a request
    StartChatContact (..),
    mkStartChatContact,

    -- ** Request lenses
    sccInstanceId,
    sccClientToken,
    sccContactFlowId,
    sccAttributes,
    sccInitialMessage,
    sccParticipantDetails,

    -- * Destructuring the response
    StartChatContactResponse (..),
    mkStartChatContactResponse,

    -- ** Response lenses
    sccrsParticipantToken,
    sccrsParticipantId,
    sccrsContactId,
    sccrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartChatContact' smart constructor.
data StartChatContact = StartChatContact'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The identifier of the contact flow for initiating the chat. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Lude.Text,
    -- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The initial message to be sent to the newly created chat.
    initialMessage :: Lude.Maybe ChatMessage,
    -- | Information identifying the participant.
    participantDetails :: ParticipantDetails
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartChatContact' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'clientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
-- * 'contactFlowId' - The identifier of the contact flow for initiating the chat. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
-- * 'attributes' - A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
-- * 'initialMessage' - The initial message to be sent to the newly created chat.
-- * 'participantDetails' - Information identifying the participant.
mkStartChatContact ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'contactFlowId'
  Lude.Text ->
  -- | 'participantDetails'
  ParticipantDetails ->
  StartChatContact
mkStartChatContact
  pInstanceId_
  pContactFlowId_
  pParticipantDetails_ =
    StartChatContact'
      { instanceId = pInstanceId_,
        clientToken = Lude.Nothing,
        contactFlowId = pContactFlowId_,
        attributes = Lude.Nothing,
        initialMessage = Lude.Nothing,
        participantDetails = pParticipantDetails_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccInstanceId :: Lens.Lens' StartChatContact Lude.Text
sccInstanceId = Lens.lens (instanceId :: StartChatContact -> Lude.Text) (\s a -> s {instanceId = a} :: StartChatContact)
{-# DEPRECATED sccInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccClientToken :: Lens.Lens' StartChatContact (Lude.Maybe Lude.Text)
sccClientToken = Lens.lens (clientToken :: StartChatContact -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: StartChatContact)
{-# DEPRECATED sccClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The identifier of the contact flow for initiating the chat. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccContactFlowId :: Lens.Lens' StartChatContact Lude.Text
sccContactFlowId = Lens.lens (contactFlowId :: StartChatContact -> Lude.Text) (\s a -> s {contactFlowId = a} :: StartChatContact)
{-# DEPRECATED sccContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccAttributes :: Lens.Lens' StartChatContact (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sccAttributes = Lens.lens (attributes :: StartChatContact -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: StartChatContact)
{-# DEPRECATED sccAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The initial message to be sent to the newly created chat.
--
-- /Note:/ Consider using 'initialMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccInitialMessage :: Lens.Lens' StartChatContact (Lude.Maybe ChatMessage)
sccInitialMessage = Lens.lens (initialMessage :: StartChatContact -> Lude.Maybe ChatMessage) (\s a -> s {initialMessage = a} :: StartChatContact)
{-# DEPRECATED sccInitialMessage "Use generic-lens or generic-optics with 'initialMessage' instead." #-}

-- | Information identifying the participant.
--
-- /Note:/ Consider using 'participantDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccParticipantDetails :: Lens.Lens' StartChatContact ParticipantDetails
sccParticipantDetails = Lens.lens (participantDetails :: StartChatContact -> ParticipantDetails) (\s a -> s {participantDetails = a} :: StartChatContact)
{-# DEPRECATED sccParticipantDetails "Use generic-lens or generic-optics with 'participantDetails' instead." #-}

instance Lude.AWSRequest StartChatContact where
  type Rs StartChatContact = StartChatContactResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartChatContactResponse'
            Lude.<$> (x Lude..?> "ParticipantToken")
            Lude.<*> (x Lude..?> "ParticipantId")
            Lude.<*> (x Lude..?> "ContactId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartChatContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartChatContact where
  toJSON StartChatContact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            Lude.Just ("ContactFlowId" Lude..= contactFlowId),
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("InitialMessage" Lude..=) Lude.<$> initialMessage,
            Lude.Just ("ParticipantDetails" Lude..= participantDetails)
          ]
      )

instance Lude.ToPath StartChatContact where
  toPath = Lude.const "/contact/chat"

instance Lude.ToQuery StartChatContact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartChatContactResponse' smart constructor.
data StartChatContactResponse = StartChatContactResponse'
  { -- | The token used by the chat participant to call <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> . The participant token is valid for the lifetime of a chat participant.
    participantToken :: Lude.Maybe Lude.Text,
    -- | The identifier for a chat participant. The participantId for a chat participant is the same throughout the chat lifecycle.
    participantId :: Lude.Maybe Lude.Text,
    -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartChatContactResponse' with the minimum fields required to make a request.
--
-- * 'participantToken' - The token used by the chat participant to call <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> . The participant token is valid for the lifetime of a chat participant.
-- * 'participantId' - The identifier for a chat participant. The participantId for a chat participant is the same throughout the chat lifecycle.
-- * 'contactId' - The identifier of this contact within the Amazon Connect instance.
-- * 'responseStatus' - The response status code.
mkStartChatContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartChatContactResponse
mkStartChatContactResponse pResponseStatus_ =
  StartChatContactResponse'
    { participantToken = Lude.Nothing,
      participantId = Lude.Nothing,
      contactId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token used by the chat participant to call <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> . The participant token is valid for the lifetime of a chat participant.
--
-- /Note:/ Consider using 'participantToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrsParticipantToken :: Lens.Lens' StartChatContactResponse (Lude.Maybe Lude.Text)
sccrsParticipantToken = Lens.lens (participantToken :: StartChatContactResponse -> Lude.Maybe Lude.Text) (\s a -> s {participantToken = a} :: StartChatContactResponse)
{-# DEPRECATED sccrsParticipantToken "Use generic-lens or generic-optics with 'participantToken' instead." #-}

-- | The identifier for a chat participant. The participantId for a chat participant is the same throughout the chat lifecycle.
--
-- /Note:/ Consider using 'participantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrsParticipantId :: Lens.Lens' StartChatContactResponse (Lude.Maybe Lude.Text)
sccrsParticipantId = Lens.lens (participantId :: StartChatContactResponse -> Lude.Maybe Lude.Text) (\s a -> s {participantId = a} :: StartChatContactResponse)
{-# DEPRECATED sccrsParticipantId "Use generic-lens or generic-optics with 'participantId' instead." #-}

-- | The identifier of this contact within the Amazon Connect instance.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrsContactId :: Lens.Lens' StartChatContactResponse (Lude.Maybe Lude.Text)
sccrsContactId = Lens.lens (contactId :: StartChatContactResponse -> Lude.Maybe Lude.Text) (\s a -> s {contactId = a} :: StartChatContactResponse)
{-# DEPRECATED sccrsContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrsResponseStatus :: Lens.Lens' StartChatContactResponse Lude.Int
sccrsResponseStatus = Lens.lens (responseStatus :: StartChatContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartChatContactResponse)
{-# DEPRECATED sccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
