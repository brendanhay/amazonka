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
    sccContactFlowId,
    sccParticipantDetails,
    sccAttributes,
    sccClientToken,
    sccInitialMessage,

    -- * Destructuring the response
    StartChatContactResponse (..),
    mkStartChatContactResponse,

    -- ** Response lenses
    sccrrsContactId,
    sccrrsParticipantId,
    sccrrsParticipantToken,
    sccrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartChatContact' smart constructor.
data StartChatContact = StartChatContact'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The identifier of the contact flow for initiating the chat. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Types.ContactFlowId,
    -- | Information identifying the participant.
    participantDetails :: Types.ParticipantDetails,
    -- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The initial message to be sent to the newly created chat.
    initialMessage :: Core.Maybe Types.ChatMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartChatContact' value with any optional fields omitted.
mkStartChatContact ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'contactFlowId'
  Types.ContactFlowId ->
  -- | 'participantDetails'
  Types.ParticipantDetails ->
  StartChatContact
mkStartChatContact instanceId contactFlowId participantDetails =
  StartChatContact'
    { instanceId,
      contactFlowId,
      participantDetails,
      attributes = Core.Nothing,
      clientToken = Core.Nothing,
      initialMessage = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccInstanceId :: Lens.Lens' StartChatContact Types.InstanceId
sccInstanceId = Lens.field @"instanceId"
{-# DEPRECATED sccInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact flow for initiating the chat. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccContactFlowId :: Lens.Lens' StartChatContact Types.ContactFlowId
sccContactFlowId = Lens.field @"contactFlowId"
{-# DEPRECATED sccContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | Information identifying the participant.
--
-- /Note:/ Consider using 'participantDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccParticipantDetails :: Lens.Lens' StartChatContact Types.ParticipantDetails
sccParticipantDetails = Lens.field @"participantDetails"
{-# DEPRECATED sccParticipantDetails "Use generic-lens or generic-optics with 'participantDetails' instead." #-}

-- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccAttributes :: Lens.Lens' StartChatContact (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
sccAttributes = Lens.field @"attributes"
{-# DEPRECATED sccAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccClientToken :: Lens.Lens' StartChatContact (Core.Maybe Types.ClientToken)
sccClientToken = Lens.field @"clientToken"
{-# DEPRECATED sccClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The initial message to be sent to the newly created chat.
--
-- /Note:/ Consider using 'initialMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccInitialMessage :: Lens.Lens' StartChatContact (Core.Maybe Types.ChatMessage)
sccInitialMessage = Lens.field @"initialMessage"
{-# DEPRECATED sccInitialMessage "Use generic-lens or generic-optics with 'initialMessage' instead." #-}

instance Core.FromJSON StartChatContact where
  toJSON StartChatContact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("ContactFlowId" Core..= contactFlowId),
            Core.Just ("ParticipantDetails" Core..= participantDetails),
            ("Attributes" Core..=) Core.<$> attributes,
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("InitialMessage" Core..=) Core.<$> initialMessage
          ]
      )

instance Core.AWSRequest StartChatContact where
  type Rs StartChatContact = StartChatContactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath "/contact/chat",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChatContactResponse'
            Core.<$> (x Core..:? "ContactId")
            Core.<*> (x Core..:? "ParticipantId")
            Core.<*> (x Core..:? "ParticipantToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartChatContactResponse' smart constructor.
data StartChatContactResponse = StartChatContactResponse'
  { -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Core.Maybe Types.ContactId,
    -- | The identifier for a chat participant. The participantId for a chat participant is the same throughout the chat lifecycle.
    participantId :: Core.Maybe Types.ParticipantId,
    -- | The token used by the chat participant to call <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> . The participant token is valid for the lifetime of a chat participant.
    participantToken :: Core.Maybe Types.ParticipantToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartChatContactResponse' value with any optional fields omitted.
mkStartChatContactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartChatContactResponse
mkStartChatContactResponse responseStatus =
  StartChatContactResponse'
    { contactId = Core.Nothing,
      participantId = Core.Nothing,
      participantToken = Core.Nothing,
      responseStatus
    }

-- | The identifier of this contact within the Amazon Connect instance.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrrsContactId :: Lens.Lens' StartChatContactResponse (Core.Maybe Types.ContactId)
sccrrsContactId = Lens.field @"contactId"
{-# DEPRECATED sccrrsContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier for a chat participant. The participantId for a chat participant is the same throughout the chat lifecycle.
--
-- /Note:/ Consider using 'participantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrrsParticipantId :: Lens.Lens' StartChatContactResponse (Core.Maybe Types.ParticipantId)
sccrrsParticipantId = Lens.field @"participantId"
{-# DEPRECATED sccrrsParticipantId "Use generic-lens or generic-optics with 'participantId' instead." #-}

-- | The token used by the chat participant to call <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection> . The participant token is valid for the lifetime of a chat participant.
--
-- /Note:/ Consider using 'participantToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrrsParticipantToken :: Lens.Lens' StartChatContactResponse (Core.Maybe Types.ParticipantToken)
sccrrsParticipantToken = Lens.field @"participantToken"
{-# DEPRECATED sccrrsParticipantToken "Use generic-lens or generic-optics with 'participantToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sccrrsResponseStatus :: Lens.Lens' StartChatContactResponse Core.Int
sccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
