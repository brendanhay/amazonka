{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StartOutboundVoiceContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API places an outbound call to a contact, and then initiates the contact flow. It performs the actions in the contact flow that's specified (in @ContactFlowId@ ).
--
-- Agents are not involved in initiating the outbound API (that is, dialing the contact). If the contact flow places an outbound call to a contact, and then puts the contact in queue, that's when the call is routed to the agent, like any other inbound case.
-- There is a 60 second dialing timeout for this operation. If the call is not connected after 60 seconds, it fails.
module Network.AWS.Connect.StartOutboundVoiceContact
  ( -- * Creating a request
    StartOutboundVoiceContact (..),
    mkStartOutboundVoiceContact,

    -- ** Request lenses
    sovcDestinationPhoneNumber,
    sovcContactFlowId,
    sovcInstanceId,
    sovcAttributes,
    sovcClientToken,
    sovcQueueId,
    sovcSourcePhoneNumber,

    -- * Destructuring the response
    StartOutboundVoiceContactResponse (..),
    mkStartOutboundVoiceContactResponse,

    -- ** Response lenses
    sovcrrsContactId,
    sovcrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartOutboundVoiceContact' smart constructor.
data StartOutboundVoiceContact = StartOutboundVoiceContact'
  { -- | The phone number of the customer, in E.164 format.
    destinationPhoneNumber :: Types.PhoneNumber,
    -- | The identifier of the contact flow for the outbound call. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Types.ContactFlowId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The queue for the call. If you specify a queue, the phone displayed for caller ID is the phone number specified in the queue. If you do not specify a queue, the queue defined in the contact flow is used. If you do not specify a queue, you must specify a source phone number.
    queueId :: Core.Maybe Types.QueueId,
    -- | The phone number associated with the Amazon Connect instance, in E.164 format. If you do not specify a source phone number, you must specify a queue.
    sourcePhoneNumber :: Core.Maybe Types.PhoneNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartOutboundVoiceContact' value with any optional fields omitted.
mkStartOutboundVoiceContact ::
  -- | 'destinationPhoneNumber'
  Types.PhoneNumber ->
  -- | 'contactFlowId'
  Types.ContactFlowId ->
  -- | 'instanceId'
  Types.InstanceId ->
  StartOutboundVoiceContact
mkStartOutboundVoiceContact
  destinationPhoneNumber
  contactFlowId
  instanceId =
    StartOutboundVoiceContact'
      { destinationPhoneNumber,
        contactFlowId,
        instanceId,
        attributes = Core.Nothing,
        clientToken = Core.Nothing,
        queueId = Core.Nothing,
        sourcePhoneNumber = Core.Nothing
      }

-- | The phone number of the customer, in E.164 format.
--
-- /Note:/ Consider using 'destinationPhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcDestinationPhoneNumber :: Lens.Lens' StartOutboundVoiceContact Types.PhoneNumber
sovcDestinationPhoneNumber = Lens.field @"destinationPhoneNumber"
{-# DEPRECATED sovcDestinationPhoneNumber "Use generic-lens or generic-optics with 'destinationPhoneNumber' instead." #-}

-- | The identifier of the contact flow for the outbound call. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcContactFlowId :: Lens.Lens' StartOutboundVoiceContact Types.ContactFlowId
sovcContactFlowId = Lens.field @"contactFlowId"
{-# DEPRECATED sovcContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcInstanceId :: Lens.Lens' StartOutboundVoiceContact Types.InstanceId
sovcInstanceId = Lens.field @"instanceId"
{-# DEPRECATED sovcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcAttributes :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
sovcAttributes = Lens.field @"attributes"
{-# DEPRECATED sovcAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcClientToken :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe Types.ClientToken)
sovcClientToken = Lens.field @"clientToken"
{-# DEPRECATED sovcClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The queue for the call. If you specify a queue, the phone displayed for caller ID is the phone number specified in the queue. If you do not specify a queue, the queue defined in the contact flow is used. If you do not specify a queue, you must specify a source phone number.
--
-- /Note:/ Consider using 'queueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcQueueId :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe Types.QueueId)
sovcQueueId = Lens.field @"queueId"
{-# DEPRECATED sovcQueueId "Use generic-lens or generic-optics with 'queueId' instead." #-}

-- | The phone number associated with the Amazon Connect instance, in E.164 format. If you do not specify a source phone number, you must specify a queue.
--
-- /Note:/ Consider using 'sourcePhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcSourcePhoneNumber :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe Types.PhoneNumber)
sovcSourcePhoneNumber = Lens.field @"sourcePhoneNumber"
{-# DEPRECATED sovcSourcePhoneNumber "Use generic-lens or generic-optics with 'sourcePhoneNumber' instead." #-}

instance Core.FromJSON StartOutboundVoiceContact where
  toJSON StartOutboundVoiceContact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DestinationPhoneNumber" Core..= destinationPhoneNumber),
            Core.Just ("ContactFlowId" Core..= contactFlowId),
            Core.Just ("InstanceId" Core..= instanceId),
            ("Attributes" Core..=) Core.<$> attributes,
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("QueueId" Core..=) Core.<$> queueId,
            ("SourcePhoneNumber" Core..=) Core.<$> sourcePhoneNumber
          ]
      )

instance Core.AWSRequest StartOutboundVoiceContact where
  type
    Rs StartOutboundVoiceContact =
      StartOutboundVoiceContactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath "/contact/outbound-voice",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOutboundVoiceContactResponse'
            Core.<$> (x Core..:? "ContactId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartOutboundVoiceContactResponse' smart constructor.
data StartOutboundVoiceContactResponse = StartOutboundVoiceContactResponse'
  { -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Core.Maybe Types.ContactId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartOutboundVoiceContactResponse' value with any optional fields omitted.
mkStartOutboundVoiceContactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartOutboundVoiceContactResponse
mkStartOutboundVoiceContactResponse responseStatus =
  StartOutboundVoiceContactResponse'
    { contactId = Core.Nothing,
      responseStatus
    }

-- | The identifier of this contact within the Amazon Connect instance.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcrrsContactId :: Lens.Lens' StartOutboundVoiceContactResponse (Core.Maybe Types.ContactId)
sovcrrsContactId = Lens.field @"contactId"
{-# DEPRECATED sovcrrsContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcrrsResponseStatus :: Lens.Lens' StartOutboundVoiceContactResponse Core.Int
sovcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sovcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
