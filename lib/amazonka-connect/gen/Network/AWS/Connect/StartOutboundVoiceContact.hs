{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    sovcClientToken,
    sovcQueueId,
    sovcAttributes,
    sovcSourcePhoneNumber,
    sovcDestinationPhoneNumber,
    sovcContactFlowId,
    sovcInstanceId,

    -- * Destructuring the response
    StartOutboundVoiceContactResponse (..),
    mkStartOutboundVoiceContactResponse,

    -- ** Response lenses
    sovcrsContactId,
    sovcrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartOutboundVoiceContact' smart constructor.
data StartOutboundVoiceContact = StartOutboundVoiceContact'
  { clientToken ::
      Lude.Maybe Lude.Text,
    queueId :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    sourcePhoneNumber ::
      Lude.Maybe Lude.Text,
    destinationPhoneNumber :: Lude.Text,
    contactFlowId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOutboundVoiceContact' with the minimum fields required to make a request.
--
-- * 'attributes' - A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
-- * 'clientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
-- * 'contactFlowId' - The identifier of the contact flow for the outbound call. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
-- * 'destinationPhoneNumber' - The phone number of the customer, in E.164 format.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'queueId' - The queue for the call. If you specify a queue, the phone displayed for caller ID is the phone number specified in the queue. If you do not specify a queue, the queue defined in the contact flow is used. If you do not specify a queue, you must specify a source phone number.
-- * 'sourcePhoneNumber' - The phone number associated with the Amazon Connect instance, in E.164 format. If you do not specify a source phone number, you must specify a queue.
mkStartOutboundVoiceContact ::
  -- | 'destinationPhoneNumber'
  Lude.Text ->
  -- | 'contactFlowId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  StartOutboundVoiceContact
mkStartOutboundVoiceContact
  pDestinationPhoneNumber_
  pContactFlowId_
  pInstanceId_ =
    StartOutboundVoiceContact'
      { clientToken = Lude.Nothing,
        queueId = Lude.Nothing,
        attributes = Lude.Nothing,
        sourcePhoneNumber = Lude.Nothing,
        destinationPhoneNumber = pDestinationPhoneNumber_,
        contactFlowId = pContactFlowId_,
        instanceId = pInstanceId_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcClientToken :: Lens.Lens' StartOutboundVoiceContact (Lude.Maybe Lude.Text)
sovcClientToken = Lens.lens (clientToken :: StartOutboundVoiceContact -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: StartOutboundVoiceContact)
{-# DEPRECATED sovcClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The queue for the call. If you specify a queue, the phone displayed for caller ID is the phone number specified in the queue. If you do not specify a queue, the queue defined in the contact flow is used. If you do not specify a queue, you must specify a source phone number.
--
-- /Note:/ Consider using 'queueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcQueueId :: Lens.Lens' StartOutboundVoiceContact (Lude.Maybe Lude.Text)
sovcQueueId = Lens.lens (queueId :: StartOutboundVoiceContact -> Lude.Maybe Lude.Text) (\s a -> s {queueId = a} :: StartOutboundVoiceContact)
{-# DEPRECATED sovcQueueId "Use generic-lens or generic-optics with 'queueId' instead." #-}

-- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcAttributes :: Lens.Lens' StartOutboundVoiceContact (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sovcAttributes = Lens.lens (attributes :: StartOutboundVoiceContact -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: StartOutboundVoiceContact)
{-# DEPRECATED sovcAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The phone number associated with the Amazon Connect instance, in E.164 format. If you do not specify a source phone number, you must specify a queue.
--
-- /Note:/ Consider using 'sourcePhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcSourcePhoneNumber :: Lens.Lens' StartOutboundVoiceContact (Lude.Maybe Lude.Text)
sovcSourcePhoneNumber = Lens.lens (sourcePhoneNumber :: StartOutboundVoiceContact -> Lude.Maybe Lude.Text) (\s a -> s {sourcePhoneNumber = a} :: StartOutboundVoiceContact)
{-# DEPRECATED sovcSourcePhoneNumber "Use generic-lens or generic-optics with 'sourcePhoneNumber' instead." #-}

-- | The phone number of the customer, in E.164 format.
--
-- /Note:/ Consider using 'destinationPhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcDestinationPhoneNumber :: Lens.Lens' StartOutboundVoiceContact Lude.Text
sovcDestinationPhoneNumber = Lens.lens (destinationPhoneNumber :: StartOutboundVoiceContact -> Lude.Text) (\s a -> s {destinationPhoneNumber = a} :: StartOutboundVoiceContact)
{-# DEPRECATED sovcDestinationPhoneNumber "Use generic-lens or generic-optics with 'destinationPhoneNumber' instead." #-}

-- | The identifier of the contact flow for the outbound call. To see the ContactFlowId in the Amazon Connect console user interface, on the navigation menu go to __Routing__ , __Contact Flows__ . Choose the contact flow. On the contact flow page, under the name of the contact flow, choose __Show additional flow information__ . The ContactFlowId is the last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/contact-flow/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcContactFlowId :: Lens.Lens' StartOutboundVoiceContact Lude.Text
sovcContactFlowId = Lens.lens (contactFlowId :: StartOutboundVoiceContact -> Lude.Text) (\s a -> s {contactFlowId = a} :: StartOutboundVoiceContact)
{-# DEPRECATED sovcContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcInstanceId :: Lens.Lens' StartOutboundVoiceContact Lude.Text
sovcInstanceId = Lens.lens (instanceId :: StartOutboundVoiceContact -> Lude.Text) (\s a -> s {instanceId = a} :: StartOutboundVoiceContact)
{-# DEPRECATED sovcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest StartOutboundVoiceContact where
  type
    Rs StartOutboundVoiceContact =
      StartOutboundVoiceContactResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartOutboundVoiceContactResponse'
            Lude.<$> (x Lude..?> "ContactId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartOutboundVoiceContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartOutboundVoiceContact where
  toJSON StartOutboundVoiceContact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("QueueId" Lude..=) Lude.<$> queueId,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("SourcePhoneNumber" Lude..=) Lude.<$> sourcePhoneNumber,
            Lude.Just
              ("DestinationPhoneNumber" Lude..= destinationPhoneNumber),
            Lude.Just ("ContactFlowId" Lude..= contactFlowId),
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath StartOutboundVoiceContact where
  toPath = Lude.const "/contact/outbound-voice"

instance Lude.ToQuery StartOutboundVoiceContact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartOutboundVoiceContactResponse' smart constructor.
data StartOutboundVoiceContactResponse = StartOutboundVoiceContactResponse'
  { contactId ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOutboundVoiceContactResponse' with the minimum fields required to make a request.
--
-- * 'contactId' - The identifier of this contact within the Amazon Connect instance.
-- * 'responseStatus' - The response status code.
mkStartOutboundVoiceContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartOutboundVoiceContactResponse
mkStartOutboundVoiceContactResponse pResponseStatus_ =
  StartOutboundVoiceContactResponse'
    { contactId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of this contact within the Amazon Connect instance.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcrsContactId :: Lens.Lens' StartOutboundVoiceContactResponse (Lude.Maybe Lude.Text)
sovcrsContactId = Lens.lens (contactId :: StartOutboundVoiceContactResponse -> Lude.Maybe Lude.Text) (\s a -> s {contactId = a} :: StartOutboundVoiceContactResponse)
{-# DEPRECATED sovcrsContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sovcrsResponseStatus :: Lens.Lens' StartOutboundVoiceContactResponse Lude.Int
sovcrsResponseStatus = Lens.lens (responseStatus :: StartOutboundVoiceContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartOutboundVoiceContactResponse)
{-# DEPRECATED sovcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
