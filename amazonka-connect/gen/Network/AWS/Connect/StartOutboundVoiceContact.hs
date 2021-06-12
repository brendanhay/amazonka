{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StartOutboundVoiceContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an outbound call to a contact, and then initiates the contact
-- flow. It performs the actions in the contact flow that\'s specified (in
-- @ContactFlowId@).
--
-- Agents do not initiate the outbound API, which means that they do not
-- dial the contact. If the contact flow places an outbound call to a
-- contact, and then puts the contact in queue, the call is then routed to
-- the agent, like any other inbound case.
--
-- There is a 60-second dialing timeout for this operation. If the call is
-- not connected after 60 seconds, it fails.
--
-- UK numbers with a 447 prefix are not allowed by default. Before you can
-- dial these UK mobile numbers, you must submit a service quota increase
-- request. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html Amazon Connect Service Quotas>
-- in the /Amazon Connect Administrator Guide/.
module Network.AWS.Connect.StartOutboundVoiceContact
  ( -- * Creating a Request
    StartOutboundVoiceContact (..),
    newStartOutboundVoiceContact,

    -- * Request Lenses
    startOutboundVoiceContact_queueId,
    startOutboundVoiceContact_sourcePhoneNumber,
    startOutboundVoiceContact_attributes,
    startOutboundVoiceContact_clientToken,
    startOutboundVoiceContact_destinationPhoneNumber,
    startOutboundVoiceContact_contactFlowId,
    startOutboundVoiceContact_instanceId,

    -- * Destructuring the Response
    StartOutboundVoiceContactResponse (..),
    newStartOutboundVoiceContactResponse,

    -- * Response Lenses
    startOutboundVoiceContactResponse_contactId,
    startOutboundVoiceContactResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartOutboundVoiceContact' smart constructor.
data StartOutboundVoiceContact = StartOutboundVoiceContact'
  { -- | The queue for the call. If you specify a queue, the phone displayed for
    -- caller ID is the phone number specified in the queue. If you do not
    -- specify a queue, the queue defined in the contact flow is used. If you
    -- do not specify a queue, you must specify a source phone number.
    queueId :: Core.Maybe Core.Text,
    -- | The phone number associated with the Amazon Connect instance, in E.164
    -- format. If you do not specify a source phone number, you must specify a
    -- queue.
    sourcePhoneNumber :: Core.Maybe Core.Text,
    -- | A custom key-value pair using an attribute map. The attributes are
    -- standard Amazon Connect attributes, and can be accessed in contact flows
    -- just like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. The token is valid for 7 days after
    -- creation. If a contact is already started, the contact ID is returned.
    -- If the contact is disconnected, a new contact is started.
    clientToken :: Core.Maybe Core.Text,
    -- | The phone number of the customer, in E.164 format.
    destinationPhoneNumber :: Core.Text,
    -- | The identifier of the contact flow for the outbound call. To see the
    -- ContactFlowId in the Amazon Connect console user interface, on the
    -- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
    -- flow. On the contact flow page, under the name of the contact flow,
    -- choose __Show additional flow information__. The ContactFlowId is the
    -- last part of the ARN, shown here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartOutboundVoiceContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueId', 'startOutboundVoiceContact_queueId' - The queue for the call. If you specify a queue, the phone displayed for
-- caller ID is the phone number specified in the queue. If you do not
-- specify a queue, the queue defined in the contact flow is used. If you
-- do not specify a queue, you must specify a source phone number.
--
-- 'sourcePhoneNumber', 'startOutboundVoiceContact_sourcePhoneNumber' - The phone number associated with the Amazon Connect instance, in E.164
-- format. If you do not specify a source phone number, you must specify a
-- queue.
--
-- 'attributes', 'startOutboundVoiceContact_attributes' - A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in contact flows
-- just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
--
-- 'clientToken', 'startOutboundVoiceContact_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. The token is valid for 7 days after
-- creation. If a contact is already started, the contact ID is returned.
-- If the contact is disconnected, a new contact is started.
--
-- 'destinationPhoneNumber', 'startOutboundVoiceContact_destinationPhoneNumber' - The phone number of the customer, in E.164 format.
--
-- 'contactFlowId', 'startOutboundVoiceContact_contactFlowId' - The identifier of the contact flow for the outbound call. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
-- flow. On the contact flow page, under the name of the contact flow,
-- choose __Show additional flow information__. The ContactFlowId is the
-- last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- 'instanceId', 'startOutboundVoiceContact_instanceId' - The identifier of the Amazon Connect instance.
newStartOutboundVoiceContact ::
  -- | 'destinationPhoneNumber'
  Core.Text ->
  -- | 'contactFlowId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  StartOutboundVoiceContact
newStartOutboundVoiceContact
  pDestinationPhoneNumber_
  pContactFlowId_
  pInstanceId_ =
    StartOutboundVoiceContact'
      { queueId = Core.Nothing,
        sourcePhoneNumber = Core.Nothing,
        attributes = Core.Nothing,
        clientToken = Core.Nothing,
        destinationPhoneNumber =
          pDestinationPhoneNumber_,
        contactFlowId = pContactFlowId_,
        instanceId = pInstanceId_
      }

-- | The queue for the call. If you specify a queue, the phone displayed for
-- caller ID is the phone number specified in the queue. If you do not
-- specify a queue, the queue defined in the contact flow is used. If you
-- do not specify a queue, you must specify a source phone number.
startOutboundVoiceContact_queueId :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe Core.Text)
startOutboundVoiceContact_queueId = Lens.lens (\StartOutboundVoiceContact' {queueId} -> queueId) (\s@StartOutboundVoiceContact' {} a -> s {queueId = a} :: StartOutboundVoiceContact)

-- | The phone number associated with the Amazon Connect instance, in E.164
-- format. If you do not specify a source phone number, you must specify a
-- queue.
startOutboundVoiceContact_sourcePhoneNumber :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe Core.Text)
startOutboundVoiceContact_sourcePhoneNumber = Lens.lens (\StartOutboundVoiceContact' {sourcePhoneNumber} -> sourcePhoneNumber) (\s@StartOutboundVoiceContact' {} a -> s {sourcePhoneNumber = a} :: StartOutboundVoiceContact)

-- | A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in contact flows
-- just like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
startOutboundVoiceContact_attributes :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe (Core.HashMap Core.Text Core.Text))
startOutboundVoiceContact_attributes = Lens.lens (\StartOutboundVoiceContact' {attributes} -> attributes) (\s@StartOutboundVoiceContact' {} a -> s {attributes = a} :: StartOutboundVoiceContact) Core.. Lens.mapping Lens._Coerce

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. The token is valid for 7 days after
-- creation. If a contact is already started, the contact ID is returned.
-- If the contact is disconnected, a new contact is started.
startOutboundVoiceContact_clientToken :: Lens.Lens' StartOutboundVoiceContact (Core.Maybe Core.Text)
startOutboundVoiceContact_clientToken = Lens.lens (\StartOutboundVoiceContact' {clientToken} -> clientToken) (\s@StartOutboundVoiceContact' {} a -> s {clientToken = a} :: StartOutboundVoiceContact)

-- | The phone number of the customer, in E.164 format.
startOutboundVoiceContact_destinationPhoneNumber :: Lens.Lens' StartOutboundVoiceContact Core.Text
startOutboundVoiceContact_destinationPhoneNumber = Lens.lens (\StartOutboundVoiceContact' {destinationPhoneNumber} -> destinationPhoneNumber) (\s@StartOutboundVoiceContact' {} a -> s {destinationPhoneNumber = a} :: StartOutboundVoiceContact)

-- | The identifier of the contact flow for the outbound call. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the contact
-- flow. On the contact flow page, under the name of the contact flow,
-- choose __Show additional flow information__. The ContactFlowId is the
-- last part of the ARN, shown here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
startOutboundVoiceContact_contactFlowId :: Lens.Lens' StartOutboundVoiceContact Core.Text
startOutboundVoiceContact_contactFlowId = Lens.lens (\StartOutboundVoiceContact' {contactFlowId} -> contactFlowId) (\s@StartOutboundVoiceContact' {} a -> s {contactFlowId = a} :: StartOutboundVoiceContact)

-- | The identifier of the Amazon Connect instance.
startOutboundVoiceContact_instanceId :: Lens.Lens' StartOutboundVoiceContact Core.Text
startOutboundVoiceContact_instanceId = Lens.lens (\StartOutboundVoiceContact' {instanceId} -> instanceId) (\s@StartOutboundVoiceContact' {} a -> s {instanceId = a} :: StartOutboundVoiceContact)

instance Core.AWSRequest StartOutboundVoiceContact where
  type
    AWSResponse StartOutboundVoiceContact =
      StartOutboundVoiceContactResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOutboundVoiceContactResponse'
            Core.<$> (x Core..?> "ContactId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartOutboundVoiceContact

instance Core.NFData StartOutboundVoiceContact

instance Core.ToHeaders StartOutboundVoiceContact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartOutboundVoiceContact where
  toJSON StartOutboundVoiceContact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("QueueId" Core..=) Core.<$> queueId,
            ("SourcePhoneNumber" Core..=)
              Core.<$> sourcePhoneNumber,
            ("Attributes" Core..=) Core.<$> attributes,
            ("ClientToken" Core..=) Core.<$> clientToken,
            Core.Just
              ( "DestinationPhoneNumber"
                  Core..= destinationPhoneNumber
              ),
            Core.Just ("ContactFlowId" Core..= contactFlowId),
            Core.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath StartOutboundVoiceContact where
  toPath = Core.const "/contact/outbound-voice"

instance Core.ToQuery StartOutboundVoiceContact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartOutboundVoiceContactResponse' smart constructor.
data StartOutboundVoiceContactResponse = StartOutboundVoiceContactResponse'
  { -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartOutboundVoiceContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'startOutboundVoiceContactResponse_contactId' - The identifier of this contact within the Amazon Connect instance.
--
-- 'httpStatus', 'startOutboundVoiceContactResponse_httpStatus' - The response's http status code.
newStartOutboundVoiceContactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartOutboundVoiceContactResponse
newStartOutboundVoiceContactResponse pHttpStatus_ =
  StartOutboundVoiceContactResponse'
    { contactId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of this contact within the Amazon Connect instance.
startOutboundVoiceContactResponse_contactId :: Lens.Lens' StartOutboundVoiceContactResponse (Core.Maybe Core.Text)
startOutboundVoiceContactResponse_contactId = Lens.lens (\StartOutboundVoiceContactResponse' {contactId} -> contactId) (\s@StartOutboundVoiceContactResponse' {} a -> s {contactId = a} :: StartOutboundVoiceContactResponse)

-- | The response's http status code.
startOutboundVoiceContactResponse_httpStatus :: Lens.Lens' StartOutboundVoiceContactResponse Core.Int
startOutboundVoiceContactResponse_httpStatus = Lens.lens (\StartOutboundVoiceContactResponse' {httpStatus} -> httpStatus) (\s@StartOutboundVoiceContactResponse' {} a -> s {httpStatus = a} :: StartOutboundVoiceContactResponse)

instance
  Core.NFData
    StartOutboundVoiceContactResponse
