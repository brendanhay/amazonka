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
-- Module      : Amazonka.Connect.StartChatContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a flow to start a new chat for the customer. Response of this
-- API provides a token required to obtain credentials from the
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>
-- API in the Amazon Connect Participant Service.
--
-- When a new chat contact is successfully created, clients must subscribe
-- to the participant’s connection for the created chat within 5 minutes.
-- This is achieved by invoking
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>
-- with WEBSOCKET and CONNECTION_CREDENTIALS.
--
-- A 429 error occurs in the following situations:
--
-- -   API rate limit is exceeded. API TPS throttling returns a
--     @TooManyRequests@ exception.
--
-- -   The
--     <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html quota for concurrent active chats>
--     is exceeded. Active chat throttling returns a
--     @LimitExceededException@.
--
-- If you use the @ChatDurationInMinutes@ parameter and receive a 400
-- error, your account may not support the ability to configure custom chat
-- durations. For more information, contact Amazon Web Services Support.
--
-- For more information about chat, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/chat.html Chat>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.StartChatContact
  ( -- * Creating a Request
    StartChatContact (..),
    newStartChatContact,

    -- * Request Lenses
    startChatContact_attributes,
    startChatContact_chatDurationInMinutes,
    startChatContact_clientToken,
    startChatContact_initialMessage,
    startChatContact_supportedMessagingContentTypes,
    startChatContact_instanceId,
    startChatContact_contactFlowId,
    startChatContact_participantDetails,

    -- * Destructuring the Response
    StartChatContactResponse (..),
    newStartChatContactResponse,

    -- * Response Lenses
    startChatContactResponse_contactId,
    startChatContactResponse_participantId,
    startChatContactResponse_participantToken,
    startChatContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartChatContact' smart constructor.
data StartChatContact = StartChatContact'
  { -- | A custom key-value pair using an attribute map. The attributes are
    -- standard Amazon Connect attributes. They can be accessed in flows just
    -- like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The total duration of the newly started chat session. If not specified,
    -- the chat session duration defaults to 25 hour. The minumum configurable
    -- time is 60 minutes. The maximum configurable time is 10,080 minutes (7
    -- days).
    chatDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The initial message to be sent to the newly created chat.
    initialMessage :: Prelude.Maybe ChatMessage,
    -- | The supported chat message content types. Content types can be
    -- text\/plain or both text\/plain and text\/markdown.
    supportedMessagingContentTypes :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow for initiating the chat. To see the
    -- ContactFlowId in the Amazon Connect console user interface, on the
    -- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
    -- On the flow page, under the name of the flow, choose __Show additional
    -- flow information__. The ContactFlowId is the last part of the ARN, shown
    -- here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Prelude.Text,
    -- | Information identifying the participant.
    participantDetails :: ParticipantDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChatContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'startChatContact_attributes' - A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes. They can be accessed in flows just
-- like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
--
-- 'chatDurationInMinutes', 'startChatContact_chatDurationInMinutes' - The total duration of the newly started chat session. If not specified,
-- the chat session duration defaults to 25 hour. The minumum configurable
-- time is 60 minutes. The maximum configurable time is 10,080 minutes (7
-- days).
--
-- 'clientToken', 'startChatContact_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'initialMessage', 'startChatContact_initialMessage' - The initial message to be sent to the newly created chat.
--
-- 'supportedMessagingContentTypes', 'startChatContact_supportedMessagingContentTypes' - The supported chat message content types. Content types can be
-- text\/plain or both text\/plain and text\/markdown.
--
-- 'instanceId', 'startChatContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowId', 'startChatContact_contactFlowId' - The identifier of the flow for initiating the chat. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
-- On the flow page, under the name of the flow, choose __Show additional
-- flow information__. The ContactFlowId is the last part of the ARN, shown
-- here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- 'participantDetails', 'startChatContact_participantDetails' - Information identifying the participant.
newStartChatContact ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  -- | 'participantDetails'
  ParticipantDetails ->
  StartChatContact
newStartChatContact
  pInstanceId_
  pContactFlowId_
  pParticipantDetails_ =
    StartChatContact'
      { attributes = Prelude.Nothing,
        chatDurationInMinutes = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        initialMessage = Prelude.Nothing,
        supportedMessagingContentTypes = Prelude.Nothing,
        instanceId = pInstanceId_,
        contactFlowId = pContactFlowId_,
        participantDetails = pParticipantDetails_
      }

-- | A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes. They can be accessed in flows just
-- like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
startChatContact_attributes :: Lens.Lens' StartChatContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startChatContact_attributes = Lens.lens (\StartChatContact' {attributes} -> attributes) (\s@StartChatContact' {} a -> s {attributes = a} :: StartChatContact) Prelude.. Lens.mapping Lens.coerced

-- | The total duration of the newly started chat session. If not specified,
-- the chat session duration defaults to 25 hour. The minumum configurable
-- time is 60 minutes. The maximum configurable time is 10,080 minutes (7
-- days).
startChatContact_chatDurationInMinutes :: Lens.Lens' StartChatContact (Prelude.Maybe Prelude.Natural)
startChatContact_chatDurationInMinutes = Lens.lens (\StartChatContact' {chatDurationInMinutes} -> chatDurationInMinutes) (\s@StartChatContact' {} a -> s {chatDurationInMinutes = a} :: StartChatContact)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
startChatContact_clientToken :: Lens.Lens' StartChatContact (Prelude.Maybe Prelude.Text)
startChatContact_clientToken = Lens.lens (\StartChatContact' {clientToken} -> clientToken) (\s@StartChatContact' {} a -> s {clientToken = a} :: StartChatContact)

-- | The initial message to be sent to the newly created chat.
startChatContact_initialMessage :: Lens.Lens' StartChatContact (Prelude.Maybe ChatMessage)
startChatContact_initialMessage = Lens.lens (\StartChatContact' {initialMessage} -> initialMessage) (\s@StartChatContact' {} a -> s {initialMessage = a} :: StartChatContact)

-- | The supported chat message content types. Content types can be
-- text\/plain or both text\/plain and text\/markdown.
startChatContact_supportedMessagingContentTypes :: Lens.Lens' StartChatContact (Prelude.Maybe [Prelude.Text])
startChatContact_supportedMessagingContentTypes = Lens.lens (\StartChatContact' {supportedMessagingContentTypes} -> supportedMessagingContentTypes) (\s@StartChatContact' {} a -> s {supportedMessagingContentTypes = a} :: StartChatContact) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
startChatContact_instanceId :: Lens.Lens' StartChatContact Prelude.Text
startChatContact_instanceId = Lens.lens (\StartChatContact' {instanceId} -> instanceId) (\s@StartChatContact' {} a -> s {instanceId = a} :: StartChatContact)

-- | The identifier of the flow for initiating the chat. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
-- On the flow page, under the name of the flow, choose __Show additional
-- flow information__. The ContactFlowId is the last part of the ARN, shown
-- here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
startChatContact_contactFlowId :: Lens.Lens' StartChatContact Prelude.Text
startChatContact_contactFlowId = Lens.lens (\StartChatContact' {contactFlowId} -> contactFlowId) (\s@StartChatContact' {} a -> s {contactFlowId = a} :: StartChatContact)

-- | Information identifying the participant.
startChatContact_participantDetails :: Lens.Lens' StartChatContact ParticipantDetails
startChatContact_participantDetails = Lens.lens (\StartChatContact' {participantDetails} -> participantDetails) (\s@StartChatContact' {} a -> s {participantDetails = a} :: StartChatContact)

instance Core.AWSRequest StartChatContact where
  type
    AWSResponse StartChatContact =
      StartChatContactResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChatContactResponse'
            Prelude.<$> (x Data..?> "ContactId")
            Prelude.<*> (x Data..?> "ParticipantId")
            Prelude.<*> (x Data..?> "ParticipantToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartChatContact where
  hashWithSalt _salt StartChatContact' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` chatDurationInMinutes
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` initialMessage
      `Prelude.hashWithSalt` supportedMessagingContentTypes
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactFlowId
      `Prelude.hashWithSalt` participantDetails

instance Prelude.NFData StartChatContact where
  rnf StartChatContact' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf chatDurationInMinutes
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf initialMessage
      `Prelude.seq` Prelude.rnf supportedMessagingContentTypes
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowId
      `Prelude.seq` Prelude.rnf participantDetails

instance Data.ToHeaders StartChatContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartChatContact where
  toJSON StartChatContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            ("ChatDurationInMinutes" Data..=)
              Prelude.<$> chatDurationInMinutes,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("InitialMessage" Data..=)
              Prelude.<$> initialMessage,
            ("SupportedMessagingContentTypes" Data..=)
              Prelude.<$> supportedMessagingContentTypes,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactFlowId" Data..= contactFlowId),
            Prelude.Just
              ("ParticipantDetails" Data..= participantDetails)
          ]
      )

instance Data.ToPath StartChatContact where
  toPath = Prelude.const "/contact/chat"

instance Data.ToQuery StartChatContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartChatContactResponse' smart constructor.
data StartChatContactResponse = StartChatContactResponse'
  { -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for a chat participant. The participantId for a chat
    -- participant is the same throughout the chat lifecycle.
    participantId :: Prelude.Maybe Prelude.Text,
    -- | The token used by the chat participant to call
    -- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>.
    -- The participant token is valid for the lifetime of a chat participant.
    participantToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChatContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'startChatContactResponse_contactId' - The identifier of this contact within the Amazon Connect instance.
--
-- 'participantId', 'startChatContactResponse_participantId' - The identifier for a chat participant. The participantId for a chat
-- participant is the same throughout the chat lifecycle.
--
-- 'participantToken', 'startChatContactResponse_participantToken' - The token used by the chat participant to call
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>.
-- The participant token is valid for the lifetime of a chat participant.
--
-- 'httpStatus', 'startChatContactResponse_httpStatus' - The response's http status code.
newStartChatContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartChatContactResponse
newStartChatContactResponse pHttpStatus_ =
  StartChatContactResponse'
    { contactId =
        Prelude.Nothing,
      participantId = Prelude.Nothing,
      participantToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of this contact within the Amazon Connect instance.
startChatContactResponse_contactId :: Lens.Lens' StartChatContactResponse (Prelude.Maybe Prelude.Text)
startChatContactResponse_contactId = Lens.lens (\StartChatContactResponse' {contactId} -> contactId) (\s@StartChatContactResponse' {} a -> s {contactId = a} :: StartChatContactResponse)

-- | The identifier for a chat participant. The participantId for a chat
-- participant is the same throughout the chat lifecycle.
startChatContactResponse_participantId :: Lens.Lens' StartChatContactResponse (Prelude.Maybe Prelude.Text)
startChatContactResponse_participantId = Lens.lens (\StartChatContactResponse' {participantId} -> participantId) (\s@StartChatContactResponse' {} a -> s {participantId = a} :: StartChatContactResponse)

-- | The token used by the chat participant to call
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>.
-- The participant token is valid for the lifetime of a chat participant.
startChatContactResponse_participantToken :: Lens.Lens' StartChatContactResponse (Prelude.Maybe Prelude.Text)
startChatContactResponse_participantToken = Lens.lens (\StartChatContactResponse' {participantToken} -> participantToken) (\s@StartChatContactResponse' {} a -> s {participantToken = a} :: StartChatContactResponse)

-- | The response's http status code.
startChatContactResponse_httpStatus :: Lens.Lens' StartChatContactResponse Prelude.Int
startChatContactResponse_httpStatus = Lens.lens (\StartChatContactResponse' {httpStatus} -> httpStatus) (\s@StartChatContactResponse' {} a -> s {httpStatus = a} :: StartChatContactResponse)

instance Prelude.NFData StartChatContactResponse where
  rnf StartChatContactResponse' {..} =
    Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf participantToken
      `Prelude.seq` Prelude.rnf httpStatus
