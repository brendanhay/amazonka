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
-- Module      : Amazonka.Connect.StartOutboundVoiceContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an outbound call to a contact, and then initiates the flow. It
-- performs the actions in the flow that\'s specified (in @ContactFlowId@).
--
-- Agents do not initiate the outbound API, which means that they do not
-- dial the contact. If the flow places an outbound call to a contact, and
-- then puts the contact in queue, the call is then routed to the agent,
-- like any other inbound case.
--
-- There is a 60-second dialing timeout for this operation. If the call is
-- not connected after 60 seconds, it fails.
--
-- UK numbers with a 447 prefix are not allowed by default. Before you can
-- dial these UK mobile numbers, you must submit a service quota increase
-- request. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html Amazon Connect Service Quotas>
-- in the /Amazon Connect Administrator Guide/.
--
-- Campaign calls are not allowed by default. Before you can make a call
-- with @TrafficType@ = @CAMPAIGN@, you must submit a service quota
-- increase request to the quota
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#outbound-communications-quotas Amazon Connect campaigns>.
module Amazonka.Connect.StartOutboundVoiceContact
  ( -- * Creating a Request
    StartOutboundVoiceContact (..),
    newStartOutboundVoiceContact,

    -- * Request Lenses
    startOutboundVoiceContact_answerMachineDetectionConfig,
    startOutboundVoiceContact_attributes,
    startOutboundVoiceContact_campaignId,
    startOutboundVoiceContact_clientToken,
    startOutboundVoiceContact_queueId,
    startOutboundVoiceContact_sourcePhoneNumber,
    startOutboundVoiceContact_trafficType,
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartOutboundVoiceContact' smart constructor.
data StartOutboundVoiceContact = StartOutboundVoiceContact'
  { -- | Configuration of the answering machine detection for this outbound call.
    answerMachineDetectionConfig :: Prelude.Maybe AnswerMachineDetectionConfig,
    -- | A custom key-value pair using an attribute map. The attributes are
    -- standard Amazon Connect attributes, and can be accessed in flows just
    -- like any other contact attributes.
    --
    -- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
    -- contact. Attribute keys can include only alphanumeric, dash, and
    -- underscore characters.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The campaign identifier of the outbound communication.
    campaignId :: Prelude.Maybe Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    -- The token is valid for 7 days after creation. If a contact is already
    -- started, the contact ID is returned.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The queue for the call. If you specify a queue, the phone displayed for
    -- caller ID is the phone number specified in the queue. If you do not
    -- specify a queue, the queue defined in the flow is used. If you do not
    -- specify a queue, you must specify a source phone number.
    queueId :: Prelude.Maybe Prelude.Text,
    -- | The phone number associated with the Amazon Connect instance, in E.164
    -- format. If you do not specify a source phone number, you must specify a
    -- queue.
    sourcePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | Denotes the class of traffic. Calls with different traffic types are
    -- handled differently by Amazon Connect. The default value is @GENERAL@.
    -- Use @CAMPAIGN@ if @EnableAnswerMachineDetection@ is set to @true@. For
    -- all other cases, use @GENERAL@.
    trafficType :: Prelude.Maybe TrafficType,
    -- | The phone number of the customer, in E.164 format.
    destinationPhoneNumber :: Prelude.Text,
    -- | The identifier of the flow for the outbound call. To see the
    -- ContactFlowId in the Amazon Connect console user interface, on the
    -- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
    -- On the flow page, under the name of the flow, choose __Show additional
    -- flow information__. The ContactFlowId is the last part of the ARN, shown
    -- here in bold:
    --
    -- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
    contactFlowId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartOutboundVoiceContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answerMachineDetectionConfig', 'startOutboundVoiceContact_answerMachineDetectionConfig' - Configuration of the answering machine detection for this outbound call.
--
-- 'attributes', 'startOutboundVoiceContact_attributes' - A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in flows just
-- like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
--
-- 'campaignId', 'startOutboundVoiceContact_campaignId' - The campaign identifier of the outbound communication.
--
-- 'clientToken', 'startOutboundVoiceContact_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
-- The token is valid for 7 days after creation. If a contact is already
-- started, the contact ID is returned.
--
-- 'queueId', 'startOutboundVoiceContact_queueId' - The queue for the call. If you specify a queue, the phone displayed for
-- caller ID is the phone number specified in the queue. If you do not
-- specify a queue, the queue defined in the flow is used. If you do not
-- specify a queue, you must specify a source phone number.
--
-- 'sourcePhoneNumber', 'startOutboundVoiceContact_sourcePhoneNumber' - The phone number associated with the Amazon Connect instance, in E.164
-- format. If you do not specify a source phone number, you must specify a
-- queue.
--
-- 'trafficType', 'startOutboundVoiceContact_trafficType' - Denotes the class of traffic. Calls with different traffic types are
-- handled differently by Amazon Connect. The default value is @GENERAL@.
-- Use @CAMPAIGN@ if @EnableAnswerMachineDetection@ is set to @true@. For
-- all other cases, use @GENERAL@.
--
-- 'destinationPhoneNumber', 'startOutboundVoiceContact_destinationPhoneNumber' - The phone number of the customer, in E.164 format.
--
-- 'contactFlowId', 'startOutboundVoiceContact_contactFlowId' - The identifier of the flow for the outbound call. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
-- On the flow page, under the name of the flow, choose __Show additional
-- flow information__. The ContactFlowId is the last part of the ARN, shown
-- here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
--
-- 'instanceId', 'startOutboundVoiceContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newStartOutboundVoiceContact ::
  -- | 'destinationPhoneNumber'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  StartOutboundVoiceContact
newStartOutboundVoiceContact
  pDestinationPhoneNumber_
  pContactFlowId_
  pInstanceId_ =
    StartOutboundVoiceContact'
      { answerMachineDetectionConfig =
          Prelude.Nothing,
        attributes = Prelude.Nothing,
        campaignId = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        queueId = Prelude.Nothing,
        sourcePhoneNumber = Prelude.Nothing,
        trafficType = Prelude.Nothing,
        destinationPhoneNumber =
          pDestinationPhoneNumber_,
        contactFlowId = pContactFlowId_,
        instanceId = pInstanceId_
      }

-- | Configuration of the answering machine detection for this outbound call.
startOutboundVoiceContact_answerMachineDetectionConfig :: Lens.Lens' StartOutboundVoiceContact (Prelude.Maybe AnswerMachineDetectionConfig)
startOutboundVoiceContact_answerMachineDetectionConfig = Lens.lens (\StartOutboundVoiceContact' {answerMachineDetectionConfig} -> answerMachineDetectionConfig) (\s@StartOutboundVoiceContact' {} a -> s {answerMachineDetectionConfig = a} :: StartOutboundVoiceContact)

-- | A custom key-value pair using an attribute map. The attributes are
-- standard Amazon Connect attributes, and can be accessed in flows just
-- like any other contact attributes.
--
-- There can be up to 32,768 UTF-8 bytes across all key-value pairs per
-- contact. Attribute keys can include only alphanumeric, dash, and
-- underscore characters.
startOutboundVoiceContact_attributes :: Lens.Lens' StartOutboundVoiceContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startOutboundVoiceContact_attributes = Lens.lens (\StartOutboundVoiceContact' {attributes} -> attributes) (\s@StartOutboundVoiceContact' {} a -> s {attributes = a} :: StartOutboundVoiceContact) Prelude.. Lens.mapping Lens.coerced

-- | The campaign identifier of the outbound communication.
startOutboundVoiceContact_campaignId :: Lens.Lens' StartOutboundVoiceContact (Prelude.Maybe Prelude.Text)
startOutboundVoiceContact_campaignId = Lens.lens (\StartOutboundVoiceContact' {campaignId} -> campaignId) (\s@StartOutboundVoiceContact' {} a -> s {campaignId = a} :: StartOutboundVoiceContact)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
-- The token is valid for 7 days after creation. If a contact is already
-- started, the contact ID is returned.
startOutboundVoiceContact_clientToken :: Lens.Lens' StartOutboundVoiceContact (Prelude.Maybe Prelude.Text)
startOutboundVoiceContact_clientToken = Lens.lens (\StartOutboundVoiceContact' {clientToken} -> clientToken) (\s@StartOutboundVoiceContact' {} a -> s {clientToken = a} :: StartOutboundVoiceContact)

-- | The queue for the call. If you specify a queue, the phone displayed for
-- caller ID is the phone number specified in the queue. If you do not
-- specify a queue, the queue defined in the flow is used. If you do not
-- specify a queue, you must specify a source phone number.
startOutboundVoiceContact_queueId :: Lens.Lens' StartOutboundVoiceContact (Prelude.Maybe Prelude.Text)
startOutboundVoiceContact_queueId = Lens.lens (\StartOutboundVoiceContact' {queueId} -> queueId) (\s@StartOutboundVoiceContact' {} a -> s {queueId = a} :: StartOutboundVoiceContact)

-- | The phone number associated with the Amazon Connect instance, in E.164
-- format. If you do not specify a source phone number, you must specify a
-- queue.
startOutboundVoiceContact_sourcePhoneNumber :: Lens.Lens' StartOutboundVoiceContact (Prelude.Maybe Prelude.Text)
startOutboundVoiceContact_sourcePhoneNumber = Lens.lens (\StartOutboundVoiceContact' {sourcePhoneNumber} -> sourcePhoneNumber) (\s@StartOutboundVoiceContact' {} a -> s {sourcePhoneNumber = a} :: StartOutboundVoiceContact)

-- | Denotes the class of traffic. Calls with different traffic types are
-- handled differently by Amazon Connect. The default value is @GENERAL@.
-- Use @CAMPAIGN@ if @EnableAnswerMachineDetection@ is set to @true@. For
-- all other cases, use @GENERAL@.
startOutboundVoiceContact_trafficType :: Lens.Lens' StartOutboundVoiceContact (Prelude.Maybe TrafficType)
startOutboundVoiceContact_trafficType = Lens.lens (\StartOutboundVoiceContact' {trafficType} -> trafficType) (\s@StartOutboundVoiceContact' {} a -> s {trafficType = a} :: StartOutboundVoiceContact)

-- | The phone number of the customer, in E.164 format.
startOutboundVoiceContact_destinationPhoneNumber :: Lens.Lens' StartOutboundVoiceContact Prelude.Text
startOutboundVoiceContact_destinationPhoneNumber = Lens.lens (\StartOutboundVoiceContact' {destinationPhoneNumber} -> destinationPhoneNumber) (\s@StartOutboundVoiceContact' {} a -> s {destinationPhoneNumber = a} :: StartOutboundVoiceContact)

-- | The identifier of the flow for the outbound call. To see the
-- ContactFlowId in the Amazon Connect console user interface, on the
-- navigation menu go to __Routing__, __Contact Flows__. Choose the flow.
-- On the flow page, under the name of the flow, choose __Show additional
-- flow information__. The ContactFlowId is the last part of the ARN, shown
-- here in bold:
--
-- arn:aws:connect:us-west-2:xxxxxxxxxxxx:instance\/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\/contact-flow\/__846ec553-a005-41c0-8341-xxxxxxxxxxxx__
startOutboundVoiceContact_contactFlowId :: Lens.Lens' StartOutboundVoiceContact Prelude.Text
startOutboundVoiceContact_contactFlowId = Lens.lens (\StartOutboundVoiceContact' {contactFlowId} -> contactFlowId) (\s@StartOutboundVoiceContact' {} a -> s {contactFlowId = a} :: StartOutboundVoiceContact)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
startOutboundVoiceContact_instanceId :: Lens.Lens' StartOutboundVoiceContact Prelude.Text
startOutboundVoiceContact_instanceId = Lens.lens (\StartOutboundVoiceContact' {instanceId} -> instanceId) (\s@StartOutboundVoiceContact' {} a -> s {instanceId = a} :: StartOutboundVoiceContact)

instance Core.AWSRequest StartOutboundVoiceContact where
  type
    AWSResponse StartOutboundVoiceContact =
      StartOutboundVoiceContactResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOutboundVoiceContactResponse'
            Prelude.<$> (x Data..?> "ContactId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartOutboundVoiceContact where
  hashWithSalt _salt StartOutboundVoiceContact' {..} =
    _salt
      `Prelude.hashWithSalt` answerMachineDetectionConfig
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` campaignId
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` queueId
      `Prelude.hashWithSalt` sourcePhoneNumber
      `Prelude.hashWithSalt` trafficType
      `Prelude.hashWithSalt` destinationPhoneNumber
      `Prelude.hashWithSalt` contactFlowId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData StartOutboundVoiceContact where
  rnf StartOutboundVoiceContact' {..} =
    Prelude.rnf answerMachineDetectionConfig `Prelude.seq`
      Prelude.rnf attributes `Prelude.seq`
        Prelude.rnf campaignId `Prelude.seq`
          Prelude.rnf clientToken `Prelude.seq`
            Prelude.rnf queueId `Prelude.seq`
              Prelude.rnf sourcePhoneNumber `Prelude.seq`
                Prelude.rnf trafficType `Prelude.seq`
                  Prelude.rnf destinationPhoneNumber `Prelude.seq`
                    Prelude.rnf contactFlowId `Prelude.seq`
                      Prelude.rnf instanceId

instance Data.ToHeaders StartOutboundVoiceContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartOutboundVoiceContact where
  toJSON StartOutboundVoiceContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnswerMachineDetectionConfig" Data..=)
              Prelude.<$> answerMachineDetectionConfig,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("CampaignId" Data..=) Prelude.<$> campaignId,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("QueueId" Data..=) Prelude.<$> queueId,
            ("SourcePhoneNumber" Data..=)
              Prelude.<$> sourcePhoneNumber,
            ("TrafficType" Data..=) Prelude.<$> trafficType,
            Prelude.Just
              ( "DestinationPhoneNumber"
                  Data..= destinationPhoneNumber
              ),
            Prelude.Just ("ContactFlowId" Data..= contactFlowId),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath StartOutboundVoiceContact where
  toPath = Prelude.const "/contact/outbound-voice"

instance Data.ToQuery StartOutboundVoiceContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartOutboundVoiceContactResponse' smart constructor.
data StartOutboundVoiceContactResponse = StartOutboundVoiceContactResponse'
  { -- | The identifier of this contact within the Amazon Connect instance.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartOutboundVoiceContactResponse
newStartOutboundVoiceContactResponse pHttpStatus_ =
  StartOutboundVoiceContactResponse'
    { contactId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of this contact within the Amazon Connect instance.
startOutboundVoiceContactResponse_contactId :: Lens.Lens' StartOutboundVoiceContactResponse (Prelude.Maybe Prelude.Text)
startOutboundVoiceContactResponse_contactId = Lens.lens (\StartOutboundVoiceContactResponse' {contactId} -> contactId) (\s@StartOutboundVoiceContactResponse' {} a -> s {contactId = a} :: StartOutboundVoiceContactResponse)

-- | The response's http status code.
startOutboundVoiceContactResponse_httpStatus :: Lens.Lens' StartOutboundVoiceContactResponse Prelude.Int
startOutboundVoiceContactResponse_httpStatus = Lens.lens (\StartOutboundVoiceContactResponse' {httpStatus} -> httpStatus) (\s@StartOutboundVoiceContactResponse' {} a -> s {httpStatus = a} :: StartOutboundVoiceContactResponse)

instance
  Prelude.NFData
    StartOutboundVoiceContactResponse
  where
  rnf StartOutboundVoiceContactResponse' {..} =
    Prelude.rnf contactId `Prelude.seq`
      Prelude.rnf httpStatus
