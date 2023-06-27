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
-- Module      : Amazonka.Connect.CreateParticipant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new participant into an on-going chat contact. For more
-- information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/chat-customize-flow.html Customize chat flow experiences by integrating custom participants>.
module Amazonka.Connect.CreateParticipant
  ( -- * Creating a Request
    CreateParticipant (..),
    newCreateParticipant,

    -- * Request Lenses
    createParticipant_clientToken,
    createParticipant_instanceId,
    createParticipant_contactId,
    createParticipant_participantDetails,

    -- * Destructuring the Response
    CreateParticipantResponse (..),
    newCreateParticipantResponse,

    -- * Response Lenses
    createParticipantResponse_participantCredentials,
    createParticipantResponse_participantId,
    createParticipantResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateParticipant' smart constructor.
data CreateParticipant = CreateParticipant'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact in this instance of Amazon Connect. Only
    -- contacts in the CHAT channel are supported.
    contactId :: Prelude.Text,
    -- | Information identifying the participant.
    --
    -- The only Valid value for @ParticipantRole@ is @CUSTOM_BOT@.
    --
    -- @DisplayName@ is __Required__.
    participantDetails :: ParticipantDetailsToAdd
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParticipant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createParticipant_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'instanceId', 'createParticipant_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'contactId', 'createParticipant_contactId' - The identifier of the contact in this instance of Amazon Connect. Only
-- contacts in the CHAT channel are supported.
--
-- 'participantDetails', 'createParticipant_participantDetails' - Information identifying the participant.
--
-- The only Valid value for @ParticipantRole@ is @CUSTOM_BOT@.
--
-- @DisplayName@ is __Required__.
newCreateParticipant ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'participantDetails'
  ParticipantDetailsToAdd ->
  CreateParticipant
newCreateParticipant
  pInstanceId_
  pContactId_
  pParticipantDetails_ =
    CreateParticipant'
      { clientToken = Prelude.Nothing,
        instanceId = pInstanceId_,
        contactId = pContactId_,
        participantDetails = pParticipantDetails_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createParticipant_clientToken :: Lens.Lens' CreateParticipant (Prelude.Maybe Prelude.Text)
createParticipant_clientToken = Lens.lens (\CreateParticipant' {clientToken} -> clientToken) (\s@CreateParticipant' {} a -> s {clientToken = a} :: CreateParticipant)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
createParticipant_instanceId :: Lens.Lens' CreateParticipant Prelude.Text
createParticipant_instanceId = Lens.lens (\CreateParticipant' {instanceId} -> instanceId) (\s@CreateParticipant' {} a -> s {instanceId = a} :: CreateParticipant)

-- | The identifier of the contact in this instance of Amazon Connect. Only
-- contacts in the CHAT channel are supported.
createParticipant_contactId :: Lens.Lens' CreateParticipant Prelude.Text
createParticipant_contactId = Lens.lens (\CreateParticipant' {contactId} -> contactId) (\s@CreateParticipant' {} a -> s {contactId = a} :: CreateParticipant)

-- | Information identifying the participant.
--
-- The only Valid value for @ParticipantRole@ is @CUSTOM_BOT@.
--
-- @DisplayName@ is __Required__.
createParticipant_participantDetails :: Lens.Lens' CreateParticipant ParticipantDetailsToAdd
createParticipant_participantDetails = Lens.lens (\CreateParticipant' {participantDetails} -> participantDetails) (\s@CreateParticipant' {} a -> s {participantDetails = a} :: CreateParticipant)

instance Core.AWSRequest CreateParticipant where
  type
    AWSResponse CreateParticipant =
      CreateParticipantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateParticipantResponse'
            Prelude.<$> (x Data..?> "ParticipantCredentials")
            Prelude.<*> (x Data..?> "ParticipantId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateParticipant where
  hashWithSalt _salt CreateParticipant' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` participantDetails

instance Prelude.NFData CreateParticipant where
  rnf CreateParticipant' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf participantDetails

instance Data.ToHeaders CreateParticipant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateParticipant where
  toJSON CreateParticipant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just
              ("ParticipantDetails" Data..= participantDetails)
          ]
      )

instance Data.ToPath CreateParticipant where
  toPath = Prelude.const "/contact/create-participant"

instance Data.ToQuery CreateParticipant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateParticipantResponse' smart constructor.
data CreateParticipantResponse = CreateParticipantResponse'
  { -- | The token used by the chat participant to call
    -- @CreateParticipantConnection@. The participant token is valid for the
    -- lifetime of a chat participant.
    participantCredentials :: Prelude.Maybe ParticipantTokenCredentials,
    -- | The identifier for a chat participant. The participantId for a chat
    -- participant is the same throughout the chat lifecycle.
    participantId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParticipantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantCredentials', 'createParticipantResponse_participantCredentials' - The token used by the chat participant to call
-- @CreateParticipantConnection@. The participant token is valid for the
-- lifetime of a chat participant.
--
-- 'participantId', 'createParticipantResponse_participantId' - The identifier for a chat participant. The participantId for a chat
-- participant is the same throughout the chat lifecycle.
--
-- 'httpStatus', 'createParticipantResponse_httpStatus' - The response's http status code.
newCreateParticipantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateParticipantResponse
newCreateParticipantResponse pHttpStatus_ =
  CreateParticipantResponse'
    { participantCredentials =
        Prelude.Nothing,
      participantId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used by the chat participant to call
-- @CreateParticipantConnection@. The participant token is valid for the
-- lifetime of a chat participant.
createParticipantResponse_participantCredentials :: Lens.Lens' CreateParticipantResponse (Prelude.Maybe ParticipantTokenCredentials)
createParticipantResponse_participantCredentials = Lens.lens (\CreateParticipantResponse' {participantCredentials} -> participantCredentials) (\s@CreateParticipantResponse' {} a -> s {participantCredentials = a} :: CreateParticipantResponse)

-- | The identifier for a chat participant. The participantId for a chat
-- participant is the same throughout the chat lifecycle.
createParticipantResponse_participantId :: Lens.Lens' CreateParticipantResponse (Prelude.Maybe Prelude.Text)
createParticipantResponse_participantId = Lens.lens (\CreateParticipantResponse' {participantId} -> participantId) (\s@CreateParticipantResponse' {} a -> s {participantId = a} :: CreateParticipantResponse)

-- | The response's http status code.
createParticipantResponse_httpStatus :: Lens.Lens' CreateParticipantResponse Prelude.Int
createParticipantResponse_httpStatus = Lens.lens (\CreateParticipantResponse' {httpStatus} -> httpStatus) (\s@CreateParticipantResponse' {} a -> s {httpStatus = a} :: CreateParticipantResponse)

instance Prelude.NFData CreateParticipantResponse where
  rnf CreateParticipantResponse' {..} =
    Prelude.rnf participantCredentials
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf httpStatus
