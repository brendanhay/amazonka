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
-- Module      : Amazonka.SSMContacts.CreateContactChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A contact channel is the method that Incident Manager uses to engage
-- your contact.
module Amazonka.SSMContacts.CreateContactChannel
  ( -- * Creating a Request
    CreateContactChannel (..),
    newCreateContactChannel,

    -- * Request Lenses
    createContactChannel_idempotencyToken,
    createContactChannel_deferActivation,
    createContactChannel_contactId,
    createContactChannel_name,
    createContactChannel_type,
    createContactChannel_deliveryAddress,

    -- * Destructuring the Response
    CreateContactChannelResponse (..),
    newCreateContactChannelResponse,

    -- * Response Lenses
    createContactChannelResponse_httpStatus,
    createContactChannelResponse_contactChannelArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newCreateContactChannel' smart constructor.
data CreateContactChannel = CreateContactChannel'
  { -- | A token ensuring that the operation is called only once with the
    -- specified details.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | If you want to activate the channel at a later time, you can choose to
    -- defer activation. Incident Manager can\'t engage your contact channel
    -- until it has been activated.
    deferActivation :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the contact you are adding the contact
    -- channel to.
    contactId :: Prelude.Text,
    -- | The name of the contact channel.
    name :: Prelude.Text,
    -- | Incident Manager supports three types of contact channels:
    --
    -- -   @SMS@
    --
    -- -   @VOICE@
    --
    -- -   @EMAIL@
    type' :: ChannelType,
    -- | The details that Incident Manager uses when trying to engage the contact
    -- channel. The format is dependent on the type of the contact channel. The
    -- following are the expected formats:
    --
    -- -   SMS - \'+\' followed by the country code and phone number
    --
    -- -   VOICE - \'+\' followed by the country code and phone number
    --
    -- -   EMAIL - any standard email format
    deliveryAddress :: ContactChannelAddress
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'createContactChannel_idempotencyToken' - A token ensuring that the operation is called only once with the
-- specified details.
--
-- 'deferActivation', 'createContactChannel_deferActivation' - If you want to activate the channel at a later time, you can choose to
-- defer activation. Incident Manager can\'t engage your contact channel
-- until it has been activated.
--
-- 'contactId', 'createContactChannel_contactId' - The Amazon Resource Name (ARN) of the contact you are adding the contact
-- channel to.
--
-- 'name', 'createContactChannel_name' - The name of the contact channel.
--
-- 'type'', 'createContactChannel_type' - Incident Manager supports three types of contact channels:
--
-- -   @SMS@
--
-- -   @VOICE@
--
-- -   @EMAIL@
--
-- 'deliveryAddress', 'createContactChannel_deliveryAddress' - The details that Incident Manager uses when trying to engage the contact
-- channel. The format is dependent on the type of the contact channel. The
-- following are the expected formats:
--
-- -   SMS - \'+\' followed by the country code and phone number
--
-- -   VOICE - \'+\' followed by the country code and phone number
--
-- -   EMAIL - any standard email format
newCreateContactChannel ::
  -- | 'contactId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ChannelType ->
  -- | 'deliveryAddress'
  ContactChannelAddress ->
  CreateContactChannel
newCreateContactChannel
  pContactId_
  pName_
  pType_
  pDeliveryAddress_ =
    CreateContactChannel'
      { idempotencyToken =
          Prelude.Nothing,
        deferActivation = Prelude.Nothing,
        contactId = pContactId_,
        name = pName_,
        type' = pType_,
        deliveryAddress = pDeliveryAddress_
      }

-- | A token ensuring that the operation is called only once with the
-- specified details.
createContactChannel_idempotencyToken :: Lens.Lens' CreateContactChannel (Prelude.Maybe Prelude.Text)
createContactChannel_idempotencyToken = Lens.lens (\CreateContactChannel' {idempotencyToken} -> idempotencyToken) (\s@CreateContactChannel' {} a -> s {idempotencyToken = a} :: CreateContactChannel)

-- | If you want to activate the channel at a later time, you can choose to
-- defer activation. Incident Manager can\'t engage your contact channel
-- until it has been activated.
createContactChannel_deferActivation :: Lens.Lens' CreateContactChannel (Prelude.Maybe Prelude.Bool)
createContactChannel_deferActivation = Lens.lens (\CreateContactChannel' {deferActivation} -> deferActivation) (\s@CreateContactChannel' {} a -> s {deferActivation = a} :: CreateContactChannel)

-- | The Amazon Resource Name (ARN) of the contact you are adding the contact
-- channel to.
createContactChannel_contactId :: Lens.Lens' CreateContactChannel Prelude.Text
createContactChannel_contactId = Lens.lens (\CreateContactChannel' {contactId} -> contactId) (\s@CreateContactChannel' {} a -> s {contactId = a} :: CreateContactChannel)

-- | The name of the contact channel.
createContactChannel_name :: Lens.Lens' CreateContactChannel Prelude.Text
createContactChannel_name = Lens.lens (\CreateContactChannel' {name} -> name) (\s@CreateContactChannel' {} a -> s {name = a} :: CreateContactChannel)

-- | Incident Manager supports three types of contact channels:
--
-- -   @SMS@
--
-- -   @VOICE@
--
-- -   @EMAIL@
createContactChannel_type :: Lens.Lens' CreateContactChannel ChannelType
createContactChannel_type = Lens.lens (\CreateContactChannel' {type'} -> type') (\s@CreateContactChannel' {} a -> s {type' = a} :: CreateContactChannel)

-- | The details that Incident Manager uses when trying to engage the contact
-- channel. The format is dependent on the type of the contact channel. The
-- following are the expected formats:
--
-- -   SMS - \'+\' followed by the country code and phone number
--
-- -   VOICE - \'+\' followed by the country code and phone number
--
-- -   EMAIL - any standard email format
createContactChannel_deliveryAddress :: Lens.Lens' CreateContactChannel ContactChannelAddress
createContactChannel_deliveryAddress = Lens.lens (\CreateContactChannel' {deliveryAddress} -> deliveryAddress) (\s@CreateContactChannel' {} a -> s {deliveryAddress = a} :: CreateContactChannel)

instance Core.AWSRequest CreateContactChannel where
  type
    AWSResponse CreateContactChannel =
      CreateContactChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ContactChannelArn")
      )

instance Prelude.Hashable CreateContactChannel where
  hashWithSalt _salt CreateContactChannel' {..} =
    _salt `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` deferActivation
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` deliveryAddress

instance Prelude.NFData CreateContactChannel where
  rnf CreateContactChannel' {..} =
    Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf deferActivation
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf deliveryAddress

instance Data.ToHeaders CreateContactChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.CreateContactChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContactChannel where
  toJSON CreateContactChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("DeferActivation" Data..=)
              Prelude.<$> deferActivation,
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just
              ("DeliveryAddress" Data..= deliveryAddress)
          ]
      )

instance Data.ToPath CreateContactChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateContactChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactChannelResponse' smart constructor.
data CreateContactChannelResponse = CreateContactChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the contact channel.
    contactChannelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createContactChannelResponse_httpStatus' - The response's http status code.
--
-- 'contactChannelArn', 'createContactChannelResponse_contactChannelArn' - The Amazon Resource Name (ARN) of the contact channel.
newCreateContactChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'contactChannelArn'
  Prelude.Text ->
  CreateContactChannelResponse
newCreateContactChannelResponse
  pHttpStatus_
  pContactChannelArn_ =
    CreateContactChannelResponse'
      { httpStatus =
          pHttpStatus_,
        contactChannelArn = pContactChannelArn_
      }

-- | The response's http status code.
createContactChannelResponse_httpStatus :: Lens.Lens' CreateContactChannelResponse Prelude.Int
createContactChannelResponse_httpStatus = Lens.lens (\CreateContactChannelResponse' {httpStatus} -> httpStatus) (\s@CreateContactChannelResponse' {} a -> s {httpStatus = a} :: CreateContactChannelResponse)

-- | The Amazon Resource Name (ARN) of the contact channel.
createContactChannelResponse_contactChannelArn :: Lens.Lens' CreateContactChannelResponse Prelude.Text
createContactChannelResponse_contactChannelArn = Lens.lens (\CreateContactChannelResponse' {contactChannelArn} -> contactChannelArn) (\s@CreateContactChannelResponse' {} a -> s {contactChannelArn = a} :: CreateContactChannelResponse)

instance Prelude.NFData CreateContactChannelResponse where
  rnf CreateContactChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contactChannelArn
