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
-- Module      : Amazonka.SSMContacts.ActivateContactChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a contact\'s contact channel. Incident Manager can\'t engage a
-- contact until the contact channel has been activated.
module Amazonka.SSMContacts.ActivateContactChannel
  ( -- * Creating a Request
    ActivateContactChannel (..),
    newActivateContactChannel,

    -- * Request Lenses
    activateContactChannel_contactChannelId,
    activateContactChannel_activationCode,

    -- * Destructuring the Response
    ActivateContactChannelResponse (..),
    newActivateContactChannelResponse,

    -- * Response Lenses
    activateContactChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newActivateContactChannel' smart constructor.
data ActivateContactChannel = ActivateContactChannel'
  { -- | The Amazon Resource Name (ARN) of the contact channel.
    contactChannelId :: Prelude.Text,
    -- | The code sent to the contact channel when it was created in the contact.
    activationCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateContactChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactChannelId', 'activateContactChannel_contactChannelId' - The Amazon Resource Name (ARN) of the contact channel.
--
-- 'activationCode', 'activateContactChannel_activationCode' - The code sent to the contact channel when it was created in the contact.
newActivateContactChannel ::
  -- | 'contactChannelId'
  Prelude.Text ->
  -- | 'activationCode'
  Prelude.Text ->
  ActivateContactChannel
newActivateContactChannel
  pContactChannelId_
  pActivationCode_ =
    ActivateContactChannel'
      { contactChannelId =
          pContactChannelId_,
        activationCode = pActivationCode_
      }

-- | The Amazon Resource Name (ARN) of the contact channel.
activateContactChannel_contactChannelId :: Lens.Lens' ActivateContactChannel Prelude.Text
activateContactChannel_contactChannelId = Lens.lens (\ActivateContactChannel' {contactChannelId} -> contactChannelId) (\s@ActivateContactChannel' {} a -> s {contactChannelId = a} :: ActivateContactChannel)

-- | The code sent to the contact channel when it was created in the contact.
activateContactChannel_activationCode :: Lens.Lens' ActivateContactChannel Prelude.Text
activateContactChannel_activationCode = Lens.lens (\ActivateContactChannel' {activationCode} -> activationCode) (\s@ActivateContactChannel' {} a -> s {activationCode = a} :: ActivateContactChannel)

instance Core.AWSRequest ActivateContactChannel where
  type
    AWSResponse ActivateContactChannel =
      ActivateContactChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ActivateContactChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ActivateContactChannel where
  hashWithSalt _salt ActivateContactChannel' {..} =
    _salt `Prelude.hashWithSalt` contactChannelId
      `Prelude.hashWithSalt` activationCode

instance Prelude.NFData ActivateContactChannel where
  rnf ActivateContactChannel' {..} =
    Prelude.rnf contactChannelId
      `Prelude.seq` Prelude.rnf activationCode

instance Data.ToHeaders ActivateContactChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.ActivateContactChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ActivateContactChannel where
  toJSON ActivateContactChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContactChannelId" Data..= contactChannelId),
            Prelude.Just
              ("ActivationCode" Data..= activationCode)
          ]
      )

instance Data.ToPath ActivateContactChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery ActivateContactChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newActivateContactChannelResponse' smart constructor.
data ActivateContactChannelResponse = ActivateContactChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateContactChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'activateContactChannelResponse_httpStatus' - The response's http status code.
newActivateContactChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ActivateContactChannelResponse
newActivateContactChannelResponse pHttpStatus_ =
  ActivateContactChannelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
activateContactChannelResponse_httpStatus :: Lens.Lens' ActivateContactChannelResponse Prelude.Int
activateContactChannelResponse_httpStatus = Lens.lens (\ActivateContactChannelResponse' {httpStatus} -> httpStatus) (\s@ActivateContactChannelResponse' {} a -> s {httpStatus = a} :: ActivateContactChannelResponse)

instance
  Prelude.NFData
    ActivateContactChannelResponse
  where
  rnf ActivateContactChannelResponse' {..} =
    Prelude.rnf httpStatus
