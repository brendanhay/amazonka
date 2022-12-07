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
-- Module      : Amazonka.ConnectParticipant.DisconnectParticipant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects a participant. Note that ConnectionToken is used for
-- invoking this API instead of ParticipantToken.
--
-- The Amazon Connect Participant Service APIs do not use
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 authentication>.
module Amazonka.ConnectParticipant.DisconnectParticipant
  ( -- * Creating a Request
    DisconnectParticipant (..),
    newDisconnectParticipant,

    -- * Request Lenses
    disconnectParticipant_clientToken,
    disconnectParticipant_connectionToken,

    -- * Destructuring the Response
    DisconnectParticipantResponse (..),
    newDisconnectParticipantResponse,

    -- * Response Lenses
    disconnectParticipantResponse_httpStatus,
  )
where

import Amazonka.ConnectParticipant.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisconnectParticipant' smart constructor.
data DisconnectParticipant = DisconnectParticipant'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The authentication token associated with the participant\'s connection.
    connectionToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectParticipant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disconnectParticipant_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'connectionToken', 'disconnectParticipant_connectionToken' - The authentication token associated with the participant\'s connection.
newDisconnectParticipant ::
  -- | 'connectionToken'
  Prelude.Text ->
  DisconnectParticipant
newDisconnectParticipant pConnectionToken_ =
  DisconnectParticipant'
    { clientToken =
        Prelude.Nothing,
      connectionToken = pConnectionToken_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
disconnectParticipant_clientToken :: Lens.Lens' DisconnectParticipant (Prelude.Maybe Prelude.Text)
disconnectParticipant_clientToken = Lens.lens (\DisconnectParticipant' {clientToken} -> clientToken) (\s@DisconnectParticipant' {} a -> s {clientToken = a} :: DisconnectParticipant)

-- | The authentication token associated with the participant\'s connection.
disconnectParticipant_connectionToken :: Lens.Lens' DisconnectParticipant Prelude.Text
disconnectParticipant_connectionToken = Lens.lens (\DisconnectParticipant' {connectionToken} -> connectionToken) (\s@DisconnectParticipant' {} a -> s {connectionToken = a} :: DisconnectParticipant)

instance Core.AWSRequest DisconnectParticipant where
  type
    AWSResponse DisconnectParticipant =
      DisconnectParticipantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisconnectParticipantResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisconnectParticipant where
  hashWithSalt _salt DisconnectParticipant' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` connectionToken

instance Prelude.NFData DisconnectParticipant where
  rnf DisconnectParticipant' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf connectionToken

instance Data.ToHeaders DisconnectParticipant where
  toHeaders DisconnectParticipant' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Data.=# connectionToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON DisconnectParticipant where
  toJSON DisconnectParticipant' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ClientToken" Data..=) Prelude.<$> clientToken]
      )

instance Data.ToPath DisconnectParticipant where
  toPath = Prelude.const "/participant/disconnect"

instance Data.ToQuery DisconnectParticipant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisconnectParticipantResponse' smart constructor.
data DisconnectParticipantResponse = DisconnectParticipantResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectParticipantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disconnectParticipantResponse_httpStatus' - The response's http status code.
newDisconnectParticipantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisconnectParticipantResponse
newDisconnectParticipantResponse pHttpStatus_ =
  DisconnectParticipantResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disconnectParticipantResponse_httpStatus :: Lens.Lens' DisconnectParticipantResponse Prelude.Int
disconnectParticipantResponse_httpStatus = Lens.lens (\DisconnectParticipantResponse' {httpStatus} -> httpStatus) (\s@DisconnectParticipantResponse' {} a -> s {httpStatus = a} :: DisconnectParticipantResponse)

instance Prelude.NFData DisconnectParticipantResponse where
  rnf DisconnectParticipantResponse' {..} =
    Prelude.rnf httpStatus
