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
-- Module      : Amazonka.Organizations.DeclineHandshake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Declines a handshake request. This sets the handshake state to
-- @DECLINED@ and effectively deactivates the request.
--
-- This operation can be called only from the account that received the
-- handshake. The originator of the handshake can use CancelHandshake
-- instead. The originator can\'t reactivate a declined request, but can
-- reinitiate the process with a new handshake request.
--
-- After you decline a handshake, it continues to appear in the results of
-- relevant APIs for only 30 days. After that, it\'s deleted.
module Amazonka.Organizations.DeclineHandshake
  ( -- * Creating a Request
    DeclineHandshake (..),
    newDeclineHandshake,

    -- * Request Lenses
    declineHandshake_handshakeId,

    -- * Destructuring the Response
    DeclineHandshakeResponse (..),
    newDeclineHandshakeResponse,

    -- * Response Lenses
    declineHandshakeResponse_handshake,
    declineHandshakeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeclineHandshake' smart constructor.
data DeclineHandshake = DeclineHandshake'
  { -- | The unique identifier (ID) of the handshake that you want to decline.
    -- You can get the ID from the ListHandshakesForAccount operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
    -- string requires \"h-\" followed by from 8 to 32 lowercase letters or
    -- digits.
    handshakeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeclineHandshake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshakeId', 'declineHandshake_handshakeId' - The unique identifier (ID) of the handshake that you want to decline.
-- You can get the ID from the ListHandshakesForAccount operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
newDeclineHandshake ::
  -- | 'handshakeId'
  Prelude.Text ->
  DeclineHandshake
newDeclineHandshake pHandshakeId_ =
  DeclineHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want to decline.
-- You can get the ID from the ListHandshakesForAccount operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
declineHandshake_handshakeId :: Lens.Lens' DeclineHandshake Prelude.Text
declineHandshake_handshakeId = Lens.lens (\DeclineHandshake' {handshakeId} -> handshakeId) (\s@DeclineHandshake' {} a -> s {handshakeId = a} :: DeclineHandshake)

instance Core.AWSRequest DeclineHandshake where
  type
    AWSResponse DeclineHandshake =
      DeclineHandshakeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeclineHandshakeResponse'
            Prelude.<$> (x Data..?> "Handshake")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeclineHandshake where
  hashWithSalt _salt DeclineHandshake' {..} =
    _salt `Prelude.hashWithSalt` handshakeId

instance Prelude.NFData DeclineHandshake where
  rnf DeclineHandshake' {..} = Prelude.rnf handshakeId

instance Data.ToHeaders DeclineHandshake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.DeclineHandshake" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeclineHandshake where
  toJSON DeclineHandshake' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HandshakeId" Data..= handshakeId)]
      )

instance Data.ToPath DeclineHandshake where
  toPath = Prelude.const "/"

instance Data.ToQuery DeclineHandshake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeclineHandshakeResponse' smart constructor.
data DeclineHandshakeResponse = DeclineHandshakeResponse'
  { -- | A structure that contains details about the declined handshake. The
    -- state is updated to show the value @DECLINED@.
    handshake :: Prelude.Maybe Handshake,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeclineHandshakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshake', 'declineHandshakeResponse_handshake' - A structure that contains details about the declined handshake. The
-- state is updated to show the value @DECLINED@.
--
-- 'httpStatus', 'declineHandshakeResponse_httpStatus' - The response's http status code.
newDeclineHandshakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeclineHandshakeResponse
newDeclineHandshakeResponse pHttpStatus_ =
  DeclineHandshakeResponse'
    { handshake =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the declined handshake. The
-- state is updated to show the value @DECLINED@.
declineHandshakeResponse_handshake :: Lens.Lens' DeclineHandshakeResponse (Prelude.Maybe Handshake)
declineHandshakeResponse_handshake = Lens.lens (\DeclineHandshakeResponse' {handshake} -> handshake) (\s@DeclineHandshakeResponse' {} a -> s {handshake = a} :: DeclineHandshakeResponse)

-- | The response's http status code.
declineHandshakeResponse_httpStatus :: Lens.Lens' DeclineHandshakeResponse Prelude.Int
declineHandshakeResponse_httpStatus = Lens.lens (\DeclineHandshakeResponse' {httpStatus} -> httpStatus) (\s@DeclineHandshakeResponse' {} a -> s {httpStatus = a} :: DeclineHandshakeResponse)

instance Prelude.NFData DeclineHandshakeResponse where
  rnf DeclineHandshakeResponse' {..} =
    Prelude.rnf handshake
      `Prelude.seq` Prelude.rnf httpStatus
