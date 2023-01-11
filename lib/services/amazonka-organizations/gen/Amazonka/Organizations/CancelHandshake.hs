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
-- Module      : Amazonka.Organizations.CancelHandshake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a handshake. Canceling a handshake sets the handshake state to
-- @CANCELED@.
--
-- This operation can be called only from the account that originated the
-- handshake. The recipient of the handshake can\'t cancel it, but can use
-- DeclineHandshake instead. After a handshake is canceled, the recipient
-- can no longer respond to that handshake.
--
-- After you cancel a handshake, it continues to appear in the results of
-- relevant APIs for only 30 days. After that, it\'s deleted.
module Amazonka.Organizations.CancelHandshake
  ( -- * Creating a Request
    CancelHandshake (..),
    newCancelHandshake,

    -- * Request Lenses
    cancelHandshake_handshakeId,

    -- * Destructuring the Response
    CancelHandshakeResponse (..),
    newCancelHandshakeResponse,

    -- * Response Lenses
    cancelHandshakeResponse_handshake,
    cancelHandshakeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelHandshake' smart constructor.
data CancelHandshake = CancelHandshake'
  { -- | The unique identifier (ID) of the handshake that you want to cancel. You
    -- can get the ID from the ListHandshakesForOrganization operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
    -- string requires \"h-\" followed by from 8 to 32 lowercase letters or
    -- digits.
    handshakeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelHandshake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshakeId', 'cancelHandshake_handshakeId' - The unique identifier (ID) of the handshake that you want to cancel. You
-- can get the ID from the ListHandshakesForOrganization operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
newCancelHandshake ::
  -- | 'handshakeId'
  Prelude.Text ->
  CancelHandshake
newCancelHandshake pHandshakeId_ =
  CancelHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want to cancel. You
-- can get the ID from the ListHandshakesForOrganization operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
cancelHandshake_handshakeId :: Lens.Lens' CancelHandshake Prelude.Text
cancelHandshake_handshakeId = Lens.lens (\CancelHandshake' {handshakeId} -> handshakeId) (\s@CancelHandshake' {} a -> s {handshakeId = a} :: CancelHandshake)

instance Core.AWSRequest CancelHandshake where
  type
    AWSResponse CancelHandshake =
      CancelHandshakeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelHandshakeResponse'
            Prelude.<$> (x Data..?> "Handshake")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelHandshake where
  hashWithSalt _salt CancelHandshake' {..} =
    _salt `Prelude.hashWithSalt` handshakeId

instance Prelude.NFData CancelHandshake where
  rnf CancelHandshake' {..} = Prelude.rnf handshakeId

instance Data.ToHeaders CancelHandshake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.CancelHandshake" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelHandshake where
  toJSON CancelHandshake' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HandshakeId" Data..= handshakeId)]
      )

instance Data.ToPath CancelHandshake where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelHandshake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelHandshakeResponse' smart constructor.
data CancelHandshakeResponse = CancelHandshakeResponse'
  { -- | A structure that contains details about the handshake that you canceled.
    handshake :: Prelude.Maybe Handshake,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelHandshakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshake', 'cancelHandshakeResponse_handshake' - A structure that contains details about the handshake that you canceled.
--
-- 'httpStatus', 'cancelHandshakeResponse_httpStatus' - The response's http status code.
newCancelHandshakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelHandshakeResponse
newCancelHandshakeResponse pHttpStatus_ =
  CancelHandshakeResponse'
    { handshake =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the handshake that you canceled.
cancelHandshakeResponse_handshake :: Lens.Lens' CancelHandshakeResponse (Prelude.Maybe Handshake)
cancelHandshakeResponse_handshake = Lens.lens (\CancelHandshakeResponse' {handshake} -> handshake) (\s@CancelHandshakeResponse' {} a -> s {handshake = a} :: CancelHandshakeResponse)

-- | The response's http status code.
cancelHandshakeResponse_httpStatus :: Lens.Lens' CancelHandshakeResponse Prelude.Int
cancelHandshakeResponse_httpStatus = Lens.lens (\CancelHandshakeResponse' {httpStatus} -> httpStatus) (\s@CancelHandshakeResponse' {} a -> s {httpStatus = a} :: CancelHandshakeResponse)

instance Prelude.NFData CancelHandshakeResponse where
  rnf CancelHandshakeResponse' {..} =
    Prelude.rnf handshake
      `Prelude.seq` Prelude.rnf httpStatus
