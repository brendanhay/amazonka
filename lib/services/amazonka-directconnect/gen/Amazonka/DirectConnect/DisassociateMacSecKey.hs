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
-- Module      : Amazonka.DirectConnect.DisassociateMacSecKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between a MAC Security (MACsec) security key and
-- an Direct Connect dedicated connection.
module Amazonka.DirectConnect.DisassociateMacSecKey
  ( -- * Creating a Request
    DisassociateMacSecKey (..),
    newDisassociateMacSecKey,

    -- * Request Lenses
    disassociateMacSecKey_connectionId,
    disassociateMacSecKey_secretARN,

    -- * Destructuring the Response
    DisassociateMacSecKeyResponse (..),
    newDisassociateMacSecKeyResponse,

    -- * Response Lenses
    disassociateMacSecKeyResponse_macSecKeys,
    disassociateMacSecKeyResponse_connectionId,
    disassociateMacSecKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateMacSecKey' smart constructor.
data DisassociateMacSecKey = DisassociateMacSecKey'
  { -- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
    -- (dxlag-xxxx).
    --
    -- You can use DescribeConnections or DescribeLags to retrieve connection
    -- ID.
    connectionId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the MAC Security (MACsec) secret key.
    --
    -- You can use DescribeConnections to retrieve the ARN of the MAC Security
    -- (MACsec) secret key.
    secretARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMacSecKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'disassociateMacSecKey_connectionId' - The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
--
-- You can use DescribeConnections or DescribeLags to retrieve connection
-- ID.
--
-- 'secretARN', 'disassociateMacSecKey_secretARN' - The Amazon Resource Name (ARN) of the MAC Security (MACsec) secret key.
--
-- You can use DescribeConnections to retrieve the ARN of the MAC Security
-- (MACsec) secret key.
newDisassociateMacSecKey ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'secretARN'
  Prelude.Text ->
  DisassociateMacSecKey
newDisassociateMacSecKey pConnectionId_ pSecretARN_ =
  DisassociateMacSecKey'
    { connectionId =
        pConnectionId_,
      secretARN = pSecretARN_
    }

-- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
--
-- You can use DescribeConnections or DescribeLags to retrieve connection
-- ID.
disassociateMacSecKey_connectionId :: Lens.Lens' DisassociateMacSecKey Prelude.Text
disassociateMacSecKey_connectionId = Lens.lens (\DisassociateMacSecKey' {connectionId} -> connectionId) (\s@DisassociateMacSecKey' {} a -> s {connectionId = a} :: DisassociateMacSecKey)

-- | The Amazon Resource Name (ARN) of the MAC Security (MACsec) secret key.
--
-- You can use DescribeConnections to retrieve the ARN of the MAC Security
-- (MACsec) secret key.
disassociateMacSecKey_secretARN :: Lens.Lens' DisassociateMacSecKey Prelude.Text
disassociateMacSecKey_secretARN = Lens.lens (\DisassociateMacSecKey' {secretARN} -> secretARN) (\s@DisassociateMacSecKey' {} a -> s {secretARN = a} :: DisassociateMacSecKey)

instance Core.AWSRequest DisassociateMacSecKey where
  type
    AWSResponse DisassociateMacSecKey =
      DisassociateMacSecKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateMacSecKeyResponse'
            Prelude.<$> (x Core..?> "macSecKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "connectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateMacSecKey where
  hashWithSalt _salt DisassociateMacSecKey' {..} =
    _salt `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` secretARN

instance Prelude.NFData DisassociateMacSecKey where
  rnf DisassociateMacSecKey' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf secretARN

instance Core.ToHeaders DisassociateMacSecKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DisassociateMacSecKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateMacSecKey where
  toJSON DisassociateMacSecKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Core..= connectionId),
            Prelude.Just ("secretARN" Core..= secretARN)
          ]
      )

instance Core.ToPath DisassociateMacSecKey where
  toPath = Prelude.const "/"

instance Core.ToQuery DisassociateMacSecKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMacSecKeyResponse' smart constructor.
data DisassociateMacSecKeyResponse = DisassociateMacSecKeyResponse'
  { -- | The MAC Security (MACsec) security keys no longer associated with the
    -- dedicated connection.
    macSecKeys :: Prelude.Maybe [MacSecKey],
    -- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
    -- (dxlag-xxxx).
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMacSecKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macSecKeys', 'disassociateMacSecKeyResponse_macSecKeys' - The MAC Security (MACsec) security keys no longer associated with the
-- dedicated connection.
--
-- 'connectionId', 'disassociateMacSecKeyResponse_connectionId' - The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
--
-- 'httpStatus', 'disassociateMacSecKeyResponse_httpStatus' - The response's http status code.
newDisassociateMacSecKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateMacSecKeyResponse
newDisassociateMacSecKeyResponse pHttpStatus_ =
  DisassociateMacSecKeyResponse'
    { macSecKeys =
        Prelude.Nothing,
      connectionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The MAC Security (MACsec) security keys no longer associated with the
-- dedicated connection.
disassociateMacSecKeyResponse_macSecKeys :: Lens.Lens' DisassociateMacSecKeyResponse (Prelude.Maybe [MacSecKey])
disassociateMacSecKeyResponse_macSecKeys = Lens.lens (\DisassociateMacSecKeyResponse' {macSecKeys} -> macSecKeys) (\s@DisassociateMacSecKeyResponse' {} a -> s {macSecKeys = a} :: DisassociateMacSecKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
disassociateMacSecKeyResponse_connectionId :: Lens.Lens' DisassociateMacSecKeyResponse (Prelude.Maybe Prelude.Text)
disassociateMacSecKeyResponse_connectionId = Lens.lens (\DisassociateMacSecKeyResponse' {connectionId} -> connectionId) (\s@DisassociateMacSecKeyResponse' {} a -> s {connectionId = a} :: DisassociateMacSecKeyResponse)

-- | The response's http status code.
disassociateMacSecKeyResponse_httpStatus :: Lens.Lens' DisassociateMacSecKeyResponse Prelude.Int
disassociateMacSecKeyResponse_httpStatus = Lens.lens (\DisassociateMacSecKeyResponse' {httpStatus} -> httpStatus) (\s@DisassociateMacSecKeyResponse' {} a -> s {httpStatus = a} :: DisassociateMacSecKeyResponse)

instance Prelude.NFData DisassociateMacSecKeyResponse where
  rnf DisassociateMacSecKeyResponse' {..} =
    Prelude.rnf macSecKeys
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf httpStatus
