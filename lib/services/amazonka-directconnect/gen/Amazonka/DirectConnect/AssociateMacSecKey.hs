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
-- Module      : Amazonka.DirectConnect.AssociateMacSecKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a MAC Security (MACsec) Connection Key Name (CKN)\/
-- Connectivity Association Key (CAK) pair with an Direct Connect dedicated
-- connection.
--
-- You must supply either the @secretARN,@ or the CKN\/CAK (@ckn@ and
-- @cak@) pair in the request.
--
-- For information about MAC Security (MACsec) key considerations, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/direct-connect-mac-sec-getting-started.html#mac-sec-key-consideration MACsec pre-shared CKN\/CAK key considerations>
-- in the /Direct Connect User Guide/.
module Amazonka.DirectConnect.AssociateMacSecKey
  ( -- * Creating a Request
    AssociateMacSecKey (..),
    newAssociateMacSecKey,

    -- * Request Lenses
    associateMacSecKey_cak,
    associateMacSecKey_secretARN,
    associateMacSecKey_ckn,
    associateMacSecKey_connectionId,

    -- * Destructuring the Response
    AssociateMacSecKeyResponse (..),
    newAssociateMacSecKeyResponse,

    -- * Response Lenses
    associateMacSecKeyResponse_macSecKeys,
    associateMacSecKeyResponse_connectionId,
    associateMacSecKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateMacSecKey' smart constructor.
data AssociateMacSecKey = AssociateMacSecKey'
  { -- | The MAC Security (MACsec) CAK to associate with the dedicated
    -- connection.
    --
    -- You can create the CKN\/CAK pair using an industry standard tool.
    --
    -- The valid values are 64 hexadecimal characters (0-9, A-E).
    --
    -- If you use this request parameter, you must use the @ckn@ request
    -- parameter and not use the @secretARN@ request parameter.
    cak :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the MAC Security (MACsec) secret key
    -- to associate with the dedicated connection.
    --
    -- You can use DescribeConnections or DescribeLags to retrieve the MAC
    -- Security (MACsec) secret key.
    --
    -- If you use this request parameter, you do not use the @ckn@ and @cak@
    -- request parameters.
    secretARN :: Prelude.Maybe Prelude.Text,
    -- | The MAC Security (MACsec) CKN to associate with the dedicated
    -- connection.
    --
    -- You can create the CKN\/CAK pair using an industry standard tool.
    --
    -- The valid values are 64 hexadecimal characters (0-9, A-E).
    --
    -- If you use this request parameter, you must use the @cak@ request
    -- parameter and not use the @secretARN@ request parameter.
    ckn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
    -- (dxlag-xxxx).
    --
    -- You can use DescribeConnections or DescribeLags to retrieve connection
    -- ID.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMacSecKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cak', 'associateMacSecKey_cak' - The MAC Security (MACsec) CAK to associate with the dedicated
-- connection.
--
-- You can create the CKN\/CAK pair using an industry standard tool.
--
-- The valid values are 64 hexadecimal characters (0-9, A-E).
--
-- If you use this request parameter, you must use the @ckn@ request
-- parameter and not use the @secretARN@ request parameter.
--
-- 'secretARN', 'associateMacSecKey_secretARN' - The Amazon Resource Name (ARN) of the MAC Security (MACsec) secret key
-- to associate with the dedicated connection.
--
-- You can use DescribeConnections or DescribeLags to retrieve the MAC
-- Security (MACsec) secret key.
--
-- If you use this request parameter, you do not use the @ckn@ and @cak@
-- request parameters.
--
-- 'ckn', 'associateMacSecKey_ckn' - The MAC Security (MACsec) CKN to associate with the dedicated
-- connection.
--
-- You can create the CKN\/CAK pair using an industry standard tool.
--
-- The valid values are 64 hexadecimal characters (0-9, A-E).
--
-- If you use this request parameter, you must use the @cak@ request
-- parameter and not use the @secretARN@ request parameter.
--
-- 'connectionId', 'associateMacSecKey_connectionId' - The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
--
-- You can use DescribeConnections or DescribeLags to retrieve connection
-- ID.
newAssociateMacSecKey ::
  -- | 'connectionId'
  Prelude.Text ->
  AssociateMacSecKey
newAssociateMacSecKey pConnectionId_ =
  AssociateMacSecKey'
    { cak = Prelude.Nothing,
      secretARN = Prelude.Nothing,
      ckn = Prelude.Nothing,
      connectionId = pConnectionId_
    }

-- | The MAC Security (MACsec) CAK to associate with the dedicated
-- connection.
--
-- You can create the CKN\/CAK pair using an industry standard tool.
--
-- The valid values are 64 hexadecimal characters (0-9, A-E).
--
-- If you use this request parameter, you must use the @ckn@ request
-- parameter and not use the @secretARN@ request parameter.
associateMacSecKey_cak :: Lens.Lens' AssociateMacSecKey (Prelude.Maybe Prelude.Text)
associateMacSecKey_cak = Lens.lens (\AssociateMacSecKey' {cak} -> cak) (\s@AssociateMacSecKey' {} a -> s {cak = a} :: AssociateMacSecKey)

-- | The Amazon Resource Name (ARN) of the MAC Security (MACsec) secret key
-- to associate with the dedicated connection.
--
-- You can use DescribeConnections or DescribeLags to retrieve the MAC
-- Security (MACsec) secret key.
--
-- If you use this request parameter, you do not use the @ckn@ and @cak@
-- request parameters.
associateMacSecKey_secretARN :: Lens.Lens' AssociateMacSecKey (Prelude.Maybe Prelude.Text)
associateMacSecKey_secretARN = Lens.lens (\AssociateMacSecKey' {secretARN} -> secretARN) (\s@AssociateMacSecKey' {} a -> s {secretARN = a} :: AssociateMacSecKey)

-- | The MAC Security (MACsec) CKN to associate with the dedicated
-- connection.
--
-- You can create the CKN\/CAK pair using an industry standard tool.
--
-- The valid values are 64 hexadecimal characters (0-9, A-E).
--
-- If you use this request parameter, you must use the @cak@ request
-- parameter and not use the @secretARN@ request parameter.
associateMacSecKey_ckn :: Lens.Lens' AssociateMacSecKey (Prelude.Maybe Prelude.Text)
associateMacSecKey_ckn = Lens.lens (\AssociateMacSecKey' {ckn} -> ckn) (\s@AssociateMacSecKey' {} a -> s {ckn = a} :: AssociateMacSecKey)

-- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
--
-- You can use DescribeConnections or DescribeLags to retrieve connection
-- ID.
associateMacSecKey_connectionId :: Lens.Lens' AssociateMacSecKey Prelude.Text
associateMacSecKey_connectionId = Lens.lens (\AssociateMacSecKey' {connectionId} -> connectionId) (\s@AssociateMacSecKey' {} a -> s {connectionId = a} :: AssociateMacSecKey)

instance Core.AWSRequest AssociateMacSecKey where
  type
    AWSResponse AssociateMacSecKey =
      AssociateMacSecKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateMacSecKeyResponse'
            Prelude.<$> (x Data..?> "macSecKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "connectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateMacSecKey where
  hashWithSalt _salt AssociateMacSecKey' {..} =
    _salt `Prelude.hashWithSalt` cak
      `Prelude.hashWithSalt` secretARN
      `Prelude.hashWithSalt` ckn
      `Prelude.hashWithSalt` connectionId

instance Prelude.NFData AssociateMacSecKey where
  rnf AssociateMacSecKey' {..} =
    Prelude.rnf cak
      `Prelude.seq` Prelude.rnf secretARN
      `Prelude.seq` Prelude.rnf ckn
      `Prelude.seq` Prelude.rnf connectionId

instance Data.ToHeaders AssociateMacSecKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.AssociateMacSecKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateMacSecKey where
  toJSON AssociateMacSecKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cak" Data..=) Prelude.<$> cak,
            ("secretARN" Data..=) Prelude.<$> secretARN,
            ("ckn" Data..=) Prelude.<$> ckn,
            Prelude.Just ("connectionId" Data..= connectionId)
          ]
      )

instance Data.ToPath AssociateMacSecKey where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateMacSecKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateMacSecKeyResponse' smart constructor.
data AssociateMacSecKeyResponse = AssociateMacSecKeyResponse'
  { -- | The MAC Security (MACsec) security keys associated with the dedicated
    -- connection.
    macSecKeys :: Prelude.Maybe [MacSecKey],
    -- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
    -- (dxlag-xxxx).
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMacSecKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macSecKeys', 'associateMacSecKeyResponse_macSecKeys' - The MAC Security (MACsec) security keys associated with the dedicated
-- connection.
--
-- 'connectionId', 'associateMacSecKeyResponse_connectionId' - The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
--
-- 'httpStatus', 'associateMacSecKeyResponse_httpStatus' - The response's http status code.
newAssociateMacSecKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateMacSecKeyResponse
newAssociateMacSecKeyResponse pHttpStatus_ =
  AssociateMacSecKeyResponse'
    { macSecKeys =
        Prelude.Nothing,
      connectionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The MAC Security (MACsec) security keys associated with the dedicated
-- connection.
associateMacSecKeyResponse_macSecKeys :: Lens.Lens' AssociateMacSecKeyResponse (Prelude.Maybe [MacSecKey])
associateMacSecKeyResponse_macSecKeys = Lens.lens (\AssociateMacSecKeyResponse' {macSecKeys} -> macSecKeys) (\s@AssociateMacSecKeyResponse' {} a -> s {macSecKeys = a} :: AssociateMacSecKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
associateMacSecKeyResponse_connectionId :: Lens.Lens' AssociateMacSecKeyResponse (Prelude.Maybe Prelude.Text)
associateMacSecKeyResponse_connectionId = Lens.lens (\AssociateMacSecKeyResponse' {connectionId} -> connectionId) (\s@AssociateMacSecKeyResponse' {} a -> s {connectionId = a} :: AssociateMacSecKeyResponse)

-- | The response's http status code.
associateMacSecKeyResponse_httpStatus :: Lens.Lens' AssociateMacSecKeyResponse Prelude.Int
associateMacSecKeyResponse_httpStatus = Lens.lens (\AssociateMacSecKeyResponse' {httpStatus} -> httpStatus) (\s@AssociateMacSecKeyResponse' {} a -> s {httpStatus = a} :: AssociateMacSecKeyResponse)

instance Prelude.NFData AssociateMacSecKeyResponse where
  rnf AssociateMacSecKeyResponse' {..} =
    Prelude.rnf macSecKeys
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf httpStatus
