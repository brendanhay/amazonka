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
-- Module      : Network.AWS.DirectConnect.AssociateMacSecKey
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DirectConnect.AssociateMacSecKey
  ( -- * Creating a Request
    AssociateMacSecKey (..),
    newAssociateMacSecKey,

    -- * Request Lenses
    associateMacSecKey_ckn,
    associateMacSecKey_cak,
    associateMacSecKey_secretARN,
    associateMacSecKey_connectionId,

    -- * Destructuring the Response
    AssociateMacSecKeyResponse (..),
    newAssociateMacSecKeyResponse,

    -- * Response Lenses
    associateMacSecKeyResponse_connectionId,
    associateMacSecKeyResponse_macSecKeys,
    associateMacSecKeyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateMacSecKey' smart constructor.
data AssociateMacSecKey = AssociateMacSecKey'
  { -- | The MAC Security (MACsec) CKN to associate with the dedicated
    -- connection.
    --
    -- You can create the CKN\/CAK pair using an industry standard tool.
    --
    -- The valid values are 64 hexadecimal characters (0-9, A-E).
    --
    -- If you use this request parameter, you must use the @cak@ request
    -- parameter and not use the @secretARN@ request parameter.
    ckn :: Prelude.Maybe Prelude.Text,
    -- | The MAC Security (MACsec) CAK to associate with the dedicated
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
    { ckn = Prelude.Nothing,
      cak = Prelude.Nothing,
      secretARN = Prelude.Nothing,
      connectionId = pConnectionId_
    }

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateMacSecKeyResponse'
            Prelude.<$> (x Core..?> "connectionId")
            Prelude.<*> (x Core..?> "macSecKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateMacSecKey

instance Prelude.NFData AssociateMacSecKey

instance Core.ToHeaders AssociateMacSecKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AssociateMacSecKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateMacSecKey where
  toJSON AssociateMacSecKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ckn" Core..=) Prelude.<$> ckn,
            ("cak" Core..=) Prelude.<$> cak,
            ("secretARN" Core..=) Prelude.<$> secretARN,
            Prelude.Just ("connectionId" Core..= connectionId)
          ]
      )

instance Core.ToPath AssociateMacSecKey where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateMacSecKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateMacSecKeyResponse' smart constructor.
data AssociateMacSecKeyResponse = AssociateMacSecKeyResponse'
  { -- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
    -- (dxlag-xxxx).
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The MAC Security (MACsec) security keys associated with the dedicated
    -- connection.
    macSecKeys :: Prelude.Maybe [MacSecKey],
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
-- 'connectionId', 'associateMacSecKeyResponse_connectionId' - The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
--
-- 'macSecKeys', 'associateMacSecKeyResponse_macSecKeys' - The MAC Security (MACsec) security keys associated with the dedicated
-- connection.
--
-- 'httpStatus', 'associateMacSecKeyResponse_httpStatus' - The response's http status code.
newAssociateMacSecKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateMacSecKeyResponse
newAssociateMacSecKeyResponse pHttpStatus_ =
  AssociateMacSecKeyResponse'
    { connectionId =
        Prelude.Nothing,
      macSecKeys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the dedicated connection (dxcon-xxxx), or the ID of the LAG
-- (dxlag-xxxx).
associateMacSecKeyResponse_connectionId :: Lens.Lens' AssociateMacSecKeyResponse (Prelude.Maybe Prelude.Text)
associateMacSecKeyResponse_connectionId = Lens.lens (\AssociateMacSecKeyResponse' {connectionId} -> connectionId) (\s@AssociateMacSecKeyResponse' {} a -> s {connectionId = a} :: AssociateMacSecKeyResponse)

-- | The MAC Security (MACsec) security keys associated with the dedicated
-- connection.
associateMacSecKeyResponse_macSecKeys :: Lens.Lens' AssociateMacSecKeyResponse (Prelude.Maybe [MacSecKey])
associateMacSecKeyResponse_macSecKeys = Lens.lens (\AssociateMacSecKeyResponse' {macSecKeys} -> macSecKeys) (\s@AssociateMacSecKeyResponse' {} a -> s {macSecKeys = a} :: AssociateMacSecKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associateMacSecKeyResponse_httpStatus :: Lens.Lens' AssociateMacSecKeyResponse Prelude.Int
associateMacSecKeyResponse_httpStatus = Lens.lens (\AssociateMacSecKeyResponse' {httpStatus} -> httpStatus) (\s@AssociateMacSecKeyResponse' {} a -> s {httpStatus = a} :: AssociateMacSecKeyResponse)

instance Prelude.NFData AssociateMacSecKeyResponse
