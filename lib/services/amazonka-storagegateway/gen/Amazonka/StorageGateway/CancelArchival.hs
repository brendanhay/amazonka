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
-- Module      : Amazonka.StorageGateway.CancelArchival
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels archiving of a virtual tape to the virtual tape shelf (VTS)
-- after the archiving process is initiated. This operation is only
-- supported in the tape gateway type.
module Amazonka.StorageGateway.CancelArchival
  ( -- * Creating a Request
    CancelArchival (..),
    newCancelArchival,

    -- * Request Lenses
    cancelArchival_gatewayARN,
    cancelArchival_tapeARN,

    -- * Destructuring the Response
    CancelArchivalResponse (..),
    newCancelArchivalResponse,

    -- * Response Lenses
    cancelArchivalResponse_tapeARN,
    cancelArchivalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | CancelArchivalInput
--
-- /See:/ 'newCancelArchival' smart constructor.
data CancelArchival = CancelArchival'
  { gatewayARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
    -- archiving for.
    tapeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelArchival' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'cancelArchival_gatewayARN' - Undocumented member.
--
-- 'tapeARN', 'cancelArchival_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- archiving for.
newCancelArchival ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'tapeARN'
  Prelude.Text ->
  CancelArchival
newCancelArchival pGatewayARN_ pTapeARN_ =
  CancelArchival'
    { gatewayARN = pGatewayARN_,
      tapeARN = pTapeARN_
    }

-- | Undocumented member.
cancelArchival_gatewayARN :: Lens.Lens' CancelArchival Prelude.Text
cancelArchival_gatewayARN = Lens.lens (\CancelArchival' {gatewayARN} -> gatewayARN) (\s@CancelArchival' {} a -> s {gatewayARN = a} :: CancelArchival)

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- archiving for.
cancelArchival_tapeARN :: Lens.Lens' CancelArchival Prelude.Text
cancelArchival_tapeARN = Lens.lens (\CancelArchival' {tapeARN} -> tapeARN) (\s@CancelArchival' {} a -> s {tapeARN = a} :: CancelArchival)

instance Core.AWSRequest CancelArchival where
  type
    AWSResponse CancelArchival =
      CancelArchivalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelArchivalResponse'
            Prelude.<$> (x Data..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelArchival where
  hashWithSalt _salt CancelArchival' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` tapeARN

instance Prelude.NFData CancelArchival where
  rnf CancelArchival' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf tapeARN

instance Data.ToHeaders CancelArchival where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.CancelArchival" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelArchival where
  toJSON CancelArchival' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just ("TapeARN" Data..= tapeARN)
          ]
      )

instance Data.ToPath CancelArchival where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelArchival where
  toQuery = Prelude.const Prelude.mempty

-- | CancelArchivalOutput
--
-- /See:/ 'newCancelArchivalResponse' smart constructor.
data CancelArchivalResponse = CancelArchivalResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which archiving
    -- was canceled.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelArchivalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'cancelArchivalResponse_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which archiving
-- was canceled.
--
-- 'httpStatus', 'cancelArchivalResponse_httpStatus' - The response's http status code.
newCancelArchivalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelArchivalResponse
newCancelArchivalResponse pHttpStatus_ =
  CancelArchivalResponse'
    { tapeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving
-- was canceled.
cancelArchivalResponse_tapeARN :: Lens.Lens' CancelArchivalResponse (Prelude.Maybe Prelude.Text)
cancelArchivalResponse_tapeARN = Lens.lens (\CancelArchivalResponse' {tapeARN} -> tapeARN) (\s@CancelArchivalResponse' {} a -> s {tapeARN = a} :: CancelArchivalResponse)

-- | The response's http status code.
cancelArchivalResponse_httpStatus :: Lens.Lens' CancelArchivalResponse Prelude.Int
cancelArchivalResponse_httpStatus = Lens.lens (\CancelArchivalResponse' {httpStatus} -> httpStatus) (\s@CancelArchivalResponse' {} a -> s {httpStatus = a} :: CancelArchivalResponse)

instance Prelude.NFData CancelArchivalResponse where
  rnf CancelArchivalResponse' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf httpStatus
