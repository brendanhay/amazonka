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
-- Module      : Amazonka.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to
-- a gateway after the retrieval process is initiated. The virtual tape is
-- returned to the VTS. This operation is only supported in the tape
-- gateway type.
module Amazonka.StorageGateway.CancelRetrieval
  ( -- * Creating a Request
    CancelRetrieval (..),
    newCancelRetrieval,

    -- * Request Lenses
    cancelRetrieval_gatewayARN,
    cancelRetrieval_tapeARN,

    -- * Destructuring the Response
    CancelRetrievalResponse (..),
    newCancelRetrievalResponse,

    -- * Response Lenses
    cancelRetrievalResponse_tapeARN,
    cancelRetrievalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | CancelRetrievalInput
--
-- /See:/ 'newCancelRetrieval' smart constructor.
data CancelRetrieval = CancelRetrieval'
  { gatewayARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
    -- retrieval for.
    tapeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelRetrieval' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'cancelRetrieval_gatewayARN' - Undocumented member.
--
-- 'tapeARN', 'cancelRetrieval_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- retrieval for.
newCancelRetrieval ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'tapeARN'
  Prelude.Text ->
  CancelRetrieval
newCancelRetrieval pGatewayARN_ pTapeARN_ =
  CancelRetrieval'
    { gatewayARN = pGatewayARN_,
      tapeARN = pTapeARN_
    }

-- | Undocumented member.
cancelRetrieval_gatewayARN :: Lens.Lens' CancelRetrieval Prelude.Text
cancelRetrieval_gatewayARN = Lens.lens (\CancelRetrieval' {gatewayARN} -> gatewayARN) (\s@CancelRetrieval' {} a -> s {gatewayARN = a} :: CancelRetrieval)

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- retrieval for.
cancelRetrieval_tapeARN :: Lens.Lens' CancelRetrieval Prelude.Text
cancelRetrieval_tapeARN = Lens.lens (\CancelRetrieval' {tapeARN} -> tapeARN) (\s@CancelRetrieval' {} a -> s {tapeARN = a} :: CancelRetrieval)

instance Core.AWSRequest CancelRetrieval where
  type
    AWSResponse CancelRetrieval =
      CancelRetrievalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelRetrievalResponse'
            Prelude.<$> (x Core..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelRetrieval where
  hashWithSalt _salt CancelRetrieval' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` tapeARN

instance Prelude.NFData CancelRetrieval where
  rnf CancelRetrieval' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf tapeARN

instance Core.ToHeaders CancelRetrieval where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CancelRetrieval" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelRetrieval where
  toJSON CancelRetrieval' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Core..= gatewayARN),
            Prelude.Just ("TapeARN" Core..= tapeARN)
          ]
      )

instance Core.ToPath CancelRetrieval where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelRetrieval where
  toQuery = Prelude.const Prelude.mempty

-- | CancelRetrievalOutput
--
-- /See:/ 'newCancelRetrievalResponse' smart constructor.
data CancelRetrievalResponse = CancelRetrievalResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval
    -- was canceled.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelRetrievalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'cancelRetrievalResponse_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which retrieval
-- was canceled.
--
-- 'httpStatus', 'cancelRetrievalResponse_httpStatus' - The response's http status code.
newCancelRetrievalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelRetrievalResponse
newCancelRetrievalResponse pHttpStatus_ =
  CancelRetrievalResponse'
    { tapeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval
-- was canceled.
cancelRetrievalResponse_tapeARN :: Lens.Lens' CancelRetrievalResponse (Prelude.Maybe Prelude.Text)
cancelRetrievalResponse_tapeARN = Lens.lens (\CancelRetrievalResponse' {tapeARN} -> tapeARN) (\s@CancelRetrievalResponse' {} a -> s {tapeARN = a} :: CancelRetrievalResponse)

-- | The response's http status code.
cancelRetrievalResponse_httpStatus :: Lens.Lens' CancelRetrievalResponse Prelude.Int
cancelRetrievalResponse_httpStatus = Lens.lens (\CancelRetrievalResponse' {httpStatus} -> httpStatus) (\s@CancelRetrievalResponse' {} a -> s {httpStatus = a} :: CancelRetrievalResponse)

instance Prelude.NFData CancelRetrievalResponse where
  rnf CancelRetrievalResponse' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf httpStatus
