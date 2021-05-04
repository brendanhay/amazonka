{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to
-- a gateway after the retrieval process is initiated. The virtual tape is
-- returned to the VTS. This operation is only supported in the tape
-- gateway type.
module Network.AWS.StorageGateway.CancelRetrieval
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | CancelRetrievalInput
--
-- /See:/ 'newCancelRetrieval' smart constructor.
data CancelRetrieval = CancelRetrieval'
  { gatewayARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
    -- retrieval for.
    tapeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CancelRetrieval where
  type Rs CancelRetrieval = CancelRetrievalResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelRetrievalResponse'
            Prelude.<$> (x Prelude..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelRetrieval

instance Prelude.NFData CancelRetrieval

instance Prelude.ToHeaders CancelRetrieval where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.CancelRetrieval" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CancelRetrieval where
  toJSON CancelRetrieval' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just ("TapeARN" Prelude..= tapeARN)
          ]
      )

instance Prelude.ToPath CancelRetrieval where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelRetrieval where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CancelRetrievalResponse
