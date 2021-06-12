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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | CancelRetrievalInput
--
-- /See:/ 'newCancelRetrieval' smart constructor.
data CancelRetrieval = CancelRetrieval'
  { gatewayARN :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
    -- retrieval for.
    tapeARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'tapeARN'
  Core.Text ->
  CancelRetrieval
newCancelRetrieval pGatewayARN_ pTapeARN_ =
  CancelRetrieval'
    { gatewayARN = pGatewayARN_,
      tapeARN = pTapeARN_
    }

-- | Undocumented member.
cancelRetrieval_gatewayARN :: Lens.Lens' CancelRetrieval Core.Text
cancelRetrieval_gatewayARN = Lens.lens (\CancelRetrieval' {gatewayARN} -> gatewayARN) (\s@CancelRetrieval' {} a -> s {gatewayARN = a} :: CancelRetrieval)

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- retrieval for.
cancelRetrieval_tapeARN :: Lens.Lens' CancelRetrieval Core.Text
cancelRetrieval_tapeARN = Lens.lens (\CancelRetrieval' {tapeARN} -> tapeARN) (\s@CancelRetrieval' {} a -> s {tapeARN = a} :: CancelRetrieval)

instance Core.AWSRequest CancelRetrieval where
  type
    AWSResponse CancelRetrieval =
      CancelRetrievalResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelRetrievalResponse'
            Core.<$> (x Core..?> "TapeARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelRetrieval

instance Core.NFData CancelRetrieval

instance Core.ToHeaders CancelRetrieval where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CancelRetrieval" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CancelRetrieval where
  toJSON CancelRetrieval' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("TapeARN" Core..= tapeARN)
          ]
      )

instance Core.ToPath CancelRetrieval where
  toPath = Core.const "/"

instance Core.ToQuery CancelRetrieval where
  toQuery = Core.const Core.mempty

-- | CancelRetrievalOutput
--
-- /See:/ 'newCancelRetrievalResponse' smart constructor.
data CancelRetrievalResponse = CancelRetrievalResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval
    -- was canceled.
    tapeARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CancelRetrievalResponse
newCancelRetrievalResponse pHttpStatus_ =
  CancelRetrievalResponse'
    { tapeARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval
-- was canceled.
cancelRetrievalResponse_tapeARN :: Lens.Lens' CancelRetrievalResponse (Core.Maybe Core.Text)
cancelRetrievalResponse_tapeARN = Lens.lens (\CancelRetrievalResponse' {tapeARN} -> tapeARN) (\s@CancelRetrievalResponse' {} a -> s {tapeARN = a} :: CancelRetrievalResponse)

-- | The response's http status code.
cancelRetrievalResponse_httpStatus :: Lens.Lens' CancelRetrievalResponse Core.Int
cancelRetrievalResponse_httpStatus = Lens.lens (\CancelRetrievalResponse' {httpStatus} -> httpStatus) (\s@CancelRetrievalResponse' {} a -> s {httpStatus = a} :: CancelRetrievalResponse)

instance Core.NFData CancelRetrievalResponse
