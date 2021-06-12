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
-- Module      : Network.AWS.StorageGateway.CancelArchival
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels archiving of a virtual tape to the virtual tape shelf (VTS)
-- after the archiving process is initiated. This operation is only
-- supported in the tape gateway type.
module Network.AWS.StorageGateway.CancelArchival
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | CancelArchivalInput
--
-- /See:/ 'newCancelArchival' smart constructor.
data CancelArchival = CancelArchival'
  { gatewayARN :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
    -- archiving for.
    tapeARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'tapeARN'
  Core.Text ->
  CancelArchival
newCancelArchival pGatewayARN_ pTapeARN_ =
  CancelArchival'
    { gatewayARN = pGatewayARN_,
      tapeARN = pTapeARN_
    }

-- | Undocumented member.
cancelArchival_gatewayARN :: Lens.Lens' CancelArchival Core.Text
cancelArchival_gatewayARN = Lens.lens (\CancelArchival' {gatewayARN} -> gatewayARN) (\s@CancelArchival' {} a -> s {gatewayARN = a} :: CancelArchival)

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- archiving for.
cancelArchival_tapeARN :: Lens.Lens' CancelArchival Core.Text
cancelArchival_tapeARN = Lens.lens (\CancelArchival' {tapeARN} -> tapeARN) (\s@CancelArchival' {} a -> s {tapeARN = a} :: CancelArchival)

instance Core.AWSRequest CancelArchival where
  type
    AWSResponse CancelArchival =
      CancelArchivalResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelArchivalResponse'
            Core.<$> (x Core..?> "TapeARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelArchival

instance Core.NFData CancelArchival

instance Core.ToHeaders CancelArchival where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CancelArchival" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CancelArchival where
  toJSON CancelArchival' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("TapeARN" Core..= tapeARN)
          ]
      )

instance Core.ToPath CancelArchival where
  toPath = Core.const "/"

instance Core.ToQuery CancelArchival where
  toQuery = Core.const Core.mempty

-- | CancelArchivalOutput
--
-- /See:/ 'newCancelArchivalResponse' smart constructor.
data CancelArchivalResponse = CancelArchivalResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which archiving
    -- was canceled.
    tapeARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CancelArchivalResponse
newCancelArchivalResponse pHttpStatus_ =
  CancelArchivalResponse'
    { tapeARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving
-- was canceled.
cancelArchivalResponse_tapeARN :: Lens.Lens' CancelArchivalResponse (Core.Maybe Core.Text)
cancelArchivalResponse_tapeARN = Lens.lens (\CancelArchivalResponse' {tapeARN} -> tapeARN) (\s@CancelArchivalResponse' {} a -> s {tapeARN = a} :: CancelArchivalResponse)

-- | The response's http status code.
cancelArchivalResponse_httpStatus :: Lens.Lens' CancelArchivalResponse Core.Int
cancelArchivalResponse_httpStatus = Lens.lens (\CancelArchivalResponse' {httpStatus} -> httpStatus) (\s@CancelArchivalResponse' {} a -> s {httpStatus = a} :: CancelArchivalResponse)

instance Core.NFData CancelArchivalResponse
