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
-- Module      : Network.AWS.MediaLive.CancelInputDeviceTransfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancel an input device transfer that you have requested.
module Network.AWS.MediaLive.CancelInputDeviceTransfer
  ( -- * Creating a Request
    CancelInputDeviceTransfer (..),
    newCancelInputDeviceTransfer,

    -- * Request Lenses
    cancelInputDeviceTransfer_inputDeviceId,

    -- * Destructuring the Response
    CancelInputDeviceTransferResponse (..),
    newCancelInputDeviceTransferResponse,

    -- * Response Lenses
    cancelInputDeviceTransferResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for CancelInputDeviceTransferRequest
--
-- /See:/ 'newCancelInputDeviceTransfer' smart constructor.
data CancelInputDeviceTransfer = CancelInputDeviceTransfer'
  { -- | The unique ID of the input device to cancel. For example,
    -- hd-123456789abcdef.
    inputDeviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelInputDeviceTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDeviceId', 'cancelInputDeviceTransfer_inputDeviceId' - The unique ID of the input device to cancel. For example,
-- hd-123456789abcdef.
newCancelInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Core.Text ->
  CancelInputDeviceTransfer
newCancelInputDeviceTransfer pInputDeviceId_ =
  CancelInputDeviceTransfer'
    { inputDeviceId =
        pInputDeviceId_
    }

-- | The unique ID of the input device to cancel. For example,
-- hd-123456789abcdef.
cancelInputDeviceTransfer_inputDeviceId :: Lens.Lens' CancelInputDeviceTransfer Core.Text
cancelInputDeviceTransfer_inputDeviceId = Lens.lens (\CancelInputDeviceTransfer' {inputDeviceId} -> inputDeviceId) (\s@CancelInputDeviceTransfer' {} a -> s {inputDeviceId = a} :: CancelInputDeviceTransfer)

instance Core.AWSRequest CancelInputDeviceTransfer where
  type
    AWSResponse CancelInputDeviceTransfer =
      CancelInputDeviceTransferResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelInputDeviceTransferResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelInputDeviceTransfer

instance Core.NFData CancelInputDeviceTransfer

instance Core.ToHeaders CancelInputDeviceTransfer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CancelInputDeviceTransfer where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath CancelInputDeviceTransfer where
  toPath CancelInputDeviceTransfer' {..} =
    Core.mconcat
      [ "/prod/inputDevices/",
        Core.toBS inputDeviceId,
        "/cancel"
      ]

instance Core.ToQuery CancelInputDeviceTransfer where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for CancelInputDeviceTransferResponse
--
-- /See:/ 'newCancelInputDeviceTransferResponse' smart constructor.
data CancelInputDeviceTransferResponse = CancelInputDeviceTransferResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelInputDeviceTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelInputDeviceTransferResponse_httpStatus' - The response's http status code.
newCancelInputDeviceTransferResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelInputDeviceTransferResponse
newCancelInputDeviceTransferResponse pHttpStatus_ =
  CancelInputDeviceTransferResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelInputDeviceTransferResponse_httpStatus :: Lens.Lens' CancelInputDeviceTransferResponse Core.Int
cancelInputDeviceTransferResponse_httpStatus = Lens.lens (\CancelInputDeviceTransferResponse' {httpStatus} -> httpStatus) (\s@CancelInputDeviceTransferResponse' {} a -> s {httpStatus = a} :: CancelInputDeviceTransferResponse)

instance
  Core.NFData
    CancelInputDeviceTransferResponse
