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
-- Module      : Network.AWS.MediaLive.AcceptInputDeviceTransfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept an incoming input device transfer. The ownership of the device
-- will transfer to your AWS account.
module Network.AWS.MediaLive.AcceptInputDeviceTransfer
  ( -- * Creating a Request
    AcceptInputDeviceTransfer (..),
    newAcceptInputDeviceTransfer,

    -- * Request Lenses
    acceptInputDeviceTransfer_inputDeviceId,

    -- * Destructuring the Response
    AcceptInputDeviceTransferResponse (..),
    newAcceptInputDeviceTransferResponse,

    -- * Response Lenses
    acceptInputDeviceTransferResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for AcceptInputDeviceTransferRequest
--
-- /See:/ 'newAcceptInputDeviceTransfer' smart constructor.
data AcceptInputDeviceTransfer = AcceptInputDeviceTransfer'
  { -- | The unique ID of the input device to accept. For example,
    -- hd-123456789abcdef.
    inputDeviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptInputDeviceTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDeviceId', 'acceptInputDeviceTransfer_inputDeviceId' - The unique ID of the input device to accept. For example,
-- hd-123456789abcdef.
newAcceptInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Core.Text ->
  AcceptInputDeviceTransfer
newAcceptInputDeviceTransfer pInputDeviceId_ =
  AcceptInputDeviceTransfer'
    { inputDeviceId =
        pInputDeviceId_
    }

-- | The unique ID of the input device to accept. For example,
-- hd-123456789abcdef.
acceptInputDeviceTransfer_inputDeviceId :: Lens.Lens' AcceptInputDeviceTransfer Core.Text
acceptInputDeviceTransfer_inputDeviceId = Lens.lens (\AcceptInputDeviceTransfer' {inputDeviceId} -> inputDeviceId) (\s@AcceptInputDeviceTransfer' {} a -> s {inputDeviceId = a} :: AcceptInputDeviceTransfer)

instance Core.AWSRequest AcceptInputDeviceTransfer where
  type
    AWSResponse AcceptInputDeviceTransfer =
      AcceptInputDeviceTransferResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptInputDeviceTransferResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptInputDeviceTransfer

instance Core.NFData AcceptInputDeviceTransfer

instance Core.ToHeaders AcceptInputDeviceTransfer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AcceptInputDeviceTransfer where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath AcceptInputDeviceTransfer where
  toPath AcceptInputDeviceTransfer' {..} =
    Core.mconcat
      [ "/prod/inputDevices/",
        Core.toBS inputDeviceId,
        "/accept"
      ]

instance Core.ToQuery AcceptInputDeviceTransfer where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for AcceptInputDeviceTransferResponse
--
-- /See:/ 'newAcceptInputDeviceTransferResponse' smart constructor.
data AcceptInputDeviceTransferResponse = AcceptInputDeviceTransferResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptInputDeviceTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptInputDeviceTransferResponse_httpStatus' - The response's http status code.
newAcceptInputDeviceTransferResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptInputDeviceTransferResponse
newAcceptInputDeviceTransferResponse pHttpStatus_ =
  AcceptInputDeviceTransferResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptInputDeviceTransferResponse_httpStatus :: Lens.Lens' AcceptInputDeviceTransferResponse Core.Int
acceptInputDeviceTransferResponse_httpStatus = Lens.lens (\AcceptInputDeviceTransferResponse' {httpStatus} -> httpStatus) (\s@AcceptInputDeviceTransferResponse' {} a -> s {httpStatus = a} :: AcceptInputDeviceTransferResponse)

instance
  Core.NFData
    AcceptInputDeviceTransferResponse
