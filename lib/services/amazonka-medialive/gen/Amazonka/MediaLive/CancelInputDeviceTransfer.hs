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
-- Module      : Amazonka.MediaLive.CancelInputDeviceTransfer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancel an input device transfer that you have requested.
module Amazonka.MediaLive.CancelInputDeviceTransfer
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for CancelInputDeviceTransferRequest
--
-- /See:/ 'newCancelInputDeviceTransfer' smart constructor.
data CancelInputDeviceTransfer = CancelInputDeviceTransfer'
  { -- | The unique ID of the input device to cancel. For example,
    -- hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CancelInputDeviceTransfer
newCancelInputDeviceTransfer pInputDeviceId_ =
  CancelInputDeviceTransfer'
    { inputDeviceId =
        pInputDeviceId_
    }

-- | The unique ID of the input device to cancel. For example,
-- hd-123456789abcdef.
cancelInputDeviceTransfer_inputDeviceId :: Lens.Lens' CancelInputDeviceTransfer Prelude.Text
cancelInputDeviceTransfer_inputDeviceId = Lens.lens (\CancelInputDeviceTransfer' {inputDeviceId} -> inputDeviceId) (\s@CancelInputDeviceTransfer' {} a -> s {inputDeviceId = a} :: CancelInputDeviceTransfer)

instance Core.AWSRequest CancelInputDeviceTransfer where
  type
    AWSResponse CancelInputDeviceTransfer =
      CancelInputDeviceTransferResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelInputDeviceTransferResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelInputDeviceTransfer where
  hashWithSalt _salt CancelInputDeviceTransfer' {..} =
    _salt `Prelude.hashWithSalt` inputDeviceId

instance Prelude.NFData CancelInputDeviceTransfer where
  rnf CancelInputDeviceTransfer' {..} =
    Prelude.rnf inputDeviceId

instance Core.ToHeaders CancelInputDeviceTransfer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelInputDeviceTransfer where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CancelInputDeviceTransfer where
  toPath CancelInputDeviceTransfer' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Core.toBS inputDeviceId,
        "/cancel"
      ]

instance Core.ToQuery CancelInputDeviceTransfer where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for CancelInputDeviceTransferResponse
--
-- /See:/ 'newCancelInputDeviceTransferResponse' smart constructor.
data CancelInputDeviceTransferResponse = CancelInputDeviceTransferResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CancelInputDeviceTransferResponse
newCancelInputDeviceTransferResponse pHttpStatus_ =
  CancelInputDeviceTransferResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelInputDeviceTransferResponse_httpStatus :: Lens.Lens' CancelInputDeviceTransferResponse Prelude.Int
cancelInputDeviceTransferResponse_httpStatus = Lens.lens (\CancelInputDeviceTransferResponse' {httpStatus} -> httpStatus) (\s@CancelInputDeviceTransferResponse' {} a -> s {httpStatus = a} :: CancelInputDeviceTransferResponse)

instance
  Prelude.NFData
    CancelInputDeviceTransferResponse
  where
  rnf CancelInputDeviceTransferResponse' {..} =
    Prelude.rnf httpStatus
