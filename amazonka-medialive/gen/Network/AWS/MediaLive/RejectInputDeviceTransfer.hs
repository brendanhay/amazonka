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
-- Module      : Network.AWS.MediaLive.RejectInputDeviceTransfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reject the transfer of the specified input device to your AWS account.
module Network.AWS.MediaLive.RejectInputDeviceTransfer
  ( -- * Creating a Request
    RejectInputDeviceTransfer (..),
    newRejectInputDeviceTransfer,

    -- * Request Lenses
    rejectInputDeviceTransfer_inputDeviceId,

    -- * Destructuring the Response
    RejectInputDeviceTransferResponse (..),
    newRejectInputDeviceTransferResponse,

    -- * Response Lenses
    rejectInputDeviceTransferResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for RejectInputDeviceTransferRequest
--
-- /See:/ 'newRejectInputDeviceTransfer' smart constructor.
data RejectInputDeviceTransfer = RejectInputDeviceTransfer'
  { -- | The unique ID of the input device to reject. For example,
    -- hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectInputDeviceTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDeviceId', 'rejectInputDeviceTransfer_inputDeviceId' - The unique ID of the input device to reject. For example,
-- hd-123456789abcdef.
newRejectInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Prelude.Text ->
  RejectInputDeviceTransfer
newRejectInputDeviceTransfer pInputDeviceId_ =
  RejectInputDeviceTransfer'
    { inputDeviceId =
        pInputDeviceId_
    }

-- | The unique ID of the input device to reject. For example,
-- hd-123456789abcdef.
rejectInputDeviceTransfer_inputDeviceId :: Lens.Lens' RejectInputDeviceTransfer Prelude.Text
rejectInputDeviceTransfer_inputDeviceId = Lens.lens (\RejectInputDeviceTransfer' {inputDeviceId} -> inputDeviceId) (\s@RejectInputDeviceTransfer' {} a -> s {inputDeviceId = a} :: RejectInputDeviceTransfer)

instance Prelude.AWSRequest RejectInputDeviceTransfer where
  type
    Rs RejectInputDeviceTransfer =
      RejectInputDeviceTransferResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectInputDeviceTransferResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectInputDeviceTransfer

instance Prelude.NFData RejectInputDeviceTransfer

instance Prelude.ToHeaders RejectInputDeviceTransfer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RejectInputDeviceTransfer where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath RejectInputDeviceTransfer where
  toPath RejectInputDeviceTransfer' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Prelude.toBS inputDeviceId,
        "/reject"
      ]

instance Prelude.ToQuery RejectInputDeviceTransfer where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for RejectInputDeviceTransferResponse
--
-- /See:/ 'newRejectInputDeviceTransferResponse' smart constructor.
data RejectInputDeviceTransferResponse = RejectInputDeviceTransferResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectInputDeviceTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectInputDeviceTransferResponse_httpStatus' - The response's http status code.
newRejectInputDeviceTransferResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectInputDeviceTransferResponse
newRejectInputDeviceTransferResponse pHttpStatus_ =
  RejectInputDeviceTransferResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectInputDeviceTransferResponse_httpStatus :: Lens.Lens' RejectInputDeviceTransferResponse Prelude.Int
rejectInputDeviceTransferResponse_httpStatus = Lens.lens (\RejectInputDeviceTransferResponse' {httpStatus} -> httpStatus) (\s@RejectInputDeviceTransferResponse' {} a -> s {httpStatus = a} :: RejectInputDeviceTransferResponse)

instance
  Prelude.NFData
    RejectInputDeviceTransferResponse
