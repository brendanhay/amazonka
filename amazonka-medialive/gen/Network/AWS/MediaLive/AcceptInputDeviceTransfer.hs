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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for AcceptInputDeviceTransferRequest
--
-- /See:/ 'newAcceptInputDeviceTransfer' smart constructor.
data AcceptInputDeviceTransfer = AcceptInputDeviceTransfer'
  { -- | The unique ID of the input device to accept. For example,
    -- hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AcceptInputDeviceTransfer
newAcceptInputDeviceTransfer pInputDeviceId_ =
  AcceptInputDeviceTransfer'
    { inputDeviceId =
        pInputDeviceId_
    }

-- | The unique ID of the input device to accept. For example,
-- hd-123456789abcdef.
acceptInputDeviceTransfer_inputDeviceId :: Lens.Lens' AcceptInputDeviceTransfer Prelude.Text
acceptInputDeviceTransfer_inputDeviceId = Lens.lens (\AcceptInputDeviceTransfer' {inputDeviceId} -> inputDeviceId) (\s@AcceptInputDeviceTransfer' {} a -> s {inputDeviceId = a} :: AcceptInputDeviceTransfer)

instance Prelude.AWSRequest AcceptInputDeviceTransfer where
  type
    Rs AcceptInputDeviceTransfer =
      AcceptInputDeviceTransferResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptInputDeviceTransferResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptInputDeviceTransfer

instance Prelude.NFData AcceptInputDeviceTransfer

instance Prelude.ToHeaders AcceptInputDeviceTransfer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AcceptInputDeviceTransfer where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath AcceptInputDeviceTransfer where
  toPath AcceptInputDeviceTransfer' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Prelude.toBS inputDeviceId,
        "/accept"
      ]

instance Prelude.ToQuery AcceptInputDeviceTransfer where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for AcceptInputDeviceTransferResponse
--
-- /See:/ 'newAcceptInputDeviceTransferResponse' smart constructor.
data AcceptInputDeviceTransferResponse = AcceptInputDeviceTransferResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AcceptInputDeviceTransferResponse
newAcceptInputDeviceTransferResponse pHttpStatus_ =
  AcceptInputDeviceTransferResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptInputDeviceTransferResponse_httpStatus :: Lens.Lens' AcceptInputDeviceTransferResponse Prelude.Int
acceptInputDeviceTransferResponse_httpStatus = Lens.lens (\AcceptInputDeviceTransferResponse' {httpStatus} -> httpStatus) (\s@AcceptInputDeviceTransferResponse' {} a -> s {httpStatus = a} :: AcceptInputDeviceTransferResponse)

instance
  Prelude.NFData
    AcceptInputDeviceTransferResponse
