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
-- Module      : Network.AWS.StorageGateway.RetrieveTapeArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an archived virtual tape from the virtual tape shelf (VTS) to
-- a tape gateway. Virtual tapes archived in the VTS are not associated
-- with any gateway. However after a tape is retrieved, it is associated
-- with a gateway, even though it is also listed in the VTS, that is,
-- archive. This operation is only supported in the tape gateway type.
--
-- Once a tape is successfully retrieved to a gateway, it cannot be
-- retrieved again to another gateway. You must archive the tape again
-- before you can retrieve it to another gateway. This operation is only
-- supported in the tape gateway type.
module Network.AWS.StorageGateway.RetrieveTapeArchive
  ( -- * Creating a Request
    RetrieveTapeArchive (..),
    newRetrieveTapeArchive,

    -- * Request Lenses
    retrieveTapeArchive_tapeARN,
    retrieveTapeArchive_gatewayARN,

    -- * Destructuring the Response
    RetrieveTapeArchiveResponse (..),
    newRetrieveTapeArchiveResponse,

    -- * Response Lenses
    retrieveTapeArchiveResponse_tapeARN,
    retrieveTapeArchiveResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | RetrieveTapeArchiveInput
--
-- /See:/ 'newRetrieveTapeArchive' smart constructor.
data RetrieveTapeArchive = RetrieveTapeArchive'
  { -- | The Amazon Resource Name (ARN) of the virtual tape you want to retrieve
    -- from the virtual tape shelf (VTS).
    tapeARN :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the gateway you want to retrieve the
    -- virtual tape to. Use the ListGateways operation to return a list of
    -- gateways for your account and AWS Region.
    --
    -- You retrieve archived virtual tapes to only one gateway and the gateway
    -- must be a tape gateway.
    gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RetrieveTapeArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'retrieveTapeArchive_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape you want to retrieve
-- from the virtual tape shelf (VTS).
--
-- 'gatewayARN', 'retrieveTapeArchive_gatewayARN' - The Amazon Resource Name (ARN) of the gateway you want to retrieve the
-- virtual tape to. Use the ListGateways operation to return a list of
-- gateways for your account and AWS Region.
--
-- You retrieve archived virtual tapes to only one gateway and the gateway
-- must be a tape gateway.
newRetrieveTapeArchive ::
  -- | 'tapeARN'
  Core.Text ->
  -- | 'gatewayARN'
  Core.Text ->
  RetrieveTapeArchive
newRetrieveTapeArchive pTapeARN_ pGatewayARN_ =
  RetrieveTapeArchive'
    { tapeARN = pTapeARN_,
      gatewayARN = pGatewayARN_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape you want to retrieve
-- from the virtual tape shelf (VTS).
retrieveTapeArchive_tapeARN :: Lens.Lens' RetrieveTapeArchive Core.Text
retrieveTapeArchive_tapeARN = Lens.lens (\RetrieveTapeArchive' {tapeARN} -> tapeARN) (\s@RetrieveTapeArchive' {} a -> s {tapeARN = a} :: RetrieveTapeArchive)

-- | The Amazon Resource Name (ARN) of the gateway you want to retrieve the
-- virtual tape to. Use the ListGateways operation to return a list of
-- gateways for your account and AWS Region.
--
-- You retrieve archived virtual tapes to only one gateway and the gateway
-- must be a tape gateway.
retrieveTapeArchive_gatewayARN :: Lens.Lens' RetrieveTapeArchive Core.Text
retrieveTapeArchive_gatewayARN = Lens.lens (\RetrieveTapeArchive' {gatewayARN} -> gatewayARN) (\s@RetrieveTapeArchive' {} a -> s {gatewayARN = a} :: RetrieveTapeArchive)

instance Core.AWSRequest RetrieveTapeArchive where
  type
    AWSResponse RetrieveTapeArchive =
      RetrieveTapeArchiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RetrieveTapeArchiveResponse'
            Core.<$> (x Core..?> "TapeARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RetrieveTapeArchive

instance Core.NFData RetrieveTapeArchive

instance Core.ToHeaders RetrieveTapeArchive where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.RetrieveTapeArchive" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RetrieveTapeArchive where
  toJSON RetrieveTapeArchive' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TapeARN" Core..= tapeARN),
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath RetrieveTapeArchive where
  toPath = Core.const "/"

instance Core.ToQuery RetrieveTapeArchive where
  toQuery = Core.const Core.mempty

-- | RetrieveTapeArchiveOutput
--
-- /See:/ 'newRetrieveTapeArchiveResponse' smart constructor.
data RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse'
  { -- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
    tapeARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RetrieveTapeArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'retrieveTapeArchiveResponse_tapeARN' - The Amazon Resource Name (ARN) of the retrieved virtual tape.
--
-- 'httpStatus', 'retrieveTapeArchiveResponse_httpStatus' - The response's http status code.
newRetrieveTapeArchiveResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RetrieveTapeArchiveResponse
newRetrieveTapeArchiveResponse pHttpStatus_ =
  RetrieveTapeArchiveResponse'
    { tapeARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
retrieveTapeArchiveResponse_tapeARN :: Lens.Lens' RetrieveTapeArchiveResponse (Core.Maybe Core.Text)
retrieveTapeArchiveResponse_tapeARN = Lens.lens (\RetrieveTapeArchiveResponse' {tapeARN} -> tapeARN) (\s@RetrieveTapeArchiveResponse' {} a -> s {tapeARN = a} :: RetrieveTapeArchiveResponse)

-- | The response's http status code.
retrieveTapeArchiveResponse_httpStatus :: Lens.Lens' RetrieveTapeArchiveResponse Core.Int
retrieveTapeArchiveResponse_httpStatus = Lens.lens (\RetrieveTapeArchiveResponse' {httpStatus} -> httpStatus) (\s@RetrieveTapeArchiveResponse' {} a -> s {httpStatus = a} :: RetrieveTapeArchiveResponse)

instance Core.NFData RetrieveTapeArchiveResponse
