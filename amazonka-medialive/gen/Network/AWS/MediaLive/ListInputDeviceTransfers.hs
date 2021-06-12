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
-- Module      : Network.AWS.MediaLive.ListInputDeviceTransfers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List input devices that are currently being transferred. List input
-- devices that you are transferring from your AWS account or input devices
-- that another AWS account is transferring to you.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputDeviceTransfers
  ( -- * Creating a Request
    ListInputDeviceTransfers (..),
    newListInputDeviceTransfers,

    -- * Request Lenses
    listInputDeviceTransfers_nextToken,
    listInputDeviceTransfers_maxResults,
    listInputDeviceTransfers_transferType,

    -- * Destructuring the Response
    ListInputDeviceTransfersResponse (..),
    newListInputDeviceTransfersResponse,

    -- * Response Lenses
    listInputDeviceTransfersResponse_nextToken,
    listInputDeviceTransfersResponse_inputDeviceTransfers,
    listInputDeviceTransfersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputDeviceTransfersRequest
--
-- /See:/ 'newListInputDeviceTransfers' smart constructor.
data ListInputDeviceTransfers = ListInputDeviceTransfers'
  { nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    transferType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInputDeviceTransfers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputDeviceTransfers_nextToken' - Undocumented member.
--
-- 'maxResults', 'listInputDeviceTransfers_maxResults' - Undocumented member.
--
-- 'transferType', 'listInputDeviceTransfers_transferType' - Undocumented member.
newListInputDeviceTransfers ::
  -- | 'transferType'
  Core.Text ->
  ListInputDeviceTransfers
newListInputDeviceTransfers pTransferType_ =
  ListInputDeviceTransfers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      transferType = pTransferType_
    }

-- | Undocumented member.
listInputDeviceTransfers_nextToken :: Lens.Lens' ListInputDeviceTransfers (Core.Maybe Core.Text)
listInputDeviceTransfers_nextToken = Lens.lens (\ListInputDeviceTransfers' {nextToken} -> nextToken) (\s@ListInputDeviceTransfers' {} a -> s {nextToken = a} :: ListInputDeviceTransfers)

-- | Undocumented member.
listInputDeviceTransfers_maxResults :: Lens.Lens' ListInputDeviceTransfers (Core.Maybe Core.Natural)
listInputDeviceTransfers_maxResults = Lens.lens (\ListInputDeviceTransfers' {maxResults} -> maxResults) (\s@ListInputDeviceTransfers' {} a -> s {maxResults = a} :: ListInputDeviceTransfers)

-- | Undocumented member.
listInputDeviceTransfers_transferType :: Lens.Lens' ListInputDeviceTransfers Core.Text
listInputDeviceTransfers_transferType = Lens.lens (\ListInputDeviceTransfers' {transferType} -> transferType) (\s@ListInputDeviceTransfers' {} a -> s {transferType = a} :: ListInputDeviceTransfers)

instance Core.AWSPager ListInputDeviceTransfers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInputDeviceTransfersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInputDeviceTransfersResponse_inputDeviceTransfers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInputDeviceTransfers_nextToken
          Lens..~ rs
          Lens.^? listInputDeviceTransfersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListInputDeviceTransfers where
  type
    AWSResponse ListInputDeviceTransfers =
      ListInputDeviceTransfersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputDeviceTransfersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "inputDeviceTransfers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInputDeviceTransfers

instance Core.NFData ListInputDeviceTransfers

instance Core.ToHeaders ListInputDeviceTransfers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListInputDeviceTransfers where
  toPath = Core.const "/prod/inputDeviceTransfers"

instance Core.ToQuery ListInputDeviceTransfers where
  toQuery ListInputDeviceTransfers' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "transferType" Core.=: transferType
      ]

-- | Placeholder documentation for ListInputDeviceTransfersResponse
--
-- /See:/ 'newListInputDeviceTransfersResponse' smart constructor.
data ListInputDeviceTransfersResponse = ListInputDeviceTransfersResponse'
  { -- | A token to get additional list results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of devices that you are transferring or are being transferred
    -- to you.
    inputDeviceTransfers :: Core.Maybe [TransferringInputDeviceSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInputDeviceTransfersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputDeviceTransfersResponse_nextToken' - A token to get additional list results.
--
-- 'inputDeviceTransfers', 'listInputDeviceTransfersResponse_inputDeviceTransfers' - The list of devices that you are transferring or are being transferred
-- to you.
--
-- 'httpStatus', 'listInputDeviceTransfersResponse_httpStatus' - The response's http status code.
newListInputDeviceTransfersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInputDeviceTransfersResponse
newListInputDeviceTransfersResponse pHttpStatus_ =
  ListInputDeviceTransfersResponse'
    { nextToken =
        Core.Nothing,
      inputDeviceTransfers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to get additional list results.
listInputDeviceTransfersResponse_nextToken :: Lens.Lens' ListInputDeviceTransfersResponse (Core.Maybe Core.Text)
listInputDeviceTransfersResponse_nextToken = Lens.lens (\ListInputDeviceTransfersResponse' {nextToken} -> nextToken) (\s@ListInputDeviceTransfersResponse' {} a -> s {nextToken = a} :: ListInputDeviceTransfersResponse)

-- | The list of devices that you are transferring or are being transferred
-- to you.
listInputDeviceTransfersResponse_inputDeviceTransfers :: Lens.Lens' ListInputDeviceTransfersResponse (Core.Maybe [TransferringInputDeviceSummary])
listInputDeviceTransfersResponse_inputDeviceTransfers = Lens.lens (\ListInputDeviceTransfersResponse' {inputDeviceTransfers} -> inputDeviceTransfers) (\s@ListInputDeviceTransfersResponse' {} a -> s {inputDeviceTransfers = a} :: ListInputDeviceTransfersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInputDeviceTransfersResponse_httpStatus :: Lens.Lens' ListInputDeviceTransfersResponse Core.Int
listInputDeviceTransfersResponse_httpStatus = Lens.lens (\ListInputDeviceTransfersResponse' {httpStatus} -> httpStatus) (\s@ListInputDeviceTransfersResponse' {} a -> s {httpStatus = a} :: ListInputDeviceTransfersResponse)

instance Core.NFData ListInputDeviceTransfersResponse
