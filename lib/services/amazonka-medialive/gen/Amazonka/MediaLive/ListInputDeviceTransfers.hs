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
-- Module      : Amazonka.MediaLive.ListInputDeviceTransfers
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.MediaLive.ListInputDeviceTransfers
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for ListInputDeviceTransfersRequest
--
-- /See:/ 'newListInputDeviceTransfers' smart constructor.
data ListInputDeviceTransfers = ListInputDeviceTransfers'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    transferType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListInputDeviceTransfers
newListInputDeviceTransfers pTransferType_ =
  ListInputDeviceTransfers'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      transferType = pTransferType_
    }

-- | Undocumented member.
listInputDeviceTransfers_nextToken :: Lens.Lens' ListInputDeviceTransfers (Prelude.Maybe Prelude.Text)
listInputDeviceTransfers_nextToken = Lens.lens (\ListInputDeviceTransfers' {nextToken} -> nextToken) (\s@ListInputDeviceTransfers' {} a -> s {nextToken = a} :: ListInputDeviceTransfers)

-- | Undocumented member.
listInputDeviceTransfers_maxResults :: Lens.Lens' ListInputDeviceTransfers (Prelude.Maybe Prelude.Natural)
listInputDeviceTransfers_maxResults = Lens.lens (\ListInputDeviceTransfers' {maxResults} -> maxResults) (\s@ListInputDeviceTransfers' {} a -> s {maxResults = a} :: ListInputDeviceTransfers)

-- | Undocumented member.
listInputDeviceTransfers_transferType :: Lens.Lens' ListInputDeviceTransfers Prelude.Text
listInputDeviceTransfers_transferType = Lens.lens (\ListInputDeviceTransfers' {transferType} -> transferType) (\s@ListInputDeviceTransfers' {} a -> s {transferType = a} :: ListInputDeviceTransfers)

instance Core.AWSPager ListInputDeviceTransfers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInputDeviceTransfersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInputDeviceTransfersResponse_inputDeviceTransfers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInputDeviceTransfers_nextToken
          Lens..~ rs
          Lens.^? listInputDeviceTransfersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListInputDeviceTransfers where
  type
    AWSResponse ListInputDeviceTransfers =
      ListInputDeviceTransfersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputDeviceTransfersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "inputDeviceTransfers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInputDeviceTransfers where
  hashWithSalt _salt ListInputDeviceTransfers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` transferType

instance Prelude.NFData ListInputDeviceTransfers where
  rnf ListInputDeviceTransfers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf transferType

instance Data.ToHeaders ListInputDeviceTransfers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListInputDeviceTransfers where
  toPath = Prelude.const "/prod/inputDeviceTransfers"

instance Data.ToQuery ListInputDeviceTransfers where
  toQuery ListInputDeviceTransfers' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults,
        "transferType" Data.=: transferType
      ]

-- | Placeholder documentation for ListInputDeviceTransfersResponse
--
-- /See:/ 'newListInputDeviceTransfersResponse' smart constructor.
data ListInputDeviceTransfersResponse = ListInputDeviceTransfersResponse'
  { -- | A token to get additional list results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of devices that you are transferring or are being transferred
    -- to you.
    inputDeviceTransfers :: Prelude.Maybe [TransferringInputDeviceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListInputDeviceTransfersResponse
newListInputDeviceTransfersResponse pHttpStatus_ =
  ListInputDeviceTransfersResponse'
    { nextToken =
        Prelude.Nothing,
      inputDeviceTransfers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to get additional list results.
listInputDeviceTransfersResponse_nextToken :: Lens.Lens' ListInputDeviceTransfersResponse (Prelude.Maybe Prelude.Text)
listInputDeviceTransfersResponse_nextToken = Lens.lens (\ListInputDeviceTransfersResponse' {nextToken} -> nextToken) (\s@ListInputDeviceTransfersResponse' {} a -> s {nextToken = a} :: ListInputDeviceTransfersResponse)

-- | The list of devices that you are transferring or are being transferred
-- to you.
listInputDeviceTransfersResponse_inputDeviceTransfers :: Lens.Lens' ListInputDeviceTransfersResponse (Prelude.Maybe [TransferringInputDeviceSummary])
listInputDeviceTransfersResponse_inputDeviceTransfers = Lens.lens (\ListInputDeviceTransfersResponse' {inputDeviceTransfers} -> inputDeviceTransfers) (\s@ListInputDeviceTransfersResponse' {} a -> s {inputDeviceTransfers = a} :: ListInputDeviceTransfersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInputDeviceTransfersResponse_httpStatus :: Lens.Lens' ListInputDeviceTransfersResponse Prelude.Int
listInputDeviceTransfersResponse_httpStatus = Lens.lens (\ListInputDeviceTransfersResponse' {httpStatus} -> httpStatus) (\s@ListInputDeviceTransfersResponse' {} a -> s {httpStatus = a} :: ListInputDeviceTransfersResponse)

instance
  Prelude.NFData
    ListInputDeviceTransfersResponse
  where
  rnf ListInputDeviceTransfersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf inputDeviceTransfers
      `Prelude.seq` Prelude.rnf httpStatus
