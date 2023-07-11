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
-- Module      : Amazonka.Transfer.StartFileTransfer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins an outbound file transfer to a remote AS2 server. You specify the
-- @ConnectorId@ and the file paths for where to send the files.
module Amazonka.Transfer.StartFileTransfer
  ( -- * Creating a Request
    StartFileTransfer (..),
    newStartFileTransfer,

    -- * Request Lenses
    startFileTransfer_connectorId,
    startFileTransfer_sendFilePaths,

    -- * Destructuring the Response
    StartFileTransferResponse (..),
    newStartFileTransferResponse,

    -- * Response Lenses
    startFileTransferResponse_httpStatus,
    startFileTransferResponse_transferId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newStartFileTransfer' smart constructor.
data StartFileTransfer = StartFileTransfer'
  { -- | The unique identifier for the connector.
    connectorId :: Prelude.Text,
    -- | An array of strings. Each string represents the absolute path for one
    -- outbound file transfer. For example,
    -- @ @/@DOC-EXAMPLE-BUCKET@/@\/@/@myfile.txt@/@ @.
    sendFilePaths :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFileTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorId', 'startFileTransfer_connectorId' - The unique identifier for the connector.
--
-- 'sendFilePaths', 'startFileTransfer_sendFilePaths' - An array of strings. Each string represents the absolute path for one
-- outbound file transfer. For example,
-- @ @/@DOC-EXAMPLE-BUCKET@/@\/@/@myfile.txt@/@ @.
newStartFileTransfer ::
  -- | 'connectorId'
  Prelude.Text ->
  -- | 'sendFilePaths'
  Prelude.NonEmpty Prelude.Text ->
  StartFileTransfer
newStartFileTransfer pConnectorId_ pSendFilePaths_ =
  StartFileTransfer'
    { connectorId = pConnectorId_,
      sendFilePaths = Lens.coerced Lens.# pSendFilePaths_
    }

-- | The unique identifier for the connector.
startFileTransfer_connectorId :: Lens.Lens' StartFileTransfer Prelude.Text
startFileTransfer_connectorId = Lens.lens (\StartFileTransfer' {connectorId} -> connectorId) (\s@StartFileTransfer' {} a -> s {connectorId = a} :: StartFileTransfer)

-- | An array of strings. Each string represents the absolute path for one
-- outbound file transfer. For example,
-- @ @/@DOC-EXAMPLE-BUCKET@/@\/@/@myfile.txt@/@ @.
startFileTransfer_sendFilePaths :: Lens.Lens' StartFileTransfer (Prelude.NonEmpty Prelude.Text)
startFileTransfer_sendFilePaths = Lens.lens (\StartFileTransfer' {sendFilePaths} -> sendFilePaths) (\s@StartFileTransfer' {} a -> s {sendFilePaths = a} :: StartFileTransfer) Prelude.. Lens.coerced

instance Core.AWSRequest StartFileTransfer where
  type
    AWSResponse StartFileTransfer =
      StartFileTransferResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFileTransferResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TransferId")
      )

instance Prelude.Hashable StartFileTransfer where
  hashWithSalt _salt StartFileTransfer' {..} =
    _salt
      `Prelude.hashWithSalt` connectorId
      `Prelude.hashWithSalt` sendFilePaths

instance Prelude.NFData StartFileTransfer where
  rnf StartFileTransfer' {..} =
    Prelude.rnf connectorId
      `Prelude.seq` Prelude.rnf sendFilePaths

instance Data.ToHeaders StartFileTransfer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.StartFileTransfer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFileTransfer where
  toJSON StartFileTransfer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ConnectorId" Data..= connectorId),
            Prelude.Just
              ("SendFilePaths" Data..= sendFilePaths)
          ]
      )

instance Data.ToPath StartFileTransfer where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFileTransfer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFileTransferResponse' smart constructor.
data StartFileTransferResponse = StartFileTransferResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the unique identifier for this file transfer.
    transferId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFileTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startFileTransferResponse_httpStatus' - The response's http status code.
--
-- 'transferId', 'startFileTransferResponse_transferId' - Returns the unique identifier for this file transfer.
newStartFileTransferResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'transferId'
  Prelude.Text ->
  StartFileTransferResponse
newStartFileTransferResponse
  pHttpStatus_
  pTransferId_ =
    StartFileTransferResponse'
      { httpStatus =
          pHttpStatus_,
        transferId = pTransferId_
      }

-- | The response's http status code.
startFileTransferResponse_httpStatus :: Lens.Lens' StartFileTransferResponse Prelude.Int
startFileTransferResponse_httpStatus = Lens.lens (\StartFileTransferResponse' {httpStatus} -> httpStatus) (\s@StartFileTransferResponse' {} a -> s {httpStatus = a} :: StartFileTransferResponse)

-- | Returns the unique identifier for this file transfer.
startFileTransferResponse_transferId :: Lens.Lens' StartFileTransferResponse Prelude.Text
startFileTransferResponse_transferId = Lens.lens (\StartFileTransferResponse' {transferId} -> transferId) (\s@StartFileTransferResponse' {} a -> s {transferId = a} :: StartFileTransferResponse)

instance Prelude.NFData StartFileTransferResponse where
  rnf StartFileTransferResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf transferId
