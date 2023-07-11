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
-- Module      : Amazonka.QuickSight.CancelIngestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an ongoing ingestion of data into SPICE.
module Amazonka.QuickSight.CancelIngestion
  ( -- * Creating a Request
    CancelIngestion (..),
    newCancelIngestion,

    -- * Request Lenses
    cancelIngestion_awsAccountId,
    cancelIngestion_dataSetId,
    cancelIngestion_ingestionId,

    -- * Destructuring the Response
    CancelIngestionResponse (..),
    newCancelIngestionResponse,

    -- * Response Lenses
    cancelIngestionResponse_arn,
    cancelIngestionResponse_ingestionId,
    cancelIngestionResponse_requestId,
    cancelIngestionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelIngestion' smart constructor.
data CancelIngestion = CancelIngestion'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the dataset used in the ingestion.
    dataSetId :: Prelude.Text,
    -- | An ID for the ingestion.
    ingestionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelIngestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'cancelIngestion_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'cancelIngestion_dataSetId' - The ID of the dataset used in the ingestion.
--
-- 'ingestionId', 'cancelIngestion_ingestionId' - An ID for the ingestion.
newCancelIngestion ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'ingestionId'
  Prelude.Text ->
  CancelIngestion
newCancelIngestion
  pAwsAccountId_
  pDataSetId_
  pIngestionId_ =
    CancelIngestion'
      { awsAccountId = pAwsAccountId_,
        dataSetId = pDataSetId_,
        ingestionId = pIngestionId_
      }

-- | The Amazon Web Services account ID.
cancelIngestion_awsAccountId :: Lens.Lens' CancelIngestion Prelude.Text
cancelIngestion_awsAccountId = Lens.lens (\CancelIngestion' {awsAccountId} -> awsAccountId) (\s@CancelIngestion' {} a -> s {awsAccountId = a} :: CancelIngestion)

-- | The ID of the dataset used in the ingestion.
cancelIngestion_dataSetId :: Lens.Lens' CancelIngestion Prelude.Text
cancelIngestion_dataSetId = Lens.lens (\CancelIngestion' {dataSetId} -> dataSetId) (\s@CancelIngestion' {} a -> s {dataSetId = a} :: CancelIngestion)

-- | An ID for the ingestion.
cancelIngestion_ingestionId :: Lens.Lens' CancelIngestion Prelude.Text
cancelIngestion_ingestionId = Lens.lens (\CancelIngestion' {ingestionId} -> ingestionId) (\s@CancelIngestion' {} a -> s {ingestionId = a} :: CancelIngestion)

instance Core.AWSRequest CancelIngestion where
  type
    AWSResponse CancelIngestion =
      CancelIngestionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelIngestionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "IngestionId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelIngestion where
  hashWithSalt _salt CancelIngestion' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` ingestionId

instance Prelude.NFData CancelIngestion where
  rnf CancelIngestion' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf ingestionId

instance Data.ToHeaders CancelIngestion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CancelIngestion where
  toPath CancelIngestion' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/ingestions/",
        Data.toBS ingestionId
      ]

instance Data.ToQuery CancelIngestion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelIngestionResponse' smart constructor.
data CancelIngestionResponse = CancelIngestionResponse'
  { -- | The Amazon Resource Name (ARN) for the data ingestion.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An ID for the ingestion.
    ingestionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelIngestionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'cancelIngestionResponse_arn' - The Amazon Resource Name (ARN) for the data ingestion.
--
-- 'ingestionId', 'cancelIngestionResponse_ingestionId' - An ID for the ingestion.
--
-- 'requestId', 'cancelIngestionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'cancelIngestionResponse_status' - The HTTP status of the request.
newCancelIngestionResponse ::
  -- | 'status'
  Prelude.Int ->
  CancelIngestionResponse
newCancelIngestionResponse pStatus_ =
  CancelIngestionResponse'
    { arn = Prelude.Nothing,
      ingestionId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the data ingestion.
cancelIngestionResponse_arn :: Lens.Lens' CancelIngestionResponse (Prelude.Maybe Prelude.Text)
cancelIngestionResponse_arn = Lens.lens (\CancelIngestionResponse' {arn} -> arn) (\s@CancelIngestionResponse' {} a -> s {arn = a} :: CancelIngestionResponse)

-- | An ID for the ingestion.
cancelIngestionResponse_ingestionId :: Lens.Lens' CancelIngestionResponse (Prelude.Maybe Prelude.Text)
cancelIngestionResponse_ingestionId = Lens.lens (\CancelIngestionResponse' {ingestionId} -> ingestionId) (\s@CancelIngestionResponse' {} a -> s {ingestionId = a} :: CancelIngestionResponse)

-- | The Amazon Web Services request ID for this operation.
cancelIngestionResponse_requestId :: Lens.Lens' CancelIngestionResponse (Prelude.Maybe Prelude.Text)
cancelIngestionResponse_requestId = Lens.lens (\CancelIngestionResponse' {requestId} -> requestId) (\s@CancelIngestionResponse' {} a -> s {requestId = a} :: CancelIngestionResponse)

-- | The HTTP status of the request.
cancelIngestionResponse_status :: Lens.Lens' CancelIngestionResponse Prelude.Int
cancelIngestionResponse_status = Lens.lens (\CancelIngestionResponse' {status} -> status) (\s@CancelIngestionResponse' {} a -> s {status = a} :: CancelIngestionResponse)

instance Prelude.NFData CancelIngestionResponse where
  rnf CancelIngestionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf ingestionId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
