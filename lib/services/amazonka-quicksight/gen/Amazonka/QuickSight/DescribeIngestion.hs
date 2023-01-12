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
-- Module      : Amazonka.QuickSight.DescribeIngestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a SPICE ingestion.
module Amazonka.QuickSight.DescribeIngestion
  ( -- * Creating a Request
    DescribeIngestion (..),
    newDescribeIngestion,

    -- * Request Lenses
    describeIngestion_awsAccountId,
    describeIngestion_dataSetId,
    describeIngestion_ingestionId,

    -- * Destructuring the Response
    DescribeIngestionResponse (..),
    newDescribeIngestionResponse,

    -- * Response Lenses
    describeIngestionResponse_ingestion,
    describeIngestionResponse_requestId,
    describeIngestionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIngestion' smart constructor.
data DescribeIngestion = DescribeIngestion'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the dataset used in the ingestion.
    dataSetId :: Prelude.Text,
    -- | An ID for the ingestion.
    ingestionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIngestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeIngestion_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'describeIngestion_dataSetId' - The ID of the dataset used in the ingestion.
--
-- 'ingestionId', 'describeIngestion_ingestionId' - An ID for the ingestion.
newDescribeIngestion ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'ingestionId'
  Prelude.Text ->
  DescribeIngestion
newDescribeIngestion
  pAwsAccountId_
  pDataSetId_
  pIngestionId_ =
    DescribeIngestion'
      { awsAccountId = pAwsAccountId_,
        dataSetId = pDataSetId_,
        ingestionId = pIngestionId_
      }

-- | The Amazon Web Services account ID.
describeIngestion_awsAccountId :: Lens.Lens' DescribeIngestion Prelude.Text
describeIngestion_awsAccountId = Lens.lens (\DescribeIngestion' {awsAccountId} -> awsAccountId) (\s@DescribeIngestion' {} a -> s {awsAccountId = a} :: DescribeIngestion)

-- | The ID of the dataset used in the ingestion.
describeIngestion_dataSetId :: Lens.Lens' DescribeIngestion Prelude.Text
describeIngestion_dataSetId = Lens.lens (\DescribeIngestion' {dataSetId} -> dataSetId) (\s@DescribeIngestion' {} a -> s {dataSetId = a} :: DescribeIngestion)

-- | An ID for the ingestion.
describeIngestion_ingestionId :: Lens.Lens' DescribeIngestion Prelude.Text
describeIngestion_ingestionId = Lens.lens (\DescribeIngestion' {ingestionId} -> ingestionId) (\s@DescribeIngestion' {} a -> s {ingestionId = a} :: DescribeIngestion)

instance Core.AWSRequest DescribeIngestion where
  type
    AWSResponse DescribeIngestion =
      DescribeIngestionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIngestionResponse'
            Prelude.<$> (x Data..?> "Ingestion")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIngestion where
  hashWithSalt _salt DescribeIngestion' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` ingestionId

instance Prelude.NFData DescribeIngestion where
  rnf DescribeIngestion' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf ingestionId

instance Data.ToHeaders DescribeIngestion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeIngestion where
  toPath DescribeIngestion' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/ingestions/",
        Data.toBS ingestionId
      ]

instance Data.ToQuery DescribeIngestion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIngestionResponse' smart constructor.
data DescribeIngestionResponse = DescribeIngestionResponse'
  { -- | Information about the ingestion.
    ingestion :: Prelude.Maybe Ingestion,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIngestionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestion', 'describeIngestionResponse_ingestion' - Information about the ingestion.
--
-- 'requestId', 'describeIngestionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeIngestionResponse_status' - The HTTP status of the request.
newDescribeIngestionResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeIngestionResponse
newDescribeIngestionResponse pStatus_ =
  DescribeIngestionResponse'
    { ingestion =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Information about the ingestion.
describeIngestionResponse_ingestion :: Lens.Lens' DescribeIngestionResponse (Prelude.Maybe Ingestion)
describeIngestionResponse_ingestion = Lens.lens (\DescribeIngestionResponse' {ingestion} -> ingestion) (\s@DescribeIngestionResponse' {} a -> s {ingestion = a} :: DescribeIngestionResponse)

-- | The Amazon Web Services request ID for this operation.
describeIngestionResponse_requestId :: Lens.Lens' DescribeIngestionResponse (Prelude.Maybe Prelude.Text)
describeIngestionResponse_requestId = Lens.lens (\DescribeIngestionResponse' {requestId} -> requestId) (\s@DescribeIngestionResponse' {} a -> s {requestId = a} :: DescribeIngestionResponse)

-- | The HTTP status of the request.
describeIngestionResponse_status :: Lens.Lens' DescribeIngestionResponse Prelude.Int
describeIngestionResponse_status = Lens.lens (\DescribeIngestionResponse' {status} -> status) (\s@DescribeIngestionResponse' {} a -> s {status = a} :: DescribeIngestionResponse)

instance Prelude.NFData DescribeIngestionResponse where
  rnf DescribeIngestionResponse' {..} =
    Prelude.rnf ingestion
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
