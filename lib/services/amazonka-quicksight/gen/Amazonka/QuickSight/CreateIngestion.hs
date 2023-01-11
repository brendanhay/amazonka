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
-- Module      : Amazonka.QuickSight.CreateIngestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and starts a new SPICE ingestion for a dataset. You can manually
-- refresh datasets in an Enterprise edition account 32 times in a 24-hour
-- period. You can manually refresh datasets in a Standard edition account
-- 8 times in a 24-hour period. Each 24-hour period is measured starting 24
-- hours before the current date and time.
--
-- Any ingestions operating on tagged datasets inherit the same tags
-- automatically for use in access control. For an example, see
-- <http://aws.amazon.com/premiumsupport/knowledge-center/iam-ec2-resource-tags/ How do I create an IAM policy to control access to Amazon EC2 resources using tags?>
-- in the Amazon Web Services Knowledge Center. Tags are visible on the
-- tagged dataset, but not on the ingestion resource.
module Amazonka.QuickSight.CreateIngestion
  ( -- * Creating a Request
    CreateIngestion (..),
    newCreateIngestion,

    -- * Request Lenses
    createIngestion_ingestionType,
    createIngestion_dataSetId,
    createIngestion_ingestionId,
    createIngestion_awsAccountId,

    -- * Destructuring the Response
    CreateIngestionResponse (..),
    newCreateIngestionResponse,

    -- * Response Lenses
    createIngestionResponse_arn,
    createIngestionResponse_ingestionId,
    createIngestionResponse_ingestionStatus,
    createIngestionResponse_requestId,
    createIngestionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIngestion' smart constructor.
data CreateIngestion = CreateIngestion'
  { -- | The type of ingestion that you want to create.
    ingestionType :: Prelude.Maybe IngestionType,
    -- | The ID of the dataset used in the ingestion.
    dataSetId :: Prelude.Text,
    -- | An ID for the ingestion.
    ingestionId :: Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIngestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestionType', 'createIngestion_ingestionType' - The type of ingestion that you want to create.
--
-- 'dataSetId', 'createIngestion_dataSetId' - The ID of the dataset used in the ingestion.
--
-- 'ingestionId', 'createIngestion_ingestionId' - An ID for the ingestion.
--
-- 'awsAccountId', 'createIngestion_awsAccountId' - The Amazon Web Services account ID.
newCreateIngestion ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'ingestionId'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  CreateIngestion
newCreateIngestion
  pDataSetId_
  pIngestionId_
  pAwsAccountId_ =
    CreateIngestion'
      { ingestionType = Prelude.Nothing,
        dataSetId = pDataSetId_,
        ingestionId = pIngestionId_,
        awsAccountId = pAwsAccountId_
      }

-- | The type of ingestion that you want to create.
createIngestion_ingestionType :: Lens.Lens' CreateIngestion (Prelude.Maybe IngestionType)
createIngestion_ingestionType = Lens.lens (\CreateIngestion' {ingestionType} -> ingestionType) (\s@CreateIngestion' {} a -> s {ingestionType = a} :: CreateIngestion)

-- | The ID of the dataset used in the ingestion.
createIngestion_dataSetId :: Lens.Lens' CreateIngestion Prelude.Text
createIngestion_dataSetId = Lens.lens (\CreateIngestion' {dataSetId} -> dataSetId) (\s@CreateIngestion' {} a -> s {dataSetId = a} :: CreateIngestion)

-- | An ID for the ingestion.
createIngestion_ingestionId :: Lens.Lens' CreateIngestion Prelude.Text
createIngestion_ingestionId = Lens.lens (\CreateIngestion' {ingestionId} -> ingestionId) (\s@CreateIngestion' {} a -> s {ingestionId = a} :: CreateIngestion)

-- | The Amazon Web Services account ID.
createIngestion_awsAccountId :: Lens.Lens' CreateIngestion Prelude.Text
createIngestion_awsAccountId = Lens.lens (\CreateIngestion' {awsAccountId} -> awsAccountId) (\s@CreateIngestion' {} a -> s {awsAccountId = a} :: CreateIngestion)

instance Core.AWSRequest CreateIngestion where
  type
    AWSResponse CreateIngestion =
      CreateIngestionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIngestionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "IngestionId")
            Prelude.<*> (x Data..?> "IngestionStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIngestion where
  hashWithSalt _salt CreateIngestion' {..} =
    _salt `Prelude.hashWithSalt` ingestionType
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` ingestionId
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData CreateIngestion where
  rnf CreateIngestion' {..} =
    Prelude.rnf ingestionType
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf ingestionId
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders CreateIngestion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIngestion where
  toJSON CreateIngestion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IngestionType" Data..=)
              Prelude.<$> ingestionType
          ]
      )

instance Data.ToPath CreateIngestion where
  toPath CreateIngestion' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/ingestions/",
        Data.toBS ingestionId
      ]

instance Data.ToQuery CreateIngestion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIngestionResponse' smart constructor.
data CreateIngestionResponse = CreateIngestionResponse'
  { -- | The Amazon Resource Name (ARN) for the data ingestion.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An ID for the ingestion.
    ingestionId :: Prelude.Maybe Prelude.Text,
    -- | The ingestion status.
    ingestionStatus :: Prelude.Maybe IngestionStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIngestionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createIngestionResponse_arn' - The Amazon Resource Name (ARN) for the data ingestion.
--
-- 'ingestionId', 'createIngestionResponse_ingestionId' - An ID for the ingestion.
--
-- 'ingestionStatus', 'createIngestionResponse_ingestionStatus' - The ingestion status.
--
-- 'requestId', 'createIngestionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'createIngestionResponse_status' - The HTTP status of the request.
newCreateIngestionResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateIngestionResponse
newCreateIngestionResponse pStatus_ =
  CreateIngestionResponse'
    { arn = Prelude.Nothing,
      ingestionId = Prelude.Nothing,
      ingestionStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the data ingestion.
createIngestionResponse_arn :: Lens.Lens' CreateIngestionResponse (Prelude.Maybe Prelude.Text)
createIngestionResponse_arn = Lens.lens (\CreateIngestionResponse' {arn} -> arn) (\s@CreateIngestionResponse' {} a -> s {arn = a} :: CreateIngestionResponse)

-- | An ID for the ingestion.
createIngestionResponse_ingestionId :: Lens.Lens' CreateIngestionResponse (Prelude.Maybe Prelude.Text)
createIngestionResponse_ingestionId = Lens.lens (\CreateIngestionResponse' {ingestionId} -> ingestionId) (\s@CreateIngestionResponse' {} a -> s {ingestionId = a} :: CreateIngestionResponse)

-- | The ingestion status.
createIngestionResponse_ingestionStatus :: Lens.Lens' CreateIngestionResponse (Prelude.Maybe IngestionStatus)
createIngestionResponse_ingestionStatus = Lens.lens (\CreateIngestionResponse' {ingestionStatus} -> ingestionStatus) (\s@CreateIngestionResponse' {} a -> s {ingestionStatus = a} :: CreateIngestionResponse)

-- | The Amazon Web Services request ID for this operation.
createIngestionResponse_requestId :: Lens.Lens' CreateIngestionResponse (Prelude.Maybe Prelude.Text)
createIngestionResponse_requestId = Lens.lens (\CreateIngestionResponse' {requestId} -> requestId) (\s@CreateIngestionResponse' {} a -> s {requestId = a} :: CreateIngestionResponse)

-- | The HTTP status of the request.
createIngestionResponse_status :: Lens.Lens' CreateIngestionResponse Prelude.Int
createIngestionResponse_status = Lens.lens (\CreateIngestionResponse' {status} -> status) (\s@CreateIngestionResponse' {} a -> s {status = a} :: CreateIngestionResponse)

instance Prelude.NFData CreateIngestionResponse where
  rnf CreateIngestionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf ingestionId
      `Prelude.seq` Prelude.rnf ingestionStatus
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
