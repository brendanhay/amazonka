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
-- Module      : Amazonka.FinSpaceData.UpdateChangeset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a FinSpace Changeset.
module Amazonka.FinSpaceData.UpdateChangeset
  ( -- * Creating a Request
    UpdateChangeset (..),
    newUpdateChangeset,

    -- * Request Lenses
    updateChangeset_clientToken,
    updateChangeset_datasetId,
    updateChangeset_changesetId,
    updateChangeset_sourceParams,
    updateChangeset_formatParams,

    -- * Destructuring the Response
    UpdateChangesetResponse (..),
    newUpdateChangesetResponse,

    -- * Response Lenses
    updateChangesetResponse_changesetId,
    updateChangesetResponse_datasetId,
    updateChangesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to update an existing changeset.
--
-- /See:/ 'newUpdateChangeset' smart constructor.
data UpdateChangeset = UpdateChangeset'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace Dataset in which the Changeset is
    -- created.
    datasetId :: Prelude.Text,
    -- | The unique identifier for the Changeset to update.
    changesetId :: Prelude.Text,
    -- | Options that define the location of the data being ingested
    -- (@s3SourcePath@) and the source of the changeset (@sourceType@).
    --
    -- Both @s3SourcePath@ and @sourceType@ are required attributes.
    --
    -- Here is an example of how you could specify the @sourceParams@:
    --
    -- @ \"sourceParams\": { \"s3SourcePath\": \"s3:\/\/finspace-landing-us-east-2-bk7gcfvitndqa6ebnvys4d\/scratch\/wr5hh8pwkpqqkxa4sxrmcw\/ingestion\/equity.csv\", \"sourceType\": \"S3\" } @
    --
    -- The S3 path that you specify must allow the FinSpace role access. To do
    -- that, you first need to configure the IAM policy on S3 bucket. For more
    -- information, see
    -- <https://docs.aws.amazon.com/finspace/latest/data-api/fs-using-the-finspace-api.html#access-s3-buckets Loading data from an Amazon S3 Bucket using the FinSpace API>section.
    sourceParams :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Options that define the structure of the source file(s) including the
    -- format type (@formatType@), header row (@withHeader@), data separation
    -- character (@separator@) and the type of compression (@compression@).
    --
    -- @formatType@ is a required attribute and can have the following values:
    --
    -- -   @PARQUET@ – Parquet source file format.
    --
    -- -   @CSV@ – CSV source file format.
    --
    -- -   @JSON@ – JSON source file format.
    --
    -- -   @XML@ – XML source file format.
    --
    -- Here is an example of how you could specify the @formatParams@:
    --
    -- @ \"formatParams\": { \"formatType\": \"CSV\", \"withHeader\": \"true\", \"separator\": \",\", \"compression\":\"None\" } @
    --
    -- Note that if you only provide @formatType@ as @CSV@, the rest of the
    -- attributes will automatically default to CSV values as following:
    --
    -- @ { \"withHeader\": \"true\", \"separator\": \",\" } @
    --
    -- For more information about supported file formats, see
    -- <https://docs.aws.amazon.com/finspace/latest/userguide/supported-data-types.html Supported Data Types and File Formats>
    -- in the FinSpace User Guide.
    formatParams :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChangeset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateChangeset_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'datasetId', 'updateChangeset_datasetId' - The unique identifier for the FinSpace Dataset in which the Changeset is
-- created.
--
-- 'changesetId', 'updateChangeset_changesetId' - The unique identifier for the Changeset to update.
--
-- 'sourceParams', 'updateChangeset_sourceParams' - Options that define the location of the data being ingested
-- (@s3SourcePath@) and the source of the changeset (@sourceType@).
--
-- Both @s3SourcePath@ and @sourceType@ are required attributes.
--
-- Here is an example of how you could specify the @sourceParams@:
--
-- @ \"sourceParams\": { \"s3SourcePath\": \"s3:\/\/finspace-landing-us-east-2-bk7gcfvitndqa6ebnvys4d\/scratch\/wr5hh8pwkpqqkxa4sxrmcw\/ingestion\/equity.csv\", \"sourceType\": \"S3\" } @
--
-- The S3 path that you specify must allow the FinSpace role access. To do
-- that, you first need to configure the IAM policy on S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/finspace/latest/data-api/fs-using-the-finspace-api.html#access-s3-buckets Loading data from an Amazon S3 Bucket using the FinSpace API>section.
--
-- 'formatParams', 'updateChangeset_formatParams' - Options that define the structure of the source file(s) including the
-- format type (@formatType@), header row (@withHeader@), data separation
-- character (@separator@) and the type of compression (@compression@).
--
-- @formatType@ is a required attribute and can have the following values:
--
-- -   @PARQUET@ – Parquet source file format.
--
-- -   @CSV@ – CSV source file format.
--
-- -   @JSON@ – JSON source file format.
--
-- -   @XML@ – XML source file format.
--
-- Here is an example of how you could specify the @formatParams@:
--
-- @ \"formatParams\": { \"formatType\": \"CSV\", \"withHeader\": \"true\", \"separator\": \",\", \"compression\":\"None\" } @
--
-- Note that if you only provide @formatType@ as @CSV@, the rest of the
-- attributes will automatically default to CSV values as following:
--
-- @ { \"withHeader\": \"true\", \"separator\": \",\" } @
--
-- For more information about supported file formats, see
-- <https://docs.aws.amazon.com/finspace/latest/userguide/supported-data-types.html Supported Data Types and File Formats>
-- in the FinSpace User Guide.
newUpdateChangeset ::
  -- | 'datasetId'
  Prelude.Text ->
  -- | 'changesetId'
  Prelude.Text ->
  UpdateChangeset
newUpdateChangeset pDatasetId_ pChangesetId_ =
  UpdateChangeset'
    { clientToken = Prelude.Nothing,
      datasetId = pDatasetId_,
      changesetId = pChangesetId_,
      sourceParams = Prelude.mempty,
      formatParams = Prelude.mempty
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateChangeset_clientToken :: Lens.Lens' UpdateChangeset (Prelude.Maybe Prelude.Text)
updateChangeset_clientToken = Lens.lens (\UpdateChangeset' {clientToken} -> clientToken) (\s@UpdateChangeset' {} a -> s {clientToken = a} :: UpdateChangeset)

-- | The unique identifier for the FinSpace Dataset in which the Changeset is
-- created.
updateChangeset_datasetId :: Lens.Lens' UpdateChangeset Prelude.Text
updateChangeset_datasetId = Lens.lens (\UpdateChangeset' {datasetId} -> datasetId) (\s@UpdateChangeset' {} a -> s {datasetId = a} :: UpdateChangeset)

-- | The unique identifier for the Changeset to update.
updateChangeset_changesetId :: Lens.Lens' UpdateChangeset Prelude.Text
updateChangeset_changesetId = Lens.lens (\UpdateChangeset' {changesetId} -> changesetId) (\s@UpdateChangeset' {} a -> s {changesetId = a} :: UpdateChangeset)

-- | Options that define the location of the data being ingested
-- (@s3SourcePath@) and the source of the changeset (@sourceType@).
--
-- Both @s3SourcePath@ and @sourceType@ are required attributes.
--
-- Here is an example of how you could specify the @sourceParams@:
--
-- @ \"sourceParams\": { \"s3SourcePath\": \"s3:\/\/finspace-landing-us-east-2-bk7gcfvitndqa6ebnvys4d\/scratch\/wr5hh8pwkpqqkxa4sxrmcw\/ingestion\/equity.csv\", \"sourceType\": \"S3\" } @
--
-- The S3 path that you specify must allow the FinSpace role access. To do
-- that, you first need to configure the IAM policy on S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/finspace/latest/data-api/fs-using-the-finspace-api.html#access-s3-buckets Loading data from an Amazon S3 Bucket using the FinSpace API>section.
updateChangeset_sourceParams :: Lens.Lens' UpdateChangeset (Prelude.HashMap Prelude.Text Prelude.Text)
updateChangeset_sourceParams = Lens.lens (\UpdateChangeset' {sourceParams} -> sourceParams) (\s@UpdateChangeset' {} a -> s {sourceParams = a} :: UpdateChangeset) Prelude.. Lens.coerced

-- | Options that define the structure of the source file(s) including the
-- format type (@formatType@), header row (@withHeader@), data separation
-- character (@separator@) and the type of compression (@compression@).
--
-- @formatType@ is a required attribute and can have the following values:
--
-- -   @PARQUET@ – Parquet source file format.
--
-- -   @CSV@ – CSV source file format.
--
-- -   @JSON@ – JSON source file format.
--
-- -   @XML@ – XML source file format.
--
-- Here is an example of how you could specify the @formatParams@:
--
-- @ \"formatParams\": { \"formatType\": \"CSV\", \"withHeader\": \"true\", \"separator\": \",\", \"compression\":\"None\" } @
--
-- Note that if you only provide @formatType@ as @CSV@, the rest of the
-- attributes will automatically default to CSV values as following:
--
-- @ { \"withHeader\": \"true\", \"separator\": \",\" } @
--
-- For more information about supported file formats, see
-- <https://docs.aws.amazon.com/finspace/latest/userguide/supported-data-types.html Supported Data Types and File Formats>
-- in the FinSpace User Guide.
updateChangeset_formatParams :: Lens.Lens' UpdateChangeset (Prelude.HashMap Prelude.Text Prelude.Text)
updateChangeset_formatParams = Lens.lens (\UpdateChangeset' {formatParams} -> formatParams) (\s@UpdateChangeset' {} a -> s {formatParams = a} :: UpdateChangeset) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateChangeset where
  type
    AWSResponse UpdateChangeset =
      UpdateChangesetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChangesetResponse'
            Prelude.<$> (x Data..?> "changesetId")
            Prelude.<*> (x Data..?> "datasetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChangeset where
  hashWithSalt _salt UpdateChangeset' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` changesetId
      `Prelude.hashWithSalt` sourceParams
      `Prelude.hashWithSalt` formatParams

instance Prelude.NFData UpdateChangeset where
  rnf UpdateChangeset' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf sourceParams
      `Prelude.seq` Prelude.rnf formatParams

instance Data.ToHeaders UpdateChangeset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateChangeset where
  toJSON UpdateChangeset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("sourceParams" Data..= sourceParams),
            Prelude.Just ("formatParams" Data..= formatParams)
          ]
      )

instance Data.ToPath UpdateChangeset where
  toPath UpdateChangeset' {..} =
    Prelude.mconcat
      [ "/datasets/",
        Data.toBS datasetId,
        "/changesetsv2/",
        Data.toBS changesetId
      ]

instance Data.ToQuery UpdateChangeset where
  toQuery = Prelude.const Prelude.mempty

-- | The response from a update changeset operation.
--
-- /See:/ 'newUpdateChangesetResponse' smart constructor.
data UpdateChangesetResponse = UpdateChangesetResponse'
  { -- | The unique identifier for the Changeset to update.
    changesetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace Dataset in which the Changeset is
    -- created.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChangesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changesetId', 'updateChangesetResponse_changesetId' - The unique identifier for the Changeset to update.
--
-- 'datasetId', 'updateChangesetResponse_datasetId' - The unique identifier for the FinSpace Dataset in which the Changeset is
-- created.
--
-- 'httpStatus', 'updateChangesetResponse_httpStatus' - The response's http status code.
newUpdateChangesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChangesetResponse
newUpdateChangesetResponse pHttpStatus_ =
  UpdateChangesetResponse'
    { changesetId =
        Prelude.Nothing,
      datasetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the Changeset to update.
updateChangesetResponse_changesetId :: Lens.Lens' UpdateChangesetResponse (Prelude.Maybe Prelude.Text)
updateChangesetResponse_changesetId = Lens.lens (\UpdateChangesetResponse' {changesetId} -> changesetId) (\s@UpdateChangesetResponse' {} a -> s {changesetId = a} :: UpdateChangesetResponse)

-- | The unique identifier for the FinSpace Dataset in which the Changeset is
-- created.
updateChangesetResponse_datasetId :: Lens.Lens' UpdateChangesetResponse (Prelude.Maybe Prelude.Text)
updateChangesetResponse_datasetId = Lens.lens (\UpdateChangesetResponse' {datasetId} -> datasetId) (\s@UpdateChangesetResponse' {} a -> s {datasetId = a} :: UpdateChangesetResponse)

-- | The response's http status code.
updateChangesetResponse_httpStatus :: Lens.Lens' UpdateChangesetResponse Prelude.Int
updateChangesetResponse_httpStatus = Lens.lens (\UpdateChangesetResponse' {httpStatus} -> httpStatus) (\s@UpdateChangesetResponse' {} a -> s {httpStatus = a} :: UpdateChangesetResponse)

instance Prelude.NFData UpdateChangesetResponse where
  rnf UpdateChangesetResponse' {..} =
    Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf httpStatus
