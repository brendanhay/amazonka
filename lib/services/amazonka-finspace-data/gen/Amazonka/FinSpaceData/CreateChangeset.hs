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
-- Module      : Amazonka.FinSpaceData.CreateChangeset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Changeset in a FinSpace Dataset.
module Amazonka.FinSpaceData.CreateChangeset
  ( -- * Creating a Request
    CreateChangeset (..),
    newCreateChangeset,

    -- * Request Lenses
    createChangeset_clientToken,
    createChangeset_datasetId,
    createChangeset_changeType,
    createChangeset_sourceParams,
    createChangeset_formatParams,

    -- * Destructuring the Response
    CreateChangesetResponse (..),
    newCreateChangesetResponse,

    -- * Response Lenses
    createChangesetResponse_changesetId,
    createChangesetResponse_datasetId,
    createChangesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request for a CreateChangeset operation.
--
-- /See:/ 'newCreateChangeset' smart constructor.
data CreateChangeset = CreateChangeset'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace Dataset where the Changeset will
    -- be created.
    datasetId :: Prelude.Text,
    -- | The option to indicate how a Changeset will be applied to a Dataset.
    --
    -- -   @REPLACE@ – Changeset will be considered as a replacement to all
    --     prior loaded Changesets.
    --
    -- -   @APPEND@ – Changeset will be considered as an addition to the end of
    --     all prior loaded Changesets.
    --
    -- -   @MODIFY@ – Changeset is considered as a replacement to a specific
    --     prior ingested Changeset.
    changeType :: ChangeType,
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
    -- <https://docs.aws.amazon.com/finspace/latest/data-api/fs-using-the-finspace-api.html#access-s3-buckets Loading data from an Amazon S3 Bucket using the FinSpace API>
    -- section.
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
-- Create a value of 'CreateChangeset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createChangeset_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'datasetId', 'createChangeset_datasetId' - The unique identifier for the FinSpace Dataset where the Changeset will
-- be created.
--
-- 'changeType', 'createChangeset_changeType' - The option to indicate how a Changeset will be applied to a Dataset.
--
-- -   @REPLACE@ – Changeset will be considered as a replacement to all
--     prior loaded Changesets.
--
-- -   @APPEND@ – Changeset will be considered as an addition to the end of
--     all prior loaded Changesets.
--
-- -   @MODIFY@ – Changeset is considered as a replacement to a specific
--     prior ingested Changeset.
--
-- 'sourceParams', 'createChangeset_sourceParams' - Options that define the location of the data being ingested
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
-- <https://docs.aws.amazon.com/finspace/latest/data-api/fs-using-the-finspace-api.html#access-s3-buckets Loading data from an Amazon S3 Bucket using the FinSpace API>
-- section.
--
-- 'formatParams', 'createChangeset_formatParams' - Options that define the structure of the source file(s) including the
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
newCreateChangeset ::
  -- | 'datasetId'
  Prelude.Text ->
  -- | 'changeType'
  ChangeType ->
  CreateChangeset
newCreateChangeset pDatasetId_ pChangeType_ =
  CreateChangeset'
    { clientToken = Prelude.Nothing,
      datasetId = pDatasetId_,
      changeType = pChangeType_,
      sourceParams = Prelude.mempty,
      formatParams = Prelude.mempty
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
createChangeset_clientToken :: Lens.Lens' CreateChangeset (Prelude.Maybe Prelude.Text)
createChangeset_clientToken = Lens.lens (\CreateChangeset' {clientToken} -> clientToken) (\s@CreateChangeset' {} a -> s {clientToken = a} :: CreateChangeset)

-- | The unique identifier for the FinSpace Dataset where the Changeset will
-- be created.
createChangeset_datasetId :: Lens.Lens' CreateChangeset Prelude.Text
createChangeset_datasetId = Lens.lens (\CreateChangeset' {datasetId} -> datasetId) (\s@CreateChangeset' {} a -> s {datasetId = a} :: CreateChangeset)

-- | The option to indicate how a Changeset will be applied to a Dataset.
--
-- -   @REPLACE@ – Changeset will be considered as a replacement to all
--     prior loaded Changesets.
--
-- -   @APPEND@ – Changeset will be considered as an addition to the end of
--     all prior loaded Changesets.
--
-- -   @MODIFY@ – Changeset is considered as a replacement to a specific
--     prior ingested Changeset.
createChangeset_changeType :: Lens.Lens' CreateChangeset ChangeType
createChangeset_changeType = Lens.lens (\CreateChangeset' {changeType} -> changeType) (\s@CreateChangeset' {} a -> s {changeType = a} :: CreateChangeset)

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
-- <https://docs.aws.amazon.com/finspace/latest/data-api/fs-using-the-finspace-api.html#access-s3-buckets Loading data from an Amazon S3 Bucket using the FinSpace API>
-- section.
createChangeset_sourceParams :: Lens.Lens' CreateChangeset (Prelude.HashMap Prelude.Text Prelude.Text)
createChangeset_sourceParams = Lens.lens (\CreateChangeset' {sourceParams} -> sourceParams) (\s@CreateChangeset' {} a -> s {sourceParams = a} :: CreateChangeset) Prelude.. Lens.coerced

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
createChangeset_formatParams :: Lens.Lens' CreateChangeset (Prelude.HashMap Prelude.Text Prelude.Text)
createChangeset_formatParams = Lens.lens (\CreateChangeset' {formatParams} -> formatParams) (\s@CreateChangeset' {} a -> s {formatParams = a} :: CreateChangeset) Prelude.. Lens.coerced

instance Core.AWSRequest CreateChangeset where
  type
    AWSResponse CreateChangeset =
      CreateChangesetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChangesetResponse'
            Prelude.<$> (x Core..?> "changesetId")
            Prelude.<*> (x Core..?> "datasetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChangeset where
  hashWithSalt _salt CreateChangeset' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` sourceParams
      `Prelude.hashWithSalt` formatParams

instance Prelude.NFData CreateChangeset where
  rnf CreateChangeset' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf sourceParams
      `Prelude.seq` Prelude.rnf formatParams

instance Core.ToHeaders CreateChangeset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateChangeset where
  toJSON CreateChangeset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("changeType" Core..= changeType),
            Prelude.Just ("sourceParams" Core..= sourceParams),
            Prelude.Just ("formatParams" Core..= formatParams)
          ]
      )

instance Core.ToPath CreateChangeset where
  toPath CreateChangeset' {..} =
    Prelude.mconcat
      ["/datasets/", Core.toBS datasetId, "/changesetsv2"]

instance Core.ToQuery CreateChangeset where
  toQuery = Prelude.const Prelude.mempty

-- | The response from a CreateChangeset operation.
--
-- /See:/ 'newCreateChangesetResponse' smart constructor.
data CreateChangesetResponse = CreateChangesetResponse'
  { -- | The unique identifier of the Changeset that is created.
    changesetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace Dataset where the Changeset is
    -- created.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChangesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changesetId', 'createChangesetResponse_changesetId' - The unique identifier of the Changeset that is created.
--
-- 'datasetId', 'createChangesetResponse_datasetId' - The unique identifier for the FinSpace Dataset where the Changeset is
-- created.
--
-- 'httpStatus', 'createChangesetResponse_httpStatus' - The response's http status code.
newCreateChangesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChangesetResponse
newCreateChangesetResponse pHttpStatus_ =
  CreateChangesetResponse'
    { changesetId =
        Prelude.Nothing,
      datasetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the Changeset that is created.
createChangesetResponse_changesetId :: Lens.Lens' CreateChangesetResponse (Prelude.Maybe Prelude.Text)
createChangesetResponse_changesetId = Lens.lens (\CreateChangesetResponse' {changesetId} -> changesetId) (\s@CreateChangesetResponse' {} a -> s {changesetId = a} :: CreateChangesetResponse)

-- | The unique identifier for the FinSpace Dataset where the Changeset is
-- created.
createChangesetResponse_datasetId :: Lens.Lens' CreateChangesetResponse (Prelude.Maybe Prelude.Text)
createChangesetResponse_datasetId = Lens.lens (\CreateChangesetResponse' {datasetId} -> datasetId) (\s@CreateChangesetResponse' {} a -> s {datasetId = a} :: CreateChangesetResponse)

-- | The response's http status code.
createChangesetResponse_httpStatus :: Lens.Lens' CreateChangesetResponse Prelude.Int
createChangesetResponse_httpStatus = Lens.lens (\CreateChangesetResponse' {httpStatus} -> httpStatus) (\s@CreateChangesetResponse' {} a -> s {httpStatus = a} :: CreateChangesetResponse)

instance Prelude.NFData CreateChangesetResponse where
  rnf CreateChangesetResponse' {..} =
    Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf httpStatus
