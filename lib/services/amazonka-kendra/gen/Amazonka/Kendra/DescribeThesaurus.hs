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
-- Module      : Amazonka.Kendra.DescribeThesaurus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an existing Amazon Kendra thesaurus.
module Amazonka.Kendra.DescribeThesaurus
  ( -- * Creating a Request
    DescribeThesaurus (..),
    newDescribeThesaurus,

    -- * Request Lenses
    describeThesaurus_id,
    describeThesaurus_indexId,

    -- * Destructuring the Response
    DescribeThesaurusResponse (..),
    newDescribeThesaurusResponse,

    -- * Response Lenses
    describeThesaurusResponse_createdAt,
    describeThesaurusResponse_description,
    describeThesaurusResponse_errorMessage,
    describeThesaurusResponse_fileSizeBytes,
    describeThesaurusResponse_id,
    describeThesaurusResponse_indexId,
    describeThesaurusResponse_name,
    describeThesaurusResponse_roleArn,
    describeThesaurusResponse_sourceS3Path,
    describeThesaurusResponse_status,
    describeThesaurusResponse_synonymRuleCount,
    describeThesaurusResponse_termCount,
    describeThesaurusResponse_updatedAt,
    describeThesaurusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeThesaurus' smart constructor.
data DescribeThesaurus = DescribeThesaurus'
  { -- | The identifier of the thesaurus you want to get information on.
    id :: Prelude.Text,
    -- | The identifier of the index for the thesaurus.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThesaurus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeThesaurus_id' - The identifier of the thesaurus you want to get information on.
--
-- 'indexId', 'describeThesaurus_indexId' - The identifier of the index for the thesaurus.
newDescribeThesaurus ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DescribeThesaurus
newDescribeThesaurus pId_ pIndexId_ =
  DescribeThesaurus' {id = pId_, indexId = pIndexId_}

-- | The identifier of the thesaurus you want to get information on.
describeThesaurus_id :: Lens.Lens' DescribeThesaurus Prelude.Text
describeThesaurus_id = Lens.lens (\DescribeThesaurus' {id} -> id) (\s@DescribeThesaurus' {} a -> s {id = a} :: DescribeThesaurus)

-- | The identifier of the index for the thesaurus.
describeThesaurus_indexId :: Lens.Lens' DescribeThesaurus Prelude.Text
describeThesaurus_indexId = Lens.lens (\DescribeThesaurus' {indexId} -> indexId) (\s@DescribeThesaurus' {} a -> s {indexId = a} :: DescribeThesaurus)

instance Core.AWSRequest DescribeThesaurus where
  type
    AWSResponse DescribeThesaurus =
      DescribeThesaurusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThesaurusResponse'
            Prelude.<$> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "FileSizeBytes")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "IndexId")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "SourceS3Path")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "SynonymRuleCount")
            Prelude.<*> (x Data..?> "TermCount")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeThesaurus where
  hashWithSalt _salt DescribeThesaurus' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData DescribeThesaurus where
  rnf DescribeThesaurus' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders DescribeThesaurus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DescribeThesaurus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeThesaurus where
  toJSON DescribeThesaurus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath DescribeThesaurus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeThesaurus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeThesaurusResponse' smart constructor.
data DescribeThesaurusResponse = DescribeThesaurusResponse'
  { -- | The Unix datetime that the thesaurus was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The thesaurus description.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
    -- provides more information.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The size of the thesaurus file in bytes.
    fileSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The identifier of the thesaurus.
    id :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index for the thesaurus.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | The thesaurus name.
    name :: Prelude.Maybe Prelude.Text,
    -- | An IAM role that gives Amazon Kendra permissions to access thesaurus
    -- file specified in @SourceS3Path@.
    roleArn :: Prelude.Maybe Prelude.Text,
    sourceS3Path :: Prelude.Maybe S3Path,
    -- | The current status of the thesaurus. When the value is @ACTIVE@, queries
    -- are able to use the thesaurus. If the @Status@ field value is @FAILED@,
    -- the @ErrorMessage@ field provides more information.
    --
    -- If the status is @ACTIVE_BUT_UPDATE_FAILED@, it means that Amazon Kendra
    -- could not ingest the new thesaurus file. The old thesaurus file is still
    -- active.
    status :: Prelude.Maybe ThesaurusStatus,
    -- | The number of synonym rules in the thesaurus file.
    synonymRuleCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of unique terms in the thesaurus file. For example, the
    -- synonyms @a,b,c@ and @a=>d@, the term count would be 4.
    termCount :: Prelude.Maybe Prelude.Integer,
    -- | The Unix datetime that the thesaurus was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThesaurusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'describeThesaurusResponse_createdAt' - The Unix datetime that the thesaurus was created.
--
-- 'description', 'describeThesaurusResponse_description' - The thesaurus description.
--
-- 'errorMessage', 'describeThesaurusResponse_errorMessage' - When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- provides more information.
--
-- 'fileSizeBytes', 'describeThesaurusResponse_fileSizeBytes' - The size of the thesaurus file in bytes.
--
-- 'id', 'describeThesaurusResponse_id' - The identifier of the thesaurus.
--
-- 'indexId', 'describeThesaurusResponse_indexId' - The identifier of the index for the thesaurus.
--
-- 'name', 'describeThesaurusResponse_name' - The thesaurus name.
--
-- 'roleArn', 'describeThesaurusResponse_roleArn' - An IAM role that gives Amazon Kendra permissions to access thesaurus
-- file specified in @SourceS3Path@.
--
-- 'sourceS3Path', 'describeThesaurusResponse_sourceS3Path' - Undocumented member.
--
-- 'status', 'describeThesaurusResponse_status' - The current status of the thesaurus. When the value is @ACTIVE@, queries
-- are able to use the thesaurus. If the @Status@ field value is @FAILED@,
-- the @ErrorMessage@ field provides more information.
--
-- If the status is @ACTIVE_BUT_UPDATE_FAILED@, it means that Amazon Kendra
-- could not ingest the new thesaurus file. The old thesaurus file is still
-- active.
--
-- 'synonymRuleCount', 'describeThesaurusResponse_synonymRuleCount' - The number of synonym rules in the thesaurus file.
--
-- 'termCount', 'describeThesaurusResponse_termCount' - The number of unique terms in the thesaurus file. For example, the
-- synonyms @a,b,c@ and @a=>d@, the term count would be 4.
--
-- 'updatedAt', 'describeThesaurusResponse_updatedAt' - The Unix datetime that the thesaurus was last updated.
--
-- 'httpStatus', 'describeThesaurusResponse_httpStatus' - The response's http status code.
newDescribeThesaurusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeThesaurusResponse
newDescribeThesaurusResponse pHttpStatus_ =
  DescribeThesaurusResponse'
    { createdAt =
        Prelude.Nothing,
      description = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      fileSizeBytes = Prelude.Nothing,
      id = Prelude.Nothing,
      indexId = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      sourceS3Path = Prelude.Nothing,
      status = Prelude.Nothing,
      synonymRuleCount = Prelude.Nothing,
      termCount = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Unix datetime that the thesaurus was created.
describeThesaurusResponse_createdAt :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.UTCTime)
describeThesaurusResponse_createdAt = Lens.lens (\DescribeThesaurusResponse' {createdAt} -> createdAt) (\s@DescribeThesaurusResponse' {} a -> s {createdAt = a} :: DescribeThesaurusResponse) Prelude.. Lens.mapping Data._Time

-- | The thesaurus description.
describeThesaurusResponse_description :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Text)
describeThesaurusResponse_description = Lens.lens (\DescribeThesaurusResponse' {description} -> description) (\s@DescribeThesaurusResponse' {} a -> s {description = a} :: DescribeThesaurusResponse)

-- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- provides more information.
describeThesaurusResponse_errorMessage :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Text)
describeThesaurusResponse_errorMessage = Lens.lens (\DescribeThesaurusResponse' {errorMessage} -> errorMessage) (\s@DescribeThesaurusResponse' {} a -> s {errorMessage = a} :: DescribeThesaurusResponse)

-- | The size of the thesaurus file in bytes.
describeThesaurusResponse_fileSizeBytes :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Integer)
describeThesaurusResponse_fileSizeBytes = Lens.lens (\DescribeThesaurusResponse' {fileSizeBytes} -> fileSizeBytes) (\s@DescribeThesaurusResponse' {} a -> s {fileSizeBytes = a} :: DescribeThesaurusResponse)

-- | The identifier of the thesaurus.
describeThesaurusResponse_id :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Text)
describeThesaurusResponse_id = Lens.lens (\DescribeThesaurusResponse' {id} -> id) (\s@DescribeThesaurusResponse' {} a -> s {id = a} :: DescribeThesaurusResponse)

-- | The identifier of the index for the thesaurus.
describeThesaurusResponse_indexId :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Text)
describeThesaurusResponse_indexId = Lens.lens (\DescribeThesaurusResponse' {indexId} -> indexId) (\s@DescribeThesaurusResponse' {} a -> s {indexId = a} :: DescribeThesaurusResponse)

-- | The thesaurus name.
describeThesaurusResponse_name :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Text)
describeThesaurusResponse_name = Lens.lens (\DescribeThesaurusResponse' {name} -> name) (\s@DescribeThesaurusResponse' {} a -> s {name = a} :: DescribeThesaurusResponse)

-- | An IAM role that gives Amazon Kendra permissions to access thesaurus
-- file specified in @SourceS3Path@.
describeThesaurusResponse_roleArn :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Text)
describeThesaurusResponse_roleArn = Lens.lens (\DescribeThesaurusResponse' {roleArn} -> roleArn) (\s@DescribeThesaurusResponse' {} a -> s {roleArn = a} :: DescribeThesaurusResponse)

-- | Undocumented member.
describeThesaurusResponse_sourceS3Path :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe S3Path)
describeThesaurusResponse_sourceS3Path = Lens.lens (\DescribeThesaurusResponse' {sourceS3Path} -> sourceS3Path) (\s@DescribeThesaurusResponse' {} a -> s {sourceS3Path = a} :: DescribeThesaurusResponse)

-- | The current status of the thesaurus. When the value is @ACTIVE@, queries
-- are able to use the thesaurus. If the @Status@ field value is @FAILED@,
-- the @ErrorMessage@ field provides more information.
--
-- If the status is @ACTIVE_BUT_UPDATE_FAILED@, it means that Amazon Kendra
-- could not ingest the new thesaurus file. The old thesaurus file is still
-- active.
describeThesaurusResponse_status :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe ThesaurusStatus)
describeThesaurusResponse_status = Lens.lens (\DescribeThesaurusResponse' {status} -> status) (\s@DescribeThesaurusResponse' {} a -> s {status = a} :: DescribeThesaurusResponse)

-- | The number of synonym rules in the thesaurus file.
describeThesaurusResponse_synonymRuleCount :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Integer)
describeThesaurusResponse_synonymRuleCount = Lens.lens (\DescribeThesaurusResponse' {synonymRuleCount} -> synonymRuleCount) (\s@DescribeThesaurusResponse' {} a -> s {synonymRuleCount = a} :: DescribeThesaurusResponse)

-- | The number of unique terms in the thesaurus file. For example, the
-- synonyms @a,b,c@ and @a=>d@, the term count would be 4.
describeThesaurusResponse_termCount :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.Integer)
describeThesaurusResponse_termCount = Lens.lens (\DescribeThesaurusResponse' {termCount} -> termCount) (\s@DescribeThesaurusResponse' {} a -> s {termCount = a} :: DescribeThesaurusResponse)

-- | The Unix datetime that the thesaurus was last updated.
describeThesaurusResponse_updatedAt :: Lens.Lens' DescribeThesaurusResponse (Prelude.Maybe Prelude.UTCTime)
describeThesaurusResponse_updatedAt = Lens.lens (\DescribeThesaurusResponse' {updatedAt} -> updatedAt) (\s@DescribeThesaurusResponse' {} a -> s {updatedAt = a} :: DescribeThesaurusResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeThesaurusResponse_httpStatus :: Lens.Lens' DescribeThesaurusResponse Prelude.Int
describeThesaurusResponse_httpStatus = Lens.lens (\DescribeThesaurusResponse' {httpStatus} -> httpStatus) (\s@DescribeThesaurusResponse' {} a -> s {httpStatus = a} :: DescribeThesaurusResponse)

instance Prelude.NFData DescribeThesaurusResponse where
  rnf DescribeThesaurusResponse' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf errorMessage `Prelude.seq`
          Prelude.rnf fileSizeBytes `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf indexId `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf roleArn `Prelude.seq`
                    Prelude.rnf sourceS3Path `Prelude.seq`
                      Prelude.rnf status `Prelude.seq`
                        Prelude.rnf synonymRuleCount `Prelude.seq`
                          Prelude.rnf termCount `Prelude.seq`
                            Prelude.rnf updatedAt `Prelude.seq`
                              Prelude.rnf httpStatus
