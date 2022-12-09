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
-- Module      : Amazonka.DataExchange.GetRevision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a revision.
module Amazonka.DataExchange.GetRevision
  ( -- * Creating a Request
    GetRevision (..),
    newGetRevision,

    -- * Request Lenses
    getRevision_dataSetId,
    getRevision_revisionId,

    -- * Destructuring the Response
    GetRevisionResponse (..),
    newGetRevisionResponse,

    -- * Response Lenses
    getRevisionResponse_arn,
    getRevisionResponse_comment,
    getRevisionResponse_createdAt,
    getRevisionResponse_dataSetId,
    getRevisionResponse_finalized,
    getRevisionResponse_id,
    getRevisionResponse_revocationComment,
    getRevisionResponse_revoked,
    getRevisionResponse_revokedAt,
    getRevisionResponse_sourceId,
    getRevisionResponse_tags,
    getRevisionResponse_updatedAt,
    getRevisionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRevision' smart constructor.
data GetRevision = GetRevision'
  { -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'getRevision_dataSetId' - The unique identifier for a data set.
--
-- 'revisionId', 'getRevision_revisionId' - The unique identifier for a revision.
newGetRevision ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  GetRevision
newGetRevision pDataSetId_ pRevisionId_ =
  GetRevision'
    { dataSetId = pDataSetId_,
      revisionId = pRevisionId_
    }

-- | The unique identifier for a data set.
getRevision_dataSetId :: Lens.Lens' GetRevision Prelude.Text
getRevision_dataSetId = Lens.lens (\GetRevision' {dataSetId} -> dataSetId) (\s@GetRevision' {} a -> s {dataSetId = a} :: GetRevision)

-- | The unique identifier for a revision.
getRevision_revisionId :: Lens.Lens' GetRevision Prelude.Text
getRevision_revisionId = Lens.lens (\GetRevision' {revisionId} -> revisionId) (\s@GetRevision' {} a -> s {revisionId = a} :: GetRevision)

instance Core.AWSRequest GetRevision where
  type AWSResponse GetRevision = GetRevisionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRevisionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Comment")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "DataSetId")
            Prelude.<*> (x Data..?> "Finalized")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "RevocationComment")
            Prelude.<*> (x Data..?> "Revoked")
            Prelude.<*> (x Data..?> "RevokedAt")
            Prelude.<*> (x Data..?> "SourceId")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRevision where
  hashWithSalt _salt GetRevision' {..} =
    _salt `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData GetRevision where
  rnf GetRevision' {..} =
    Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId

instance Data.ToHeaders GetRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRevision where
  toPath GetRevision' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Data.toBS dataSetId,
        "/revisions/",
        Data.toBS revisionId
      ]

instance Data.ToQuery GetRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRevisionResponse' smart constructor.
data GetRevisionResponse = GetRevisionResponse'
  { -- | The ARN for the revision.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the data set associated with the data set
    -- revision.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | To publish a revision to a data set in a product, the revision must
    -- first be finalized. Finalizing a revision tells AWS Data Exchange that
    -- your changes to the assets in the revision are complete. After it\'s in
    -- this read-only state, you can publish the revision to your products.
    -- Finalized revisions can be published through the AWS Data Exchange
    -- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
    -- Marketplace Catalog API action. When using the API, revisions are
    -- uniquely identified by their ARN.
    finalized :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the revision.
    id :: Prelude.Maybe Prelude.Text,
    -- | A required comment to inform subscribers of the reason their access to
    -- the revision was revoked.
    revocationComment :: Prelude.Maybe Prelude.Text,
    -- | A status indicating that subscribers\' access to the revision was
    -- revoked.
    revoked :: Prelude.Maybe Prelude.Bool,
    -- | The date and time that the revision was revoked, in ISO 8601 format.
    revokedAt :: Prelude.Maybe Data.POSIX,
    -- | The revision ID of the owned revision corresponding to the entitled
    -- revision being viewed. This parameter is returned when a revision owner
    -- is viewing the entitled copy of its owned revision.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the revision.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time that the revision was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRevisionResponse_arn' - The ARN for the revision.
--
-- 'comment', 'getRevisionResponse_comment' - An optional comment about the revision.
--
-- 'createdAt', 'getRevisionResponse_createdAt' - The date and time that the revision was created, in ISO 8601 format.
--
-- 'dataSetId', 'getRevisionResponse_dataSetId' - The unique identifier for the data set associated with the data set
-- revision.
--
-- 'finalized', 'getRevisionResponse_finalized' - To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
--
-- 'id', 'getRevisionResponse_id' - The unique identifier for the revision.
--
-- 'revocationComment', 'getRevisionResponse_revocationComment' - A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
--
-- 'revoked', 'getRevisionResponse_revoked' - A status indicating that subscribers\' access to the revision was
-- revoked.
--
-- 'revokedAt', 'getRevisionResponse_revokedAt' - The date and time that the revision was revoked, in ISO 8601 format.
--
-- 'sourceId', 'getRevisionResponse_sourceId' - The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
--
-- 'tags', 'getRevisionResponse_tags' - The tags for the revision.
--
-- 'updatedAt', 'getRevisionResponse_updatedAt' - The date and time that the revision was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'getRevisionResponse_httpStatus' - The response's http status code.
newGetRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRevisionResponse
newGetRevisionResponse pHttpStatus_ =
  GetRevisionResponse'
    { arn = Prelude.Nothing,
      comment = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      finalized = Prelude.Nothing,
      id = Prelude.Nothing,
      revocationComment = Prelude.Nothing,
      revoked = Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the revision.
getRevisionResponse_arn :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_arn = Lens.lens (\GetRevisionResponse' {arn} -> arn) (\s@GetRevisionResponse' {} a -> s {arn = a} :: GetRevisionResponse)

-- | An optional comment about the revision.
getRevisionResponse_comment :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_comment = Lens.lens (\GetRevisionResponse' {comment} -> comment) (\s@GetRevisionResponse' {} a -> s {comment = a} :: GetRevisionResponse)

-- | The date and time that the revision was created, in ISO 8601 format.
getRevisionResponse_createdAt :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.UTCTime)
getRevisionResponse_createdAt = Lens.lens (\GetRevisionResponse' {createdAt} -> createdAt) (\s@GetRevisionResponse' {} a -> s {createdAt = a} :: GetRevisionResponse) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the data set associated with the data set
-- revision.
getRevisionResponse_dataSetId :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_dataSetId = Lens.lens (\GetRevisionResponse' {dataSetId} -> dataSetId) (\s@GetRevisionResponse' {} a -> s {dataSetId = a} :: GetRevisionResponse)

-- | To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
getRevisionResponse_finalized :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Bool)
getRevisionResponse_finalized = Lens.lens (\GetRevisionResponse' {finalized} -> finalized) (\s@GetRevisionResponse' {} a -> s {finalized = a} :: GetRevisionResponse)

-- | The unique identifier for the revision.
getRevisionResponse_id :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_id = Lens.lens (\GetRevisionResponse' {id} -> id) (\s@GetRevisionResponse' {} a -> s {id = a} :: GetRevisionResponse)

-- | A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
getRevisionResponse_revocationComment :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_revocationComment = Lens.lens (\GetRevisionResponse' {revocationComment} -> revocationComment) (\s@GetRevisionResponse' {} a -> s {revocationComment = a} :: GetRevisionResponse)

-- | A status indicating that subscribers\' access to the revision was
-- revoked.
getRevisionResponse_revoked :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Bool)
getRevisionResponse_revoked = Lens.lens (\GetRevisionResponse' {revoked} -> revoked) (\s@GetRevisionResponse' {} a -> s {revoked = a} :: GetRevisionResponse)

-- | The date and time that the revision was revoked, in ISO 8601 format.
getRevisionResponse_revokedAt :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.UTCTime)
getRevisionResponse_revokedAt = Lens.lens (\GetRevisionResponse' {revokedAt} -> revokedAt) (\s@GetRevisionResponse' {} a -> s {revokedAt = a} :: GetRevisionResponse) Prelude.. Lens.mapping Data._Time

-- | The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
getRevisionResponse_sourceId :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_sourceId = Lens.lens (\GetRevisionResponse' {sourceId} -> sourceId) (\s@GetRevisionResponse' {} a -> s {sourceId = a} :: GetRevisionResponse)

-- | The tags for the revision.
getRevisionResponse_tags :: Lens.Lens' GetRevisionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRevisionResponse_tags = Lens.lens (\GetRevisionResponse' {tags} -> tags) (\s@GetRevisionResponse' {} a -> s {tags = a} :: GetRevisionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the revision was last updated, in ISO 8601
-- format.
getRevisionResponse_updatedAt :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.UTCTime)
getRevisionResponse_updatedAt = Lens.lens (\GetRevisionResponse' {updatedAt} -> updatedAt) (\s@GetRevisionResponse' {} a -> s {updatedAt = a} :: GetRevisionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getRevisionResponse_httpStatus :: Lens.Lens' GetRevisionResponse Prelude.Int
getRevisionResponse_httpStatus = Lens.lens (\GetRevisionResponse' {httpStatus} -> httpStatus) (\s@GetRevisionResponse' {} a -> s {httpStatus = a} :: GetRevisionResponse)

instance Prelude.NFData GetRevisionResponse where
  rnf GetRevisionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf finalized
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf revocationComment
      `Prelude.seq` Prelude.rnf revoked
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
