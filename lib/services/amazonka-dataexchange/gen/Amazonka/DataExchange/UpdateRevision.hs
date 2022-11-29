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
-- Module      : Amazonka.DataExchange.UpdateRevision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates a revision.
module Amazonka.DataExchange.UpdateRevision
  ( -- * Creating a Request
    UpdateRevision (..),
    newUpdateRevision,

    -- * Request Lenses
    updateRevision_comment,
    updateRevision_finalized,
    updateRevision_dataSetId,
    updateRevision_revisionId,

    -- * Destructuring the Response
    UpdateRevisionResponse (..),
    newUpdateRevisionResponse,

    -- * Response Lenses
    updateRevisionResponse_sourceId,
    updateRevisionResponse_revocationComment,
    updateRevisionResponse_arn,
    updateRevisionResponse_id,
    updateRevisionResponse_comment,
    updateRevisionResponse_finalized,
    updateRevisionResponse_dataSetId,
    updateRevisionResponse_revokedAt,
    updateRevisionResponse_revoked,
    updateRevisionResponse_createdAt,
    updateRevisionResponse_updatedAt,
    updateRevisionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRevision' smart constructor.
data UpdateRevision = UpdateRevision'
  { -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | Finalizing a revision tells AWS Data Exchange that your changes to the
    -- assets in the revision are complete. After it\'s in this read-only
    -- state, you can publish the revision to your products.
    finalized :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'updateRevision_comment' - An optional comment about the revision.
--
-- 'finalized', 'updateRevision_finalized' - Finalizing a revision tells AWS Data Exchange that your changes to the
-- assets in the revision are complete. After it\'s in this read-only
-- state, you can publish the revision to your products.
--
-- 'dataSetId', 'updateRevision_dataSetId' - The unique identifier for a data set.
--
-- 'revisionId', 'updateRevision_revisionId' - The unique identifier for a revision.
newUpdateRevision ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  UpdateRevision
newUpdateRevision pDataSetId_ pRevisionId_ =
  UpdateRevision'
    { comment = Prelude.Nothing,
      finalized = Prelude.Nothing,
      dataSetId = pDataSetId_,
      revisionId = pRevisionId_
    }

-- | An optional comment about the revision.
updateRevision_comment :: Lens.Lens' UpdateRevision (Prelude.Maybe Prelude.Text)
updateRevision_comment = Lens.lens (\UpdateRevision' {comment} -> comment) (\s@UpdateRevision' {} a -> s {comment = a} :: UpdateRevision)

-- | Finalizing a revision tells AWS Data Exchange that your changes to the
-- assets in the revision are complete. After it\'s in this read-only
-- state, you can publish the revision to your products.
updateRevision_finalized :: Lens.Lens' UpdateRevision (Prelude.Maybe Prelude.Bool)
updateRevision_finalized = Lens.lens (\UpdateRevision' {finalized} -> finalized) (\s@UpdateRevision' {} a -> s {finalized = a} :: UpdateRevision)

-- | The unique identifier for a data set.
updateRevision_dataSetId :: Lens.Lens' UpdateRevision Prelude.Text
updateRevision_dataSetId = Lens.lens (\UpdateRevision' {dataSetId} -> dataSetId) (\s@UpdateRevision' {} a -> s {dataSetId = a} :: UpdateRevision)

-- | The unique identifier for a revision.
updateRevision_revisionId :: Lens.Lens' UpdateRevision Prelude.Text
updateRevision_revisionId = Lens.lens (\UpdateRevision' {revisionId} -> revisionId) (\s@UpdateRevision' {} a -> s {revisionId = a} :: UpdateRevision)

instance Core.AWSRequest UpdateRevision where
  type
    AWSResponse UpdateRevision =
      UpdateRevisionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRevisionResponse'
            Prelude.<$> (x Core..?> "SourceId")
            Prelude.<*> (x Core..?> "RevocationComment")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Comment")
            Prelude.<*> (x Core..?> "Finalized")
            Prelude.<*> (x Core..?> "DataSetId")
            Prelude.<*> (x Core..?> "RevokedAt")
            Prelude.<*> (x Core..?> "Revoked")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRevision where
  hashWithSalt _salt UpdateRevision' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` finalized
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData UpdateRevision where
  rnf UpdateRevision' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf finalized
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId

instance Core.ToHeaders UpdateRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRevision where
  toJSON UpdateRevision' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Comment" Core..=) Prelude.<$> comment,
            ("Finalized" Core..=) Prelude.<$> finalized
          ]
      )

instance Core.ToPath UpdateRevision where
  toPath UpdateRevision' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Core.toBS dataSetId,
        "/revisions/",
        Core.toBS revisionId
      ]

instance Core.ToQuery UpdateRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRevisionResponse' smart constructor.
data UpdateRevisionResponse = UpdateRevisionResponse'
  { -- | The revision ID of the owned revision corresponding to the entitled
    -- revision being viewed. This parameter is returned when a revision owner
    -- is viewing the entitled copy of its owned revision.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | A required comment to inform subscribers of the reason their access to
    -- the revision was revoked.
    revocationComment :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the revision.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the revision.
    id :: Prelude.Maybe Prelude.Text,
    -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | To publish a revision to a data set in a product, the revision must
    -- first be finalized. Finalizing a revision tells AWS Data Exchange that
    -- changes to the assets in the revision are complete. After it\'s in this
    -- read-only state, you can publish the revision to your products.
    -- Finalized revisions can be published through the AWS Data Exchange
    -- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
    -- Marketplace Catalog API action. When using the API, revisions are
    -- uniquely identified by their ARN.
    finalized :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the data set associated with this revision.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was revoked, in ISO 8601 format.
    revokedAt :: Prelude.Maybe Core.POSIX,
    -- | A status indicating that subscribers\' access to the revision was
    -- revoked.
    revoked :: Prelude.Maybe Prelude.Bool,
    -- | The date and time that the revision was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the revision was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceId', 'updateRevisionResponse_sourceId' - The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
--
-- 'revocationComment', 'updateRevisionResponse_revocationComment' - A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
--
-- 'arn', 'updateRevisionResponse_arn' - The ARN for the revision.
--
-- 'id', 'updateRevisionResponse_id' - The unique identifier for the revision.
--
-- 'comment', 'updateRevisionResponse_comment' - An optional comment about the revision.
--
-- 'finalized', 'updateRevisionResponse_finalized' - To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- changes to the assets in the revision are complete. After it\'s in this
-- read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
--
-- 'dataSetId', 'updateRevisionResponse_dataSetId' - The unique identifier for the data set associated with this revision.
--
-- 'revokedAt', 'updateRevisionResponse_revokedAt' - The date and time that the revision was revoked, in ISO 8601 format.
--
-- 'revoked', 'updateRevisionResponse_revoked' - A status indicating that subscribers\' access to the revision was
-- revoked.
--
-- 'createdAt', 'updateRevisionResponse_createdAt' - The date and time that the revision was created, in ISO 8601 format.
--
-- 'updatedAt', 'updateRevisionResponse_updatedAt' - The date and time that the revision was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'updateRevisionResponse_httpStatus' - The response's http status code.
newUpdateRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRevisionResponse
newUpdateRevisionResponse pHttpStatus_ =
  UpdateRevisionResponse'
    { sourceId = Prelude.Nothing,
      revocationComment = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      comment = Prelude.Nothing,
      finalized = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      revoked = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
updateRevisionResponse_sourceId :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_sourceId = Lens.lens (\UpdateRevisionResponse' {sourceId} -> sourceId) (\s@UpdateRevisionResponse' {} a -> s {sourceId = a} :: UpdateRevisionResponse)

-- | A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
updateRevisionResponse_revocationComment :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_revocationComment = Lens.lens (\UpdateRevisionResponse' {revocationComment} -> revocationComment) (\s@UpdateRevisionResponse' {} a -> s {revocationComment = a} :: UpdateRevisionResponse)

-- | The ARN for the revision.
updateRevisionResponse_arn :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_arn = Lens.lens (\UpdateRevisionResponse' {arn} -> arn) (\s@UpdateRevisionResponse' {} a -> s {arn = a} :: UpdateRevisionResponse)

-- | The unique identifier for the revision.
updateRevisionResponse_id :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_id = Lens.lens (\UpdateRevisionResponse' {id} -> id) (\s@UpdateRevisionResponse' {} a -> s {id = a} :: UpdateRevisionResponse)

-- | An optional comment about the revision.
updateRevisionResponse_comment :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_comment = Lens.lens (\UpdateRevisionResponse' {comment} -> comment) (\s@UpdateRevisionResponse' {} a -> s {comment = a} :: UpdateRevisionResponse)

-- | To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- changes to the assets in the revision are complete. After it\'s in this
-- read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
updateRevisionResponse_finalized :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Bool)
updateRevisionResponse_finalized = Lens.lens (\UpdateRevisionResponse' {finalized} -> finalized) (\s@UpdateRevisionResponse' {} a -> s {finalized = a} :: UpdateRevisionResponse)

-- | The unique identifier for the data set associated with this revision.
updateRevisionResponse_dataSetId :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_dataSetId = Lens.lens (\UpdateRevisionResponse' {dataSetId} -> dataSetId) (\s@UpdateRevisionResponse' {} a -> s {dataSetId = a} :: UpdateRevisionResponse)

-- | The date and time that the revision was revoked, in ISO 8601 format.
updateRevisionResponse_revokedAt :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
updateRevisionResponse_revokedAt = Lens.lens (\UpdateRevisionResponse' {revokedAt} -> revokedAt) (\s@UpdateRevisionResponse' {} a -> s {revokedAt = a} :: UpdateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | A status indicating that subscribers\' access to the revision was
-- revoked.
updateRevisionResponse_revoked :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Bool)
updateRevisionResponse_revoked = Lens.lens (\UpdateRevisionResponse' {revoked} -> revoked) (\s@UpdateRevisionResponse' {} a -> s {revoked = a} :: UpdateRevisionResponse)

-- | The date and time that the revision was created, in ISO 8601 format.
updateRevisionResponse_createdAt :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
updateRevisionResponse_createdAt = Lens.lens (\UpdateRevisionResponse' {createdAt} -> createdAt) (\s@UpdateRevisionResponse' {} a -> s {createdAt = a} :: UpdateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time that the revision was last updated, in ISO 8601
-- format.
updateRevisionResponse_updatedAt :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
updateRevisionResponse_updatedAt = Lens.lens (\UpdateRevisionResponse' {updatedAt} -> updatedAt) (\s@UpdateRevisionResponse' {} a -> s {updatedAt = a} :: UpdateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
updateRevisionResponse_httpStatus :: Lens.Lens' UpdateRevisionResponse Prelude.Int
updateRevisionResponse_httpStatus = Lens.lens (\UpdateRevisionResponse' {httpStatus} -> httpStatus) (\s@UpdateRevisionResponse' {} a -> s {httpStatus = a} :: UpdateRevisionResponse)

instance Prelude.NFData UpdateRevisionResponse where
  rnf UpdateRevisionResponse' {..} =
    Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf revocationComment
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf finalized
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf revoked
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
