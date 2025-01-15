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
-- Module      : Amazonka.DataExchange.RevokeRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation revokes subscribers\' access to a revision.
module Amazonka.DataExchange.RevokeRevision
  ( -- * Creating a Request
    RevokeRevision (..),
    newRevokeRevision,

    -- * Request Lenses
    revokeRevision_dataSetId,
    revokeRevision_revisionId,
    revokeRevision_revocationComment,

    -- * Destructuring the Response
    RevokeRevisionResponse (..),
    newRevokeRevisionResponse,

    -- * Response Lenses
    revokeRevisionResponse_arn,
    revokeRevisionResponse_comment,
    revokeRevisionResponse_createdAt,
    revokeRevisionResponse_dataSetId,
    revokeRevisionResponse_finalized,
    revokeRevisionResponse_id,
    revokeRevisionResponse_revocationComment,
    revokeRevisionResponse_revoked,
    revokeRevisionResponse_revokedAt,
    revokeRevisionResponse_sourceId,
    revokeRevisionResponse_updatedAt,
    revokeRevisionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRevokeRevision' smart constructor.
data RevokeRevision = RevokeRevision'
  { -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text,
    -- | A required comment to inform subscribers of the reason their access to
    -- the revision was revoked.
    revocationComment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'revokeRevision_dataSetId' - The unique identifier for a data set.
--
-- 'revisionId', 'revokeRevision_revisionId' - The unique identifier for a revision.
--
-- 'revocationComment', 'revokeRevision_revocationComment' - A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
newRevokeRevision ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'revocationComment'
  Prelude.Text ->
  RevokeRevision
newRevokeRevision
  pDataSetId_
  pRevisionId_
  pRevocationComment_ =
    RevokeRevision'
      { dataSetId = pDataSetId_,
        revisionId = pRevisionId_,
        revocationComment = pRevocationComment_
      }

-- | The unique identifier for a data set.
revokeRevision_dataSetId :: Lens.Lens' RevokeRevision Prelude.Text
revokeRevision_dataSetId = Lens.lens (\RevokeRevision' {dataSetId} -> dataSetId) (\s@RevokeRevision' {} a -> s {dataSetId = a} :: RevokeRevision)

-- | The unique identifier for a revision.
revokeRevision_revisionId :: Lens.Lens' RevokeRevision Prelude.Text
revokeRevision_revisionId = Lens.lens (\RevokeRevision' {revisionId} -> revisionId) (\s@RevokeRevision' {} a -> s {revisionId = a} :: RevokeRevision)

-- | A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
revokeRevision_revocationComment :: Lens.Lens' RevokeRevision Prelude.Text
revokeRevision_revocationComment = Lens.lens (\RevokeRevision' {revocationComment} -> revocationComment) (\s@RevokeRevision' {} a -> s {revocationComment = a} :: RevokeRevision)

instance Core.AWSRequest RevokeRevision where
  type
    AWSResponse RevokeRevision =
      RevokeRevisionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RevokeRevisionResponse'
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
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeRevision where
  hashWithSalt _salt RevokeRevision' {..} =
    _salt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` revocationComment

instance Prelude.NFData RevokeRevision where
  rnf RevokeRevision' {..} =
    Prelude.rnf dataSetId `Prelude.seq`
      Prelude.rnf revisionId `Prelude.seq`
        Prelude.rnf revocationComment

instance Data.ToHeaders RevokeRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RevokeRevision where
  toJSON RevokeRevision' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RevocationComment" Data..= revocationComment)
          ]
      )

instance Data.ToPath RevokeRevision where
  toPath RevokeRevision' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Data.toBS dataSetId,
        "/revisions/",
        Data.toBS revisionId,
        "/revoke"
      ]

instance Data.ToQuery RevokeRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokeRevisionResponse' smart constructor.
data RevokeRevisionResponse = RevokeRevisionResponse'
  { -- | The ARN for the revision.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The unique identifier for the data set associated with the data set
    -- revision.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | To publish a revision to a data set in a product, the revision must
    -- first be finalized. Finalizing a revision tells AWS Data Exchange that
    -- changes to the assets in the revision are complete. After it\'s in this
    -- read-only state, you can publish the revision to your products.
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
    revokedAt :: Prelude.Maybe Data.ISO8601,
    -- | The revision ID of the owned revision corresponding to the entitled
    -- revision being viewed. This parameter is returned when a revision owner
    -- is viewing the entitled copy of its owned revision.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'revokeRevisionResponse_arn' - The ARN for the revision.
--
-- 'comment', 'revokeRevisionResponse_comment' - An optional comment about the revision.
--
-- 'createdAt', 'revokeRevisionResponse_createdAt' - The date and time that the revision was created, in ISO 8601 format.
--
-- 'dataSetId', 'revokeRevisionResponse_dataSetId' - The unique identifier for the data set associated with the data set
-- revision.
--
-- 'finalized', 'revokeRevisionResponse_finalized' - To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- changes to the assets in the revision are complete. After it\'s in this
-- read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
--
-- 'id', 'revokeRevisionResponse_id' - The unique identifier for the revision.
--
-- 'revocationComment', 'revokeRevisionResponse_revocationComment' - A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
--
-- 'revoked', 'revokeRevisionResponse_revoked' - A status indicating that subscribers\' access to the revision was
-- revoked.
--
-- 'revokedAt', 'revokeRevisionResponse_revokedAt' - The date and time that the revision was revoked, in ISO 8601 format.
--
-- 'sourceId', 'revokeRevisionResponse_sourceId' - The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
--
-- 'updatedAt', 'revokeRevisionResponse_updatedAt' - The date and time that the revision was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'revokeRevisionResponse_httpStatus' - The response's http status code.
newRevokeRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeRevisionResponse
newRevokeRevisionResponse pHttpStatus_ =
  RevokeRevisionResponse'
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
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the revision.
revokeRevisionResponse_arn :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Text)
revokeRevisionResponse_arn = Lens.lens (\RevokeRevisionResponse' {arn} -> arn) (\s@RevokeRevisionResponse' {} a -> s {arn = a} :: RevokeRevisionResponse)

-- | An optional comment about the revision.
revokeRevisionResponse_comment :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Text)
revokeRevisionResponse_comment = Lens.lens (\RevokeRevisionResponse' {comment} -> comment) (\s@RevokeRevisionResponse' {} a -> s {comment = a} :: RevokeRevisionResponse)

-- | The date and time that the revision was created, in ISO 8601 format.
revokeRevisionResponse_createdAt :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.UTCTime)
revokeRevisionResponse_createdAt = Lens.lens (\RevokeRevisionResponse' {createdAt} -> createdAt) (\s@RevokeRevisionResponse' {} a -> s {createdAt = a} :: RevokeRevisionResponse) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the data set associated with the data set
-- revision.
revokeRevisionResponse_dataSetId :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Text)
revokeRevisionResponse_dataSetId = Lens.lens (\RevokeRevisionResponse' {dataSetId} -> dataSetId) (\s@RevokeRevisionResponse' {} a -> s {dataSetId = a} :: RevokeRevisionResponse)

-- | To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- changes to the assets in the revision are complete. After it\'s in this
-- read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
revokeRevisionResponse_finalized :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Bool)
revokeRevisionResponse_finalized = Lens.lens (\RevokeRevisionResponse' {finalized} -> finalized) (\s@RevokeRevisionResponse' {} a -> s {finalized = a} :: RevokeRevisionResponse)

-- | The unique identifier for the revision.
revokeRevisionResponse_id :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Text)
revokeRevisionResponse_id = Lens.lens (\RevokeRevisionResponse' {id} -> id) (\s@RevokeRevisionResponse' {} a -> s {id = a} :: RevokeRevisionResponse)

-- | A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
revokeRevisionResponse_revocationComment :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Text)
revokeRevisionResponse_revocationComment = Lens.lens (\RevokeRevisionResponse' {revocationComment} -> revocationComment) (\s@RevokeRevisionResponse' {} a -> s {revocationComment = a} :: RevokeRevisionResponse)

-- | A status indicating that subscribers\' access to the revision was
-- revoked.
revokeRevisionResponse_revoked :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Bool)
revokeRevisionResponse_revoked = Lens.lens (\RevokeRevisionResponse' {revoked} -> revoked) (\s@RevokeRevisionResponse' {} a -> s {revoked = a} :: RevokeRevisionResponse)

-- | The date and time that the revision was revoked, in ISO 8601 format.
revokeRevisionResponse_revokedAt :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.UTCTime)
revokeRevisionResponse_revokedAt = Lens.lens (\RevokeRevisionResponse' {revokedAt} -> revokedAt) (\s@RevokeRevisionResponse' {} a -> s {revokedAt = a} :: RevokeRevisionResponse) Prelude.. Lens.mapping Data._Time

-- | The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
revokeRevisionResponse_sourceId :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.Text)
revokeRevisionResponse_sourceId = Lens.lens (\RevokeRevisionResponse' {sourceId} -> sourceId) (\s@RevokeRevisionResponse' {} a -> s {sourceId = a} :: RevokeRevisionResponse)

-- | The date and time that the revision was last updated, in ISO 8601
-- format.
revokeRevisionResponse_updatedAt :: Lens.Lens' RevokeRevisionResponse (Prelude.Maybe Prelude.UTCTime)
revokeRevisionResponse_updatedAt = Lens.lens (\RevokeRevisionResponse' {updatedAt} -> updatedAt) (\s@RevokeRevisionResponse' {} a -> s {updatedAt = a} :: RevokeRevisionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
revokeRevisionResponse_httpStatus :: Lens.Lens' RevokeRevisionResponse Prelude.Int
revokeRevisionResponse_httpStatus = Lens.lens (\RevokeRevisionResponse' {httpStatus} -> httpStatus) (\s@RevokeRevisionResponse' {} a -> s {httpStatus = a} :: RevokeRevisionResponse)

instance Prelude.NFData RevokeRevisionResponse where
  rnf RevokeRevisionResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf comment `Prelude.seq`
        Prelude.rnf createdAt `Prelude.seq`
          Prelude.rnf dataSetId `Prelude.seq`
            Prelude.rnf finalized `Prelude.seq`
              Prelude.rnf id `Prelude.seq`
                Prelude.rnf revocationComment `Prelude.seq`
                  Prelude.rnf revoked `Prelude.seq`
                    Prelude.rnf revokedAt `Prelude.seq`
                      Prelude.rnf sourceId `Prelude.seq`
                        Prelude.rnf updatedAt `Prelude.seq`
                          Prelude.rnf httpStatus
