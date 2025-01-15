{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.Types.RevisionEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.RevisionEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A revision is a container for one or more assets.
--
-- /See:/ 'newRevisionEntry' smart constructor.
data RevisionEntry = RevisionEntry'
  { -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | To publish a revision to a data set in a product, the revision must
    -- first be finalized. Finalizing a revision tells AWS Data Exchange that
    -- your changes to the assets in the revision are complete. After it\'s in
    -- this read-only state, you can publish the revision to your products.
    -- Finalized revisions can be published through the AWS Data Exchange
    -- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
    -- Marketplace Catalog API action. When using the API, revisions are
    -- uniquely identified by their ARN.
    finalized :: Prelude.Maybe Prelude.Bool,
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
    -- | The ARN for the revision.
    arn :: Prelude.Text,
    -- | The date and time that the revision was created, in ISO 8601 format.
    createdAt :: Data.ISO8601,
    -- | The unique identifier for the data set associated with the data set
    -- revision.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision.
    id :: Prelude.Text,
    -- | The date and time that the revision was last updated, in ISO 8601
    -- format.
    updatedAt :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevisionEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'revisionEntry_comment' - An optional comment about the revision.
--
-- 'finalized', 'revisionEntry_finalized' - To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
--
-- 'revocationComment', 'revisionEntry_revocationComment' - A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
--
-- 'revoked', 'revisionEntry_revoked' - A status indicating that subscribers\' access to the revision was
-- revoked.
--
-- 'revokedAt', 'revisionEntry_revokedAt' - The date and time that the revision was revoked, in ISO 8601 format.
--
-- 'sourceId', 'revisionEntry_sourceId' - The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
--
-- 'arn', 'revisionEntry_arn' - The ARN for the revision.
--
-- 'createdAt', 'revisionEntry_createdAt' - The date and time that the revision was created, in ISO 8601 format.
--
-- 'dataSetId', 'revisionEntry_dataSetId' - The unique identifier for the data set associated with the data set
-- revision.
--
-- 'id', 'revisionEntry_id' - The unique identifier for the revision.
--
-- 'updatedAt', 'revisionEntry_updatedAt' - The date and time that the revision was last updated, in ISO 8601
-- format.
newRevisionEntry ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  RevisionEntry
newRevisionEntry
  pArn_
  pCreatedAt_
  pDataSetId_
  pId_
  pUpdatedAt_ =
    RevisionEntry'
      { comment = Prelude.Nothing,
        finalized = Prelude.Nothing,
        revocationComment = Prelude.Nothing,
        revoked = Prelude.Nothing,
        revokedAt = Prelude.Nothing,
        sourceId = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        dataSetId = pDataSetId_,
        id = pId_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | An optional comment about the revision.
revisionEntry_comment :: Lens.Lens' RevisionEntry (Prelude.Maybe Prelude.Text)
revisionEntry_comment = Lens.lens (\RevisionEntry' {comment} -> comment) (\s@RevisionEntry' {} a -> s {comment = a} :: RevisionEntry)

-- | To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
revisionEntry_finalized :: Lens.Lens' RevisionEntry (Prelude.Maybe Prelude.Bool)
revisionEntry_finalized = Lens.lens (\RevisionEntry' {finalized} -> finalized) (\s@RevisionEntry' {} a -> s {finalized = a} :: RevisionEntry)

-- | A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
revisionEntry_revocationComment :: Lens.Lens' RevisionEntry (Prelude.Maybe Prelude.Text)
revisionEntry_revocationComment = Lens.lens (\RevisionEntry' {revocationComment} -> revocationComment) (\s@RevisionEntry' {} a -> s {revocationComment = a} :: RevisionEntry)

-- | A status indicating that subscribers\' access to the revision was
-- revoked.
revisionEntry_revoked :: Lens.Lens' RevisionEntry (Prelude.Maybe Prelude.Bool)
revisionEntry_revoked = Lens.lens (\RevisionEntry' {revoked} -> revoked) (\s@RevisionEntry' {} a -> s {revoked = a} :: RevisionEntry)

-- | The date and time that the revision was revoked, in ISO 8601 format.
revisionEntry_revokedAt :: Lens.Lens' RevisionEntry (Prelude.Maybe Prelude.UTCTime)
revisionEntry_revokedAt = Lens.lens (\RevisionEntry' {revokedAt} -> revokedAt) (\s@RevisionEntry' {} a -> s {revokedAt = a} :: RevisionEntry) Prelude.. Lens.mapping Data._Time

-- | The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
revisionEntry_sourceId :: Lens.Lens' RevisionEntry (Prelude.Maybe Prelude.Text)
revisionEntry_sourceId = Lens.lens (\RevisionEntry' {sourceId} -> sourceId) (\s@RevisionEntry' {} a -> s {sourceId = a} :: RevisionEntry)

-- | The ARN for the revision.
revisionEntry_arn :: Lens.Lens' RevisionEntry Prelude.Text
revisionEntry_arn = Lens.lens (\RevisionEntry' {arn} -> arn) (\s@RevisionEntry' {} a -> s {arn = a} :: RevisionEntry)

-- | The date and time that the revision was created, in ISO 8601 format.
revisionEntry_createdAt :: Lens.Lens' RevisionEntry Prelude.UTCTime
revisionEntry_createdAt = Lens.lens (\RevisionEntry' {createdAt} -> createdAt) (\s@RevisionEntry' {} a -> s {createdAt = a} :: RevisionEntry) Prelude.. Data._Time

-- | The unique identifier for the data set associated with the data set
-- revision.
revisionEntry_dataSetId :: Lens.Lens' RevisionEntry Prelude.Text
revisionEntry_dataSetId = Lens.lens (\RevisionEntry' {dataSetId} -> dataSetId) (\s@RevisionEntry' {} a -> s {dataSetId = a} :: RevisionEntry)

-- | The unique identifier for the revision.
revisionEntry_id :: Lens.Lens' RevisionEntry Prelude.Text
revisionEntry_id = Lens.lens (\RevisionEntry' {id} -> id) (\s@RevisionEntry' {} a -> s {id = a} :: RevisionEntry)

-- | The date and time that the revision was last updated, in ISO 8601
-- format.
revisionEntry_updatedAt :: Lens.Lens' RevisionEntry Prelude.UTCTime
revisionEntry_updatedAt = Lens.lens (\RevisionEntry' {updatedAt} -> updatedAt) (\s@RevisionEntry' {} a -> s {updatedAt = a} :: RevisionEntry) Prelude.. Data._Time

instance Data.FromJSON RevisionEntry where
  parseJSON =
    Data.withObject
      "RevisionEntry"
      ( \x ->
          RevisionEntry'
            Prelude.<$> (x Data..:? "Comment")
            Prelude.<*> (x Data..:? "Finalized")
            Prelude.<*> (x Data..:? "RevocationComment")
            Prelude.<*> (x Data..:? "Revoked")
            Prelude.<*> (x Data..:? "RevokedAt")
            Prelude.<*> (x Data..:? "SourceId")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "UpdatedAt")
      )

instance Prelude.Hashable RevisionEntry where
  hashWithSalt _salt RevisionEntry' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` finalized
      `Prelude.hashWithSalt` revocationComment
      `Prelude.hashWithSalt` revoked
      `Prelude.hashWithSalt` revokedAt
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData RevisionEntry where
  rnf RevisionEntry' {..} =
    Prelude.rnf comment `Prelude.seq`
      Prelude.rnf finalized `Prelude.seq`
        Prelude.rnf revocationComment `Prelude.seq`
          Prelude.rnf revoked `Prelude.seq`
            Prelude.rnf revokedAt `Prelude.seq`
              Prelude.rnf sourceId `Prelude.seq`
                Prelude.rnf arn `Prelude.seq`
                  Prelude.rnf createdAt `Prelude.seq`
                    Prelude.rnf dataSetId `Prelude.seq`
                      Prelude.rnf id `Prelude.seq`
                        Prelude.rnf updatedAt
