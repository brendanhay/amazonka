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
-- Module      : Amazonka.DataExchange.CreateRevision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a revision for a data set.
module Amazonka.DataExchange.CreateRevision
  ( -- * Creating a Request
    CreateRevision (..),
    newCreateRevision,

    -- * Request Lenses
    createRevision_tags,
    createRevision_comment,
    createRevision_dataSetId,

    -- * Destructuring the Response
    CreateRevisionResponse (..),
    newCreateRevisionResponse,

    -- * Response Lenses
    createRevisionResponse_tags,
    createRevisionResponse_sourceId,
    createRevisionResponse_revocationComment,
    createRevisionResponse_arn,
    createRevisionResponse_id,
    createRevisionResponse_comment,
    createRevisionResponse_finalized,
    createRevisionResponse_dataSetId,
    createRevisionResponse_revokedAt,
    createRevisionResponse_revoked,
    createRevisionResponse_createdAt,
    createRevisionResponse_updatedAt,
    createRevisionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRevision' smart constructor.
data CreateRevision = CreateRevision'
  { -- | A revision tag is an optional label that you can assign to a revision
    -- when you create it. Each tag consists of a key and an optional value,
    -- both of which you define. When you use tagging, you can also use
    -- tag-based access control in IAM policies to control access to these data
    -- sets and revisions.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRevision_tags' - A revision tag is an optional label that you can assign to a revision
-- when you create it. Each tag consists of a key and an optional value,
-- both of which you define. When you use tagging, you can also use
-- tag-based access control in IAM policies to control access to these data
-- sets and revisions.
--
-- 'comment', 'createRevision_comment' - An optional comment about the revision.
--
-- 'dataSetId', 'createRevision_dataSetId' - The unique identifier for a data set.
newCreateRevision ::
  -- | 'dataSetId'
  Prelude.Text ->
  CreateRevision
newCreateRevision pDataSetId_ =
  CreateRevision'
    { tags = Prelude.Nothing,
      comment = Prelude.Nothing,
      dataSetId = pDataSetId_
    }

-- | A revision tag is an optional label that you can assign to a revision
-- when you create it. Each tag consists of a key and an optional value,
-- both of which you define. When you use tagging, you can also use
-- tag-based access control in IAM policies to control access to these data
-- sets and revisions.
createRevision_tags :: Lens.Lens' CreateRevision (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRevision_tags = Lens.lens (\CreateRevision' {tags} -> tags) (\s@CreateRevision' {} a -> s {tags = a} :: CreateRevision) Prelude.. Lens.mapping Lens.coerced

-- | An optional comment about the revision.
createRevision_comment :: Lens.Lens' CreateRevision (Prelude.Maybe Prelude.Text)
createRevision_comment = Lens.lens (\CreateRevision' {comment} -> comment) (\s@CreateRevision' {} a -> s {comment = a} :: CreateRevision)

-- | The unique identifier for a data set.
createRevision_dataSetId :: Lens.Lens' CreateRevision Prelude.Text
createRevision_dataSetId = Lens.lens (\CreateRevision' {dataSetId} -> dataSetId) (\s@CreateRevision' {} a -> s {dataSetId = a} :: CreateRevision)

instance Core.AWSRequest CreateRevision where
  type
    AWSResponse CreateRevision =
      CreateRevisionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRevisionResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "SourceId")
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

instance Prelude.Hashable CreateRevision where
  hashWithSalt _salt CreateRevision' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData CreateRevision where
  rnf CreateRevision' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf dataSetId

instance Core.ToHeaders CreateRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRevision where
  toJSON CreateRevision' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Comment" Core..=) Prelude.<$> comment
          ]
      )

instance Core.ToPath CreateRevision where
  toPath CreateRevision' {..} =
    Prelude.mconcat
      ["/v1/data-sets/", Core.toBS dataSetId, "/revisions"]

instance Core.ToQuery CreateRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRevisionResponse' smart constructor.
data CreateRevisionResponse = CreateRevisionResponse'
  { -- | The tags for the revision.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The revision ID of the owned revision corresponding to the entitled
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
    -- your changes to the assets in the revision are complete. After it\'s in
    -- this read-only state, you can publish the revision to your products.
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
-- Create a value of 'CreateRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRevisionResponse_tags' - The tags for the revision.
--
-- 'sourceId', 'createRevisionResponse_sourceId' - The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
--
-- 'revocationComment', 'createRevisionResponse_revocationComment' - A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
--
-- 'arn', 'createRevisionResponse_arn' - The ARN for the revision.
--
-- 'id', 'createRevisionResponse_id' - The unique identifier for the revision.
--
-- 'comment', 'createRevisionResponse_comment' - An optional comment about the revision.
--
-- 'finalized', 'createRevisionResponse_finalized' - To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
--
-- 'dataSetId', 'createRevisionResponse_dataSetId' - The unique identifier for the data set associated with this revision.
--
-- 'revokedAt', 'createRevisionResponse_revokedAt' - The date and time that the revision was revoked, in ISO 8601 format.
--
-- 'revoked', 'createRevisionResponse_revoked' - A status indicating that subscribers\' access to the revision was
-- revoked.
--
-- 'createdAt', 'createRevisionResponse_createdAt' - The date and time that the revision was created, in ISO 8601 format.
--
-- 'updatedAt', 'createRevisionResponse_updatedAt' - The date and time that the revision was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'createRevisionResponse_httpStatus' - The response's http status code.
newCreateRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRevisionResponse
newCreateRevisionResponse pHttpStatus_ =
  CreateRevisionResponse'
    { tags = Prelude.Nothing,
      sourceId = Prelude.Nothing,
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

-- | The tags for the revision.
createRevisionResponse_tags :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRevisionResponse_tags = Lens.lens (\CreateRevisionResponse' {tags} -> tags) (\s@CreateRevisionResponse' {} a -> s {tags = a} :: CreateRevisionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
createRevisionResponse_sourceId :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Text)
createRevisionResponse_sourceId = Lens.lens (\CreateRevisionResponse' {sourceId} -> sourceId) (\s@CreateRevisionResponse' {} a -> s {sourceId = a} :: CreateRevisionResponse)

-- | A required comment to inform subscribers of the reason their access to
-- the revision was revoked.
createRevisionResponse_revocationComment :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Text)
createRevisionResponse_revocationComment = Lens.lens (\CreateRevisionResponse' {revocationComment} -> revocationComment) (\s@CreateRevisionResponse' {} a -> s {revocationComment = a} :: CreateRevisionResponse)

-- | The ARN for the revision.
createRevisionResponse_arn :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Text)
createRevisionResponse_arn = Lens.lens (\CreateRevisionResponse' {arn} -> arn) (\s@CreateRevisionResponse' {} a -> s {arn = a} :: CreateRevisionResponse)

-- | The unique identifier for the revision.
createRevisionResponse_id :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Text)
createRevisionResponse_id = Lens.lens (\CreateRevisionResponse' {id} -> id) (\s@CreateRevisionResponse' {} a -> s {id = a} :: CreateRevisionResponse)

-- | An optional comment about the revision.
createRevisionResponse_comment :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Text)
createRevisionResponse_comment = Lens.lens (\CreateRevisionResponse' {comment} -> comment) (\s@CreateRevisionResponse' {} a -> s {comment = a} :: CreateRevisionResponse)

-- | To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
createRevisionResponse_finalized :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Bool)
createRevisionResponse_finalized = Lens.lens (\CreateRevisionResponse' {finalized} -> finalized) (\s@CreateRevisionResponse' {} a -> s {finalized = a} :: CreateRevisionResponse)

-- | The unique identifier for the data set associated with this revision.
createRevisionResponse_dataSetId :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Text)
createRevisionResponse_dataSetId = Lens.lens (\CreateRevisionResponse' {dataSetId} -> dataSetId) (\s@CreateRevisionResponse' {} a -> s {dataSetId = a} :: CreateRevisionResponse)

-- | The date and time that the revision was revoked, in ISO 8601 format.
createRevisionResponse_revokedAt :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
createRevisionResponse_revokedAt = Lens.lens (\CreateRevisionResponse' {revokedAt} -> revokedAt) (\s@CreateRevisionResponse' {} a -> s {revokedAt = a} :: CreateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | A status indicating that subscribers\' access to the revision was
-- revoked.
createRevisionResponse_revoked :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.Bool)
createRevisionResponse_revoked = Lens.lens (\CreateRevisionResponse' {revoked} -> revoked) (\s@CreateRevisionResponse' {} a -> s {revoked = a} :: CreateRevisionResponse)

-- | The date and time that the revision was created, in ISO 8601 format.
createRevisionResponse_createdAt :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
createRevisionResponse_createdAt = Lens.lens (\CreateRevisionResponse' {createdAt} -> createdAt) (\s@CreateRevisionResponse' {} a -> s {createdAt = a} :: CreateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time that the revision was last updated, in ISO 8601
-- format.
createRevisionResponse_updatedAt :: Lens.Lens' CreateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
createRevisionResponse_updatedAt = Lens.lens (\CreateRevisionResponse' {updatedAt} -> updatedAt) (\s@CreateRevisionResponse' {} a -> s {updatedAt = a} :: CreateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
createRevisionResponse_httpStatus :: Lens.Lens' CreateRevisionResponse Prelude.Int
createRevisionResponse_httpStatus = Lens.lens (\CreateRevisionResponse' {httpStatus} -> httpStatus) (\s@CreateRevisionResponse' {} a -> s {httpStatus = a} :: CreateRevisionResponse)

instance Prelude.NFData CreateRevisionResponse where
  rnf CreateRevisionResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceId
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
