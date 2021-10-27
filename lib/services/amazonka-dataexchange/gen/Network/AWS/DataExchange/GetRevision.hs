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
-- Module      : Network.AWS.DataExchange.GetRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a revision.
module Network.AWS.DataExchange.GetRevision
  ( -- * Creating a Request
    GetRevision (..),
    newGetRevision,

    -- * Request Lenses
    getRevision_revisionId,
    getRevision_dataSetId,

    -- * Destructuring the Response
    GetRevisionResponse (..),
    newGetRevisionResponse,

    -- * Response Lenses
    getRevisionResponse_arn,
    getRevisionResponse_createdAt,
    getRevisionResponse_sourceId,
    getRevisionResponse_finalized,
    getRevisionResponse_dataSetId,
    getRevisionResponse_id,
    getRevisionResponse_updatedAt,
    getRevisionResponse_comment,
    getRevisionResponse_tags,
    getRevisionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataExchange.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRevision' smart constructor.
data GetRevision = GetRevision'
  { -- | The unique identifier for a revision.
    revisionId :: Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
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
-- 'revisionId', 'getRevision_revisionId' - The unique identifier for a revision.
--
-- 'dataSetId', 'getRevision_dataSetId' - The unique identifier for a data set.
newGetRevision ::
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  GetRevision
newGetRevision pRevisionId_ pDataSetId_ =
  GetRevision'
    { revisionId = pRevisionId_,
      dataSetId = pDataSetId_
    }

-- | The unique identifier for a revision.
getRevision_revisionId :: Lens.Lens' GetRevision Prelude.Text
getRevision_revisionId = Lens.lens (\GetRevision' {revisionId} -> revisionId) (\s@GetRevision' {} a -> s {revisionId = a} :: GetRevision)

-- | The unique identifier for a data set.
getRevision_dataSetId :: Lens.Lens' GetRevision Prelude.Text
getRevision_dataSetId = Lens.lens (\GetRevision' {dataSetId} -> dataSetId) (\s@GetRevision' {} a -> s {dataSetId = a} :: GetRevision)

instance Core.AWSRequest GetRevision where
  type AWSResponse GetRevision = GetRevisionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRevisionResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "SourceId")
            Prelude.<*> (x Core..?> "Finalized")
            Prelude.<*> (x Core..?> "DataSetId")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (x Core..?> "Comment")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRevision

instance Prelude.NFData GetRevision

instance Core.ToHeaders GetRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetRevision where
  toPath GetRevision' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Core.toBS dataSetId,
        "/revisions/",
        Core.toBS revisionId
      ]

instance Core.ToQuery GetRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRevisionResponse' smart constructor.
data GetRevisionResponse = GetRevisionResponse'
  { -- | The ARN for the revision.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The revision ID of the owned revision corresponding to the entitled
    -- revision being viewed. This parameter is returned when a revision owner
    -- is viewing the entitled copy of its owned revision.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | To publish a revision to a data set in a product, the revision must
    -- first be finalized. Finalizing a revision tells AWS Data Exchange that
    -- your changes to the assets in the revision are complete. After it\'s in
    -- this read-only state, you can publish the revision to your products.
    --
    -- Finalized revisions can be published through the AWS Data Exchange
    -- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
    -- Marketplace Catalog API action. When using the API, revisions are
    -- uniquely identified by their ARN.
    finalized :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the data set associated with this revision.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the revision.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The tags for the revision.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'createdAt', 'getRevisionResponse_createdAt' - The date and time that the revision was created, in ISO 8601 format.
--
-- 'sourceId', 'getRevisionResponse_sourceId' - The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
--
-- 'finalized', 'getRevisionResponse_finalized' - To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
--
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
--
-- 'dataSetId', 'getRevisionResponse_dataSetId' - The unique identifier for the data set associated with this revision.
--
-- 'id', 'getRevisionResponse_id' - The unique identifier for the revision.
--
-- 'updatedAt', 'getRevisionResponse_updatedAt' - The date and time that the revision was last updated, in ISO 8601
-- format.
--
-- 'comment', 'getRevisionResponse_comment' - An optional comment about the revision.
--
-- 'tags', 'getRevisionResponse_tags' - The tags for the revision.
--
-- 'httpStatus', 'getRevisionResponse_httpStatus' - The response's http status code.
newGetRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRevisionResponse
newGetRevisionResponse pHttpStatus_ =
  GetRevisionResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      finalized = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      comment = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the revision.
getRevisionResponse_arn :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_arn = Lens.lens (\GetRevisionResponse' {arn} -> arn) (\s@GetRevisionResponse' {} a -> s {arn = a} :: GetRevisionResponse)

-- | The date and time that the revision was created, in ISO 8601 format.
getRevisionResponse_createdAt :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.UTCTime)
getRevisionResponse_createdAt = Lens.lens (\GetRevisionResponse' {createdAt} -> createdAt) (\s@GetRevisionResponse' {} a -> s {createdAt = a} :: GetRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
getRevisionResponse_sourceId :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_sourceId = Lens.lens (\GetRevisionResponse' {sourceId} -> sourceId) (\s@GetRevisionResponse' {} a -> s {sourceId = a} :: GetRevisionResponse)

-- | To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- your changes to the assets in the revision are complete. After it\'s in
-- this read-only state, you can publish the revision to your products.
--
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
getRevisionResponse_finalized :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Bool)
getRevisionResponse_finalized = Lens.lens (\GetRevisionResponse' {finalized} -> finalized) (\s@GetRevisionResponse' {} a -> s {finalized = a} :: GetRevisionResponse)

-- | The unique identifier for the data set associated with this revision.
getRevisionResponse_dataSetId :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_dataSetId = Lens.lens (\GetRevisionResponse' {dataSetId} -> dataSetId) (\s@GetRevisionResponse' {} a -> s {dataSetId = a} :: GetRevisionResponse)

-- | The unique identifier for the revision.
getRevisionResponse_id :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_id = Lens.lens (\GetRevisionResponse' {id} -> id) (\s@GetRevisionResponse' {} a -> s {id = a} :: GetRevisionResponse)

-- | The date and time that the revision was last updated, in ISO 8601
-- format.
getRevisionResponse_updatedAt :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.UTCTime)
getRevisionResponse_updatedAt = Lens.lens (\GetRevisionResponse' {updatedAt} -> updatedAt) (\s@GetRevisionResponse' {} a -> s {updatedAt = a} :: GetRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | An optional comment about the revision.
getRevisionResponse_comment :: Lens.Lens' GetRevisionResponse (Prelude.Maybe Prelude.Text)
getRevisionResponse_comment = Lens.lens (\GetRevisionResponse' {comment} -> comment) (\s@GetRevisionResponse' {} a -> s {comment = a} :: GetRevisionResponse)

-- | The tags for the revision.
getRevisionResponse_tags :: Lens.Lens' GetRevisionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRevisionResponse_tags = Lens.lens (\GetRevisionResponse' {tags} -> tags) (\s@GetRevisionResponse' {} a -> s {tags = a} :: GetRevisionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRevisionResponse_httpStatus :: Lens.Lens' GetRevisionResponse Prelude.Int
getRevisionResponse_httpStatus = Lens.lens (\GetRevisionResponse' {httpStatus} -> httpStatus) (\s@GetRevisionResponse' {} a -> s {httpStatus = a} :: GetRevisionResponse)

instance Prelude.NFData GetRevisionResponse
