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
-- Module      : Amazonka.Kendra.DescribeFeaturedResultsSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a set of featured results. Features results are
-- placed above all other results for certain queries. If there\'s an exact
-- match of a query, then one or more specific documents are featured in
-- the search results.
module Amazonka.Kendra.DescribeFeaturedResultsSet
  ( -- * Creating a Request
    DescribeFeaturedResultsSet (..),
    newDescribeFeaturedResultsSet,

    -- * Request Lenses
    describeFeaturedResultsSet_indexId,
    describeFeaturedResultsSet_featuredResultsSetId,

    -- * Destructuring the Response
    DescribeFeaturedResultsSetResponse (..),
    newDescribeFeaturedResultsSetResponse,

    -- * Response Lenses
    describeFeaturedResultsSetResponse_creationTimestamp,
    describeFeaturedResultsSetResponse_description,
    describeFeaturedResultsSetResponse_featuredDocumentsMissing,
    describeFeaturedResultsSetResponse_featuredDocumentsWithMetadata,
    describeFeaturedResultsSetResponse_featuredResultsSetId,
    describeFeaturedResultsSetResponse_featuredResultsSetName,
    describeFeaturedResultsSetResponse_lastUpdatedTimestamp,
    describeFeaturedResultsSetResponse_queryTexts,
    describeFeaturedResultsSetResponse_status,
    describeFeaturedResultsSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFeaturedResultsSet' smart constructor.
data DescribeFeaturedResultsSet = DescribeFeaturedResultsSet'
  { -- | The identifier of the index used for featuring results.
    indexId :: Prelude.Text,
    -- | The identifier of the set of featured results that you want to get
    -- information on.
    featuredResultsSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeaturedResultsSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'describeFeaturedResultsSet_indexId' - The identifier of the index used for featuring results.
--
-- 'featuredResultsSetId', 'describeFeaturedResultsSet_featuredResultsSetId' - The identifier of the set of featured results that you want to get
-- information on.
newDescribeFeaturedResultsSet ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'featuredResultsSetId'
  Prelude.Text ->
  DescribeFeaturedResultsSet
newDescribeFeaturedResultsSet
  pIndexId_
  pFeaturedResultsSetId_ =
    DescribeFeaturedResultsSet'
      { indexId = pIndexId_,
        featuredResultsSetId = pFeaturedResultsSetId_
      }

-- | The identifier of the index used for featuring results.
describeFeaturedResultsSet_indexId :: Lens.Lens' DescribeFeaturedResultsSet Prelude.Text
describeFeaturedResultsSet_indexId = Lens.lens (\DescribeFeaturedResultsSet' {indexId} -> indexId) (\s@DescribeFeaturedResultsSet' {} a -> s {indexId = a} :: DescribeFeaturedResultsSet)

-- | The identifier of the set of featured results that you want to get
-- information on.
describeFeaturedResultsSet_featuredResultsSetId :: Lens.Lens' DescribeFeaturedResultsSet Prelude.Text
describeFeaturedResultsSet_featuredResultsSetId = Lens.lens (\DescribeFeaturedResultsSet' {featuredResultsSetId} -> featuredResultsSetId) (\s@DescribeFeaturedResultsSet' {} a -> s {featuredResultsSetId = a} :: DescribeFeaturedResultsSet)

instance Core.AWSRequest DescribeFeaturedResultsSet where
  type
    AWSResponse DescribeFeaturedResultsSet =
      DescribeFeaturedResultsSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFeaturedResultsSetResponse'
            Prelude.<$> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> ( x
                            Data..?> "FeaturedDocumentsMissing"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "FeaturedDocumentsWithMetadata"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "FeaturedResultsSetId")
            Prelude.<*> (x Data..?> "FeaturedResultsSetName")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "QueryTexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFeaturedResultsSet where
  hashWithSalt _salt DescribeFeaturedResultsSet' {..} =
    _salt
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` featuredResultsSetId

instance Prelude.NFData DescribeFeaturedResultsSet where
  rnf DescribeFeaturedResultsSet' {..} =
    Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf featuredResultsSetId

instance Data.ToHeaders DescribeFeaturedResultsSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DescribeFeaturedResultsSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFeaturedResultsSet where
  toJSON DescribeFeaturedResultsSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just
              ( "FeaturedResultsSetId"
                  Data..= featuredResultsSetId
              )
          ]
      )

instance Data.ToPath DescribeFeaturedResultsSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFeaturedResultsSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFeaturedResultsSetResponse' smart constructor.
data DescribeFeaturedResultsSetResponse = DescribeFeaturedResultsSetResponse'
  { -- | The Unix timestamp when the set of the featured results was created.
    creationTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The description for the set of featured results.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of document IDs that don\'t exist but you have specified as
    -- featured documents. Amazon Kendra cannot feature these documents if they
    -- don\'t exist in the index. You can check the status of a document and
    -- its ID or check for documents with status errors using the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchGetDocumentStatus.html BatchGetDocumentStatus>
    -- API.
    featuredDocumentsMissing :: Prelude.Maybe [FeaturedDocumentMissing],
    -- | The list of document IDs for the documents you want to feature with
    -- their metadata information. For more information on the list of featured
    -- documents, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
    featuredDocumentsWithMetadata :: Prelude.Maybe [FeaturedDocumentWithMetadata],
    -- | The identifier of the set of featured results.
    featuredResultsSetId :: Prelude.Maybe Prelude.Text,
    -- | The name for the set of featured results.
    featuredResultsSetName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the set of featured results was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The list of queries for featuring results. For more information on the
    -- list of queries, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
    queryTexts :: Prelude.Maybe [Prelude.Text],
    -- | The current status of the set of featured results. When the value is
    -- @ACTIVE@, featured results are ready for use. You can still configure
    -- your settings before setting the status to @ACTIVE@. You can set the
    -- status to @ACTIVE@ or @INACTIVE@ using the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
    -- API. The queries you specify for featured results must be unique per
    -- featured results set for each index, whether the status is @ACTIVE@ or
    -- @INACTIVE@.
    status :: Prelude.Maybe FeaturedResultsSetStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeaturedResultsSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'describeFeaturedResultsSetResponse_creationTimestamp' - The Unix timestamp when the set of the featured results was created.
--
-- 'description', 'describeFeaturedResultsSetResponse_description' - The description for the set of featured results.
--
-- 'featuredDocumentsMissing', 'describeFeaturedResultsSetResponse_featuredDocumentsMissing' - The list of document IDs that don\'t exist but you have specified as
-- featured documents. Amazon Kendra cannot feature these documents if they
-- don\'t exist in the index. You can check the status of a document and
-- its ID or check for documents with status errors using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchGetDocumentStatus.html BatchGetDocumentStatus>
-- API.
--
-- 'featuredDocumentsWithMetadata', 'describeFeaturedResultsSetResponse_featuredDocumentsWithMetadata' - The list of document IDs for the documents you want to feature with
-- their metadata information. For more information on the list of featured
-- documents, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
--
-- 'featuredResultsSetId', 'describeFeaturedResultsSetResponse_featuredResultsSetId' - The identifier of the set of featured results.
--
-- 'featuredResultsSetName', 'describeFeaturedResultsSetResponse_featuredResultsSetName' - The name for the set of featured results.
--
-- 'lastUpdatedTimestamp', 'describeFeaturedResultsSetResponse_lastUpdatedTimestamp' - The timestamp when the set of featured results was last updated.
--
-- 'queryTexts', 'describeFeaturedResultsSetResponse_queryTexts' - The list of queries for featuring results. For more information on the
-- list of queries, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
--
-- 'status', 'describeFeaturedResultsSetResponse_status' - The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
--
-- 'httpStatus', 'describeFeaturedResultsSetResponse_httpStatus' - The response's http status code.
newDescribeFeaturedResultsSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFeaturedResultsSetResponse
newDescribeFeaturedResultsSetResponse pHttpStatus_ =
  DescribeFeaturedResultsSetResponse'
    { creationTimestamp =
        Prelude.Nothing,
      description = Prelude.Nothing,
      featuredDocumentsMissing =
        Prelude.Nothing,
      featuredDocumentsWithMetadata =
        Prelude.Nothing,
      featuredResultsSetId = Prelude.Nothing,
      featuredResultsSetName =
        Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      queryTexts = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Unix timestamp when the set of the featured results was created.
describeFeaturedResultsSetResponse_creationTimestamp :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe Prelude.Integer)
describeFeaturedResultsSetResponse_creationTimestamp = Lens.lens (\DescribeFeaturedResultsSetResponse' {creationTimestamp} -> creationTimestamp) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {creationTimestamp = a} :: DescribeFeaturedResultsSetResponse)

-- | The description for the set of featured results.
describeFeaturedResultsSetResponse_description :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe Prelude.Text)
describeFeaturedResultsSetResponse_description = Lens.lens (\DescribeFeaturedResultsSetResponse' {description} -> description) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {description = a} :: DescribeFeaturedResultsSetResponse)

-- | The list of document IDs that don\'t exist but you have specified as
-- featured documents. Amazon Kendra cannot feature these documents if they
-- don\'t exist in the index. You can check the status of a document and
-- its ID or check for documents with status errors using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchGetDocumentStatus.html BatchGetDocumentStatus>
-- API.
describeFeaturedResultsSetResponse_featuredDocumentsMissing :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe [FeaturedDocumentMissing])
describeFeaturedResultsSetResponse_featuredDocumentsMissing = Lens.lens (\DescribeFeaturedResultsSetResponse' {featuredDocumentsMissing} -> featuredDocumentsMissing) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {featuredDocumentsMissing = a} :: DescribeFeaturedResultsSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of document IDs for the documents you want to feature with
-- their metadata information. For more information on the list of featured
-- documents, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
describeFeaturedResultsSetResponse_featuredDocumentsWithMetadata :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe [FeaturedDocumentWithMetadata])
describeFeaturedResultsSetResponse_featuredDocumentsWithMetadata = Lens.lens (\DescribeFeaturedResultsSetResponse' {featuredDocumentsWithMetadata} -> featuredDocumentsWithMetadata) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {featuredDocumentsWithMetadata = a} :: DescribeFeaturedResultsSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the set of featured results.
describeFeaturedResultsSetResponse_featuredResultsSetId :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe Prelude.Text)
describeFeaturedResultsSetResponse_featuredResultsSetId = Lens.lens (\DescribeFeaturedResultsSetResponse' {featuredResultsSetId} -> featuredResultsSetId) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {featuredResultsSetId = a} :: DescribeFeaturedResultsSetResponse)

-- | The name for the set of featured results.
describeFeaturedResultsSetResponse_featuredResultsSetName :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe Prelude.Text)
describeFeaturedResultsSetResponse_featuredResultsSetName = Lens.lens (\DescribeFeaturedResultsSetResponse' {featuredResultsSetName} -> featuredResultsSetName) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {featuredResultsSetName = a} :: DescribeFeaturedResultsSetResponse)

-- | The timestamp when the set of featured results was last updated.
describeFeaturedResultsSetResponse_lastUpdatedTimestamp :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe Prelude.Integer)
describeFeaturedResultsSetResponse_lastUpdatedTimestamp = Lens.lens (\DescribeFeaturedResultsSetResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {lastUpdatedTimestamp = a} :: DescribeFeaturedResultsSetResponse)

-- | The list of queries for featuring results. For more information on the
-- list of queries, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
describeFeaturedResultsSetResponse_queryTexts :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe [Prelude.Text])
describeFeaturedResultsSetResponse_queryTexts = Lens.lens (\DescribeFeaturedResultsSetResponse' {queryTexts} -> queryTexts) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {queryTexts = a} :: DescribeFeaturedResultsSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
describeFeaturedResultsSetResponse_status :: Lens.Lens' DescribeFeaturedResultsSetResponse (Prelude.Maybe FeaturedResultsSetStatus)
describeFeaturedResultsSetResponse_status = Lens.lens (\DescribeFeaturedResultsSetResponse' {status} -> status) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {status = a} :: DescribeFeaturedResultsSetResponse)

-- | The response's http status code.
describeFeaturedResultsSetResponse_httpStatus :: Lens.Lens' DescribeFeaturedResultsSetResponse Prelude.Int
describeFeaturedResultsSetResponse_httpStatus = Lens.lens (\DescribeFeaturedResultsSetResponse' {httpStatus} -> httpStatus) (\s@DescribeFeaturedResultsSetResponse' {} a -> s {httpStatus = a} :: DescribeFeaturedResultsSetResponse)

instance
  Prelude.NFData
    DescribeFeaturedResultsSetResponse
  where
  rnf DescribeFeaturedResultsSetResponse' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf featuredDocumentsMissing
      `Prelude.seq` Prelude.rnf featuredDocumentsWithMetadata
      `Prelude.seq` Prelude.rnf featuredResultsSetId
      `Prelude.seq` Prelude.rnf featuredResultsSetName
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf queryTexts
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
