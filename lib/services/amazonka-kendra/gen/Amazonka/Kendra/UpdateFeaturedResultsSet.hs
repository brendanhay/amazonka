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
-- Module      : Amazonka.Kendra.UpdateFeaturedResultsSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a set of featured results. Features results are placed above all
-- other results for certain queries. You map specific queries to specific
-- documents for featuring in the results. If a query contains an exact
-- match of a query, then one or more specific documents are featured in
-- the search results.
module Amazonka.Kendra.UpdateFeaturedResultsSet
  ( -- * Creating a Request
    UpdateFeaturedResultsSet (..),
    newUpdateFeaturedResultsSet,

    -- * Request Lenses
    updateFeaturedResultsSet_description,
    updateFeaturedResultsSet_featuredDocuments,
    updateFeaturedResultsSet_featuredResultsSetName,
    updateFeaturedResultsSet_queryTexts,
    updateFeaturedResultsSet_status,
    updateFeaturedResultsSet_indexId,
    updateFeaturedResultsSet_featuredResultsSetId,

    -- * Destructuring the Response
    UpdateFeaturedResultsSetResponse (..),
    newUpdateFeaturedResultsSetResponse,

    -- * Response Lenses
    updateFeaturedResultsSetResponse_featuredResultsSet,
    updateFeaturedResultsSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFeaturedResultsSet' smart constructor.
data UpdateFeaturedResultsSet = UpdateFeaturedResultsSet'
  { -- | A new description for the set of featured results.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of document IDs for the documents you want to feature at the top
    -- of the search results page. For more information on the list of featured
    -- documents, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
    featuredDocuments :: Prelude.Maybe [FeaturedDocument],
    -- | A new name for the set of featured results.
    featuredResultsSetName :: Prelude.Maybe Prelude.Text,
    -- | A list of queries for featuring results. For more information on the
    -- list of queries, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
    queryTexts :: Prelude.Maybe [Prelude.Text],
    -- | You can set the status to @ACTIVE@ or @INACTIVE@. When the value is
    -- @ACTIVE@, featured results are ready for use. You can still configure
    -- your settings before setting the status to @ACTIVE@. The queries you
    -- specify for featured results must be unique per featured results set for
    -- each index, whether the status is @ACTIVE@ or @INACTIVE@.
    status :: Prelude.Maybe FeaturedResultsSetStatus,
    -- | The identifier of the index used for featuring results.
    indexId :: Prelude.Text,
    -- | The identifier of the set of featured results that you want to update.
    featuredResultsSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeaturedResultsSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateFeaturedResultsSet_description' - A new description for the set of featured results.
--
-- 'featuredDocuments', 'updateFeaturedResultsSet_featuredDocuments' - A list of document IDs for the documents you want to feature at the top
-- of the search results page. For more information on the list of featured
-- documents, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
--
-- 'featuredResultsSetName', 'updateFeaturedResultsSet_featuredResultsSetName' - A new name for the set of featured results.
--
-- 'queryTexts', 'updateFeaturedResultsSet_queryTexts' - A list of queries for featuring results. For more information on the
-- list of queries, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
--
-- 'status', 'updateFeaturedResultsSet_status' - You can set the status to @ACTIVE@ or @INACTIVE@. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. The queries you
-- specify for featured results must be unique per featured results set for
-- each index, whether the status is @ACTIVE@ or @INACTIVE@.
--
-- 'indexId', 'updateFeaturedResultsSet_indexId' - The identifier of the index used for featuring results.
--
-- 'featuredResultsSetId', 'updateFeaturedResultsSet_featuredResultsSetId' - The identifier of the set of featured results that you want to update.
newUpdateFeaturedResultsSet ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'featuredResultsSetId'
  Prelude.Text ->
  UpdateFeaturedResultsSet
newUpdateFeaturedResultsSet
  pIndexId_
  pFeaturedResultsSetId_ =
    UpdateFeaturedResultsSet'
      { description =
          Prelude.Nothing,
        featuredDocuments = Prelude.Nothing,
        featuredResultsSetName = Prelude.Nothing,
        queryTexts = Prelude.Nothing,
        status = Prelude.Nothing,
        indexId = pIndexId_,
        featuredResultsSetId = pFeaturedResultsSetId_
      }

-- | A new description for the set of featured results.
updateFeaturedResultsSet_description :: Lens.Lens' UpdateFeaturedResultsSet (Prelude.Maybe Prelude.Text)
updateFeaturedResultsSet_description = Lens.lens (\UpdateFeaturedResultsSet' {description} -> description) (\s@UpdateFeaturedResultsSet' {} a -> s {description = a} :: UpdateFeaturedResultsSet)

-- | A list of document IDs for the documents you want to feature at the top
-- of the search results page. For more information on the list of featured
-- documents, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
updateFeaturedResultsSet_featuredDocuments :: Lens.Lens' UpdateFeaturedResultsSet (Prelude.Maybe [FeaturedDocument])
updateFeaturedResultsSet_featuredDocuments = Lens.lens (\UpdateFeaturedResultsSet' {featuredDocuments} -> featuredDocuments) (\s@UpdateFeaturedResultsSet' {} a -> s {featuredDocuments = a} :: UpdateFeaturedResultsSet) Prelude.. Lens.mapping Lens.coerced

-- | A new name for the set of featured results.
updateFeaturedResultsSet_featuredResultsSetName :: Lens.Lens' UpdateFeaturedResultsSet (Prelude.Maybe Prelude.Text)
updateFeaturedResultsSet_featuredResultsSetName = Lens.lens (\UpdateFeaturedResultsSet' {featuredResultsSetName} -> featuredResultsSetName) (\s@UpdateFeaturedResultsSet' {} a -> s {featuredResultsSetName = a} :: UpdateFeaturedResultsSet)

-- | A list of queries for featuring results. For more information on the
-- list of queries, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
updateFeaturedResultsSet_queryTexts :: Lens.Lens' UpdateFeaturedResultsSet (Prelude.Maybe [Prelude.Text])
updateFeaturedResultsSet_queryTexts = Lens.lens (\UpdateFeaturedResultsSet' {queryTexts} -> queryTexts) (\s@UpdateFeaturedResultsSet' {} a -> s {queryTexts = a} :: UpdateFeaturedResultsSet) Prelude.. Lens.mapping Lens.coerced

-- | You can set the status to @ACTIVE@ or @INACTIVE@. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. The queries you
-- specify for featured results must be unique per featured results set for
-- each index, whether the status is @ACTIVE@ or @INACTIVE@.
updateFeaturedResultsSet_status :: Lens.Lens' UpdateFeaturedResultsSet (Prelude.Maybe FeaturedResultsSetStatus)
updateFeaturedResultsSet_status = Lens.lens (\UpdateFeaturedResultsSet' {status} -> status) (\s@UpdateFeaturedResultsSet' {} a -> s {status = a} :: UpdateFeaturedResultsSet)

-- | The identifier of the index used for featuring results.
updateFeaturedResultsSet_indexId :: Lens.Lens' UpdateFeaturedResultsSet Prelude.Text
updateFeaturedResultsSet_indexId = Lens.lens (\UpdateFeaturedResultsSet' {indexId} -> indexId) (\s@UpdateFeaturedResultsSet' {} a -> s {indexId = a} :: UpdateFeaturedResultsSet)

-- | The identifier of the set of featured results that you want to update.
updateFeaturedResultsSet_featuredResultsSetId :: Lens.Lens' UpdateFeaturedResultsSet Prelude.Text
updateFeaturedResultsSet_featuredResultsSetId = Lens.lens (\UpdateFeaturedResultsSet' {featuredResultsSetId} -> featuredResultsSetId) (\s@UpdateFeaturedResultsSet' {} a -> s {featuredResultsSetId = a} :: UpdateFeaturedResultsSet)

instance Core.AWSRequest UpdateFeaturedResultsSet where
  type
    AWSResponse UpdateFeaturedResultsSet =
      UpdateFeaturedResultsSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFeaturedResultsSetResponse'
            Prelude.<$> (x Data..?> "FeaturedResultsSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFeaturedResultsSet where
  hashWithSalt _salt UpdateFeaturedResultsSet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` featuredDocuments
      `Prelude.hashWithSalt` featuredResultsSetName
      `Prelude.hashWithSalt` queryTexts
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` featuredResultsSetId

instance Prelude.NFData UpdateFeaturedResultsSet where
  rnf UpdateFeaturedResultsSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf featuredDocuments
      `Prelude.seq` Prelude.rnf featuredResultsSetName
      `Prelude.seq` Prelude.rnf queryTexts
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf featuredResultsSetId

instance Data.ToHeaders UpdateFeaturedResultsSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.UpdateFeaturedResultsSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFeaturedResultsSet where
  toJSON UpdateFeaturedResultsSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("FeaturedDocuments" Data..=)
              Prelude.<$> featuredDocuments,
            ("FeaturedResultsSetName" Data..=)
              Prelude.<$> featuredResultsSetName,
            ("QueryTexts" Data..=) Prelude.<$> queryTexts,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just
              ( "FeaturedResultsSetId"
                  Data..= featuredResultsSetId
              )
          ]
      )

instance Data.ToPath UpdateFeaturedResultsSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFeaturedResultsSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFeaturedResultsSetResponse' smart constructor.
data UpdateFeaturedResultsSetResponse = UpdateFeaturedResultsSetResponse'
  { -- | Information on the set of featured results. This includes the identifier
    -- of the featured results set, whether the featured results set is active
    -- or inactive, when the featured results set was last updated, and more.
    featuredResultsSet :: Prelude.Maybe FeaturedResultsSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeaturedResultsSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featuredResultsSet', 'updateFeaturedResultsSetResponse_featuredResultsSet' - Information on the set of featured results. This includes the identifier
-- of the featured results set, whether the featured results set is active
-- or inactive, when the featured results set was last updated, and more.
--
-- 'httpStatus', 'updateFeaturedResultsSetResponse_httpStatus' - The response's http status code.
newUpdateFeaturedResultsSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFeaturedResultsSetResponse
newUpdateFeaturedResultsSetResponse pHttpStatus_ =
  UpdateFeaturedResultsSetResponse'
    { featuredResultsSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the set of featured results. This includes the identifier
-- of the featured results set, whether the featured results set is active
-- or inactive, when the featured results set was last updated, and more.
updateFeaturedResultsSetResponse_featuredResultsSet :: Lens.Lens' UpdateFeaturedResultsSetResponse (Prelude.Maybe FeaturedResultsSet)
updateFeaturedResultsSetResponse_featuredResultsSet = Lens.lens (\UpdateFeaturedResultsSetResponse' {featuredResultsSet} -> featuredResultsSet) (\s@UpdateFeaturedResultsSetResponse' {} a -> s {featuredResultsSet = a} :: UpdateFeaturedResultsSetResponse)

-- | The response's http status code.
updateFeaturedResultsSetResponse_httpStatus :: Lens.Lens' UpdateFeaturedResultsSetResponse Prelude.Int
updateFeaturedResultsSetResponse_httpStatus = Lens.lens (\UpdateFeaturedResultsSetResponse' {httpStatus} -> httpStatus) (\s@UpdateFeaturedResultsSetResponse' {} a -> s {httpStatus = a} :: UpdateFeaturedResultsSetResponse)

instance
  Prelude.NFData
    UpdateFeaturedResultsSetResponse
  where
  rnf UpdateFeaturedResultsSetResponse' {..} =
    Prelude.rnf featuredResultsSet
      `Prelude.seq` Prelude.rnf httpStatus
