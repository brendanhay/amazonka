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
-- Module      : Amazonka.Kendra.CreateFeaturedResultsSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of featured results to display at the top of the search
-- results page. Featured results are placed above all other results for
-- certain queries. You map specific queries to specific documents for
-- featuring in the results. If a query contains an exact match, then one
-- or more specific documents are featured in the search results.
--
-- You can create up to 50 sets of featured results per index. You can
-- request to increase this limit by contacting
-- <http://aws.amazon.com/contact-us/ Support>.
module Amazonka.Kendra.CreateFeaturedResultsSet
  ( -- * Creating a Request
    CreateFeaturedResultsSet (..),
    newCreateFeaturedResultsSet,

    -- * Request Lenses
    createFeaturedResultsSet_clientToken,
    createFeaturedResultsSet_description,
    createFeaturedResultsSet_featuredDocuments,
    createFeaturedResultsSet_queryTexts,
    createFeaturedResultsSet_status,
    createFeaturedResultsSet_tags,
    createFeaturedResultsSet_indexId,
    createFeaturedResultsSet_featuredResultsSetName,

    -- * Destructuring the Response
    CreateFeaturedResultsSetResponse (..),
    newCreateFeaturedResultsSetResponse,

    -- * Response Lenses
    createFeaturedResultsSetResponse_featuredResultsSet,
    createFeaturedResultsSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFeaturedResultsSet' smart constructor.
data CreateFeaturedResultsSet = CreateFeaturedResultsSet'
  { -- | A token that you provide to identify the request to create a set of
    -- featured results. Multiple calls to the @CreateFeaturedResultsSet@ API
    -- with the same client token will create only one featured results set.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the set of featured results.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of document IDs for the documents you want to feature at the top
    -- of the search results page. For more information on the list of
    -- documents, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
    featuredDocuments :: Prelude.Maybe [FeaturedDocument],
    -- | A list of queries for featuring results. For more information on the
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
    -- | A list of key-value pairs that identify or categorize the featured
    -- results set. You can also use tags to help control access to the
    -- featured results set. Tag keys and values can consist of Unicode
    -- letters, digits, white space, and any of the following symbols:_ . : \/
    -- = + - \@.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the index that you want to use for featuring results.
    indexId :: Prelude.Text,
    -- | A name for the set of featured results.
    featuredResultsSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFeaturedResultsSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createFeaturedResultsSet_clientToken' - A token that you provide to identify the request to create a set of
-- featured results. Multiple calls to the @CreateFeaturedResultsSet@ API
-- with the same client token will create only one featured results set.
--
-- 'description', 'createFeaturedResultsSet_description' - A description for the set of featured results.
--
-- 'featuredDocuments', 'createFeaturedResultsSet_featuredDocuments' - A list of document IDs for the documents you want to feature at the top
-- of the search results page. For more information on the list of
-- documents, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
--
-- 'queryTexts', 'createFeaturedResultsSet_queryTexts' - A list of queries for featuring results. For more information on the
-- list of queries, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
--
-- 'status', 'createFeaturedResultsSet_status' - The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
--
-- 'tags', 'createFeaturedResultsSet_tags' - A list of key-value pairs that identify or categorize the featured
-- results set. You can also use tags to help control access to the
-- featured results set. Tag keys and values can consist of Unicode
-- letters, digits, white space, and any of the following symbols:_ . : \/
-- = + - \@.
--
-- 'indexId', 'createFeaturedResultsSet_indexId' - The identifier of the index that you want to use for featuring results.
--
-- 'featuredResultsSetName', 'createFeaturedResultsSet_featuredResultsSetName' - A name for the set of featured results.
newCreateFeaturedResultsSet ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'featuredResultsSetName'
  Prelude.Text ->
  CreateFeaturedResultsSet
newCreateFeaturedResultsSet
  pIndexId_
  pFeaturedResultsSetName_ =
    CreateFeaturedResultsSet'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        featuredDocuments = Prelude.Nothing,
        queryTexts = Prelude.Nothing,
        status = Prelude.Nothing,
        tags = Prelude.Nothing,
        indexId = pIndexId_,
        featuredResultsSetName = pFeaturedResultsSetName_
      }

-- | A token that you provide to identify the request to create a set of
-- featured results. Multiple calls to the @CreateFeaturedResultsSet@ API
-- with the same client token will create only one featured results set.
createFeaturedResultsSet_clientToken :: Lens.Lens' CreateFeaturedResultsSet (Prelude.Maybe Prelude.Text)
createFeaturedResultsSet_clientToken = Lens.lens (\CreateFeaturedResultsSet' {clientToken} -> clientToken) (\s@CreateFeaturedResultsSet' {} a -> s {clientToken = a} :: CreateFeaturedResultsSet)

-- | A description for the set of featured results.
createFeaturedResultsSet_description :: Lens.Lens' CreateFeaturedResultsSet (Prelude.Maybe Prelude.Text)
createFeaturedResultsSet_description = Lens.lens (\CreateFeaturedResultsSet' {description} -> description) (\s@CreateFeaturedResultsSet' {} a -> s {description = a} :: CreateFeaturedResultsSet)

-- | A list of document IDs for the documents you want to feature at the top
-- of the search results page. For more information on the list of
-- documents, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
createFeaturedResultsSet_featuredDocuments :: Lens.Lens' CreateFeaturedResultsSet (Prelude.Maybe [FeaturedDocument])
createFeaturedResultsSet_featuredDocuments = Lens.lens (\CreateFeaturedResultsSet' {featuredDocuments} -> featuredDocuments) (\s@CreateFeaturedResultsSet' {} a -> s {featuredDocuments = a} :: CreateFeaturedResultsSet) Prelude.. Lens.mapping Lens.coerced

-- | A list of queries for featuring results. For more information on the
-- list of queries, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_FeaturedResultsSet.html FeaturedResultsSet>.
createFeaturedResultsSet_queryTexts :: Lens.Lens' CreateFeaturedResultsSet (Prelude.Maybe [Prelude.Text])
createFeaturedResultsSet_queryTexts = Lens.lens (\CreateFeaturedResultsSet' {queryTexts} -> queryTexts) (\s@CreateFeaturedResultsSet' {} a -> s {queryTexts = a} :: CreateFeaturedResultsSet) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
createFeaturedResultsSet_status :: Lens.Lens' CreateFeaturedResultsSet (Prelude.Maybe FeaturedResultsSetStatus)
createFeaturedResultsSet_status = Lens.lens (\CreateFeaturedResultsSet' {status} -> status) (\s@CreateFeaturedResultsSet' {} a -> s {status = a} :: CreateFeaturedResultsSet)

-- | A list of key-value pairs that identify or categorize the featured
-- results set. You can also use tags to help control access to the
-- featured results set. Tag keys and values can consist of Unicode
-- letters, digits, white space, and any of the following symbols:_ . : \/
-- = + - \@.
createFeaturedResultsSet_tags :: Lens.Lens' CreateFeaturedResultsSet (Prelude.Maybe [Tag])
createFeaturedResultsSet_tags = Lens.lens (\CreateFeaturedResultsSet' {tags} -> tags) (\s@CreateFeaturedResultsSet' {} a -> s {tags = a} :: CreateFeaturedResultsSet) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the index that you want to use for featuring results.
createFeaturedResultsSet_indexId :: Lens.Lens' CreateFeaturedResultsSet Prelude.Text
createFeaturedResultsSet_indexId = Lens.lens (\CreateFeaturedResultsSet' {indexId} -> indexId) (\s@CreateFeaturedResultsSet' {} a -> s {indexId = a} :: CreateFeaturedResultsSet)

-- | A name for the set of featured results.
createFeaturedResultsSet_featuredResultsSetName :: Lens.Lens' CreateFeaturedResultsSet Prelude.Text
createFeaturedResultsSet_featuredResultsSetName = Lens.lens (\CreateFeaturedResultsSet' {featuredResultsSetName} -> featuredResultsSetName) (\s@CreateFeaturedResultsSet' {} a -> s {featuredResultsSetName = a} :: CreateFeaturedResultsSet)

instance Core.AWSRequest CreateFeaturedResultsSet where
  type
    AWSResponse CreateFeaturedResultsSet =
      CreateFeaturedResultsSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFeaturedResultsSetResponse'
            Prelude.<$> (x Data..?> "FeaturedResultsSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFeaturedResultsSet where
  hashWithSalt _salt CreateFeaturedResultsSet' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` featuredDocuments
      `Prelude.hashWithSalt` queryTexts
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` featuredResultsSetName

instance Prelude.NFData CreateFeaturedResultsSet where
  rnf CreateFeaturedResultsSet' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf featuredDocuments
      `Prelude.seq` Prelude.rnf queryTexts
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf featuredResultsSetName

instance Data.ToHeaders CreateFeaturedResultsSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.CreateFeaturedResultsSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFeaturedResultsSet where
  toJSON CreateFeaturedResultsSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("FeaturedDocuments" Data..=)
              Prelude.<$> featuredDocuments,
            ("QueryTexts" Data..=) Prelude.<$> queryTexts,
            ("Status" Data..=) Prelude.<$> status,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just
              ( "FeaturedResultsSetName"
                  Data..= featuredResultsSetName
              )
          ]
      )

instance Data.ToPath CreateFeaturedResultsSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFeaturedResultsSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFeaturedResultsSetResponse' smart constructor.
data CreateFeaturedResultsSetResponse = CreateFeaturedResultsSetResponse'
  { -- | Information on the set of featured results. This includes the identifier
    -- of the featured results set, whether the featured results set is active
    -- or inactive, when the featured results set was created, and more.
    featuredResultsSet :: Prelude.Maybe FeaturedResultsSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFeaturedResultsSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featuredResultsSet', 'createFeaturedResultsSetResponse_featuredResultsSet' - Information on the set of featured results. This includes the identifier
-- of the featured results set, whether the featured results set is active
-- or inactive, when the featured results set was created, and more.
--
-- 'httpStatus', 'createFeaturedResultsSetResponse_httpStatus' - The response's http status code.
newCreateFeaturedResultsSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFeaturedResultsSetResponse
newCreateFeaturedResultsSetResponse pHttpStatus_ =
  CreateFeaturedResultsSetResponse'
    { featuredResultsSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the set of featured results. This includes the identifier
-- of the featured results set, whether the featured results set is active
-- or inactive, when the featured results set was created, and more.
createFeaturedResultsSetResponse_featuredResultsSet :: Lens.Lens' CreateFeaturedResultsSetResponse (Prelude.Maybe FeaturedResultsSet)
createFeaturedResultsSetResponse_featuredResultsSet = Lens.lens (\CreateFeaturedResultsSetResponse' {featuredResultsSet} -> featuredResultsSet) (\s@CreateFeaturedResultsSetResponse' {} a -> s {featuredResultsSet = a} :: CreateFeaturedResultsSetResponse)

-- | The response's http status code.
createFeaturedResultsSetResponse_httpStatus :: Lens.Lens' CreateFeaturedResultsSetResponse Prelude.Int
createFeaturedResultsSetResponse_httpStatus = Lens.lens (\CreateFeaturedResultsSetResponse' {httpStatus} -> httpStatus) (\s@CreateFeaturedResultsSetResponse' {} a -> s {httpStatus = a} :: CreateFeaturedResultsSetResponse)

instance
  Prelude.NFData
    CreateFeaturedResultsSetResponse
  where
  rnf CreateFeaturedResultsSetResponse' {..} =
    Prelude.rnf featuredResultsSet
      `Prelude.seq` Prelude.rnf httpStatus
