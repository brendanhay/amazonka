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
-- Module      : Amazonka.SageMaker.ListModelPackages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the model packages that have been created.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelPackages
  ( -- * Creating a Request
    ListModelPackages (..),
    newListModelPackages,

    -- * Request Lenses
    listModelPackages_modelPackageGroupName,
    listModelPackages_sortOrder,
    listModelPackages_nextToken,
    listModelPackages_nameContains,
    listModelPackages_modelApprovalStatus,
    listModelPackages_creationTimeBefore,
    listModelPackages_sortBy,
    listModelPackages_maxResults,
    listModelPackages_modelPackageType,
    listModelPackages_creationTimeAfter,

    -- * Destructuring the Response
    ListModelPackagesResponse (..),
    newListModelPackagesResponse,

    -- * Response Lenses
    listModelPackagesResponse_nextToken,
    listModelPackagesResponse_httpStatus,
    listModelPackagesResponse_modelPackageSummaryList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelPackages' smart constructor.
data ListModelPackages = ListModelPackages'
  { -- | A filter that returns only model versions that belong to the specified
    -- model group.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The sort order for the results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the response to a previous @ListModelPackages@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of model
    -- packages, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the model package name. This filter returns only model
    -- packages whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only the model packages with the specified
    -- approval status.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | A filter that returns only model packages created before the specified
    -- time (timestamp).
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The parameter by which to sort the results. The default is
    -- @CreationTime@.
    sortBy :: Prelude.Maybe ModelPackageSortBy,
    -- | The maximum number of model packages to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only the model packages of the specified type.
    -- This can be one of the following values.
    --
    -- -   @UNVERSIONED@ - List only unversioined models. This is the default
    --     value if no @ModelPackageType@ is specified.
    --
    -- -   @VERSIONED@ - List only versioned models.
    --
    -- -   @BOTH@ - List both versioned and unversioned models.
    modelPackageType :: Prelude.Maybe ModelPackageType,
    -- | A filter that returns only model packages created after the specified
    -- time (timestamp).
    creationTimeAfter :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelPackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'listModelPackages_modelPackageGroupName' - A filter that returns only model versions that belong to the specified
-- model group.
--
-- 'sortOrder', 'listModelPackages_sortOrder' - The sort order for the results. The default is @Ascending@.
--
-- 'nextToken', 'listModelPackages_nextToken' - If the response to a previous @ListModelPackages@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of model
-- packages, use the token in the next request.
--
-- 'nameContains', 'listModelPackages_nameContains' - A string in the model package name. This filter returns only model
-- packages whose name contains the specified string.
--
-- 'modelApprovalStatus', 'listModelPackages_modelApprovalStatus' - A filter that returns only the model packages with the specified
-- approval status.
--
-- 'creationTimeBefore', 'listModelPackages_creationTimeBefore' - A filter that returns only model packages created before the specified
-- time (timestamp).
--
-- 'sortBy', 'listModelPackages_sortBy' - The parameter by which to sort the results. The default is
-- @CreationTime@.
--
-- 'maxResults', 'listModelPackages_maxResults' - The maximum number of model packages to return in the response.
--
-- 'modelPackageType', 'listModelPackages_modelPackageType' - A filter that returns only the model packages of the specified type.
-- This can be one of the following values.
--
-- -   @UNVERSIONED@ - List only unversioined models. This is the default
--     value if no @ModelPackageType@ is specified.
--
-- -   @VERSIONED@ - List only versioned models.
--
-- -   @BOTH@ - List both versioned and unversioned models.
--
-- 'creationTimeAfter', 'listModelPackages_creationTimeAfter' - A filter that returns only model packages created after the specified
-- time (timestamp).
newListModelPackages ::
  ListModelPackages
newListModelPackages =
  ListModelPackages'
    { modelPackageGroupName =
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      modelApprovalStatus = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelPackageType = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | A filter that returns only model versions that belong to the specified
-- model group.
listModelPackages_modelPackageGroupName :: Lens.Lens' ListModelPackages (Prelude.Maybe Prelude.Text)
listModelPackages_modelPackageGroupName = Lens.lens (\ListModelPackages' {modelPackageGroupName} -> modelPackageGroupName) (\s@ListModelPackages' {} a -> s {modelPackageGroupName = a} :: ListModelPackages)

-- | The sort order for the results. The default is @Ascending@.
listModelPackages_sortOrder :: Lens.Lens' ListModelPackages (Prelude.Maybe SortOrder)
listModelPackages_sortOrder = Lens.lens (\ListModelPackages' {sortOrder} -> sortOrder) (\s@ListModelPackages' {} a -> s {sortOrder = a} :: ListModelPackages)

-- | If the response to a previous @ListModelPackages@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of model
-- packages, use the token in the next request.
listModelPackages_nextToken :: Lens.Lens' ListModelPackages (Prelude.Maybe Prelude.Text)
listModelPackages_nextToken = Lens.lens (\ListModelPackages' {nextToken} -> nextToken) (\s@ListModelPackages' {} a -> s {nextToken = a} :: ListModelPackages)

-- | A string in the model package name. This filter returns only model
-- packages whose name contains the specified string.
listModelPackages_nameContains :: Lens.Lens' ListModelPackages (Prelude.Maybe Prelude.Text)
listModelPackages_nameContains = Lens.lens (\ListModelPackages' {nameContains} -> nameContains) (\s@ListModelPackages' {} a -> s {nameContains = a} :: ListModelPackages)

-- | A filter that returns only the model packages with the specified
-- approval status.
listModelPackages_modelApprovalStatus :: Lens.Lens' ListModelPackages (Prelude.Maybe ModelApprovalStatus)
listModelPackages_modelApprovalStatus = Lens.lens (\ListModelPackages' {modelApprovalStatus} -> modelApprovalStatus) (\s@ListModelPackages' {} a -> s {modelApprovalStatus = a} :: ListModelPackages)

-- | A filter that returns only model packages created before the specified
-- time (timestamp).
listModelPackages_creationTimeBefore :: Lens.Lens' ListModelPackages (Prelude.Maybe Prelude.UTCTime)
listModelPackages_creationTimeBefore = Lens.lens (\ListModelPackages' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelPackages' {} a -> s {creationTimeBefore = a} :: ListModelPackages) Prelude.. Lens.mapping Data._Time

-- | The parameter by which to sort the results. The default is
-- @CreationTime@.
listModelPackages_sortBy :: Lens.Lens' ListModelPackages (Prelude.Maybe ModelPackageSortBy)
listModelPackages_sortBy = Lens.lens (\ListModelPackages' {sortBy} -> sortBy) (\s@ListModelPackages' {} a -> s {sortBy = a} :: ListModelPackages)

-- | The maximum number of model packages to return in the response.
listModelPackages_maxResults :: Lens.Lens' ListModelPackages (Prelude.Maybe Prelude.Natural)
listModelPackages_maxResults = Lens.lens (\ListModelPackages' {maxResults} -> maxResults) (\s@ListModelPackages' {} a -> s {maxResults = a} :: ListModelPackages)

-- | A filter that returns only the model packages of the specified type.
-- This can be one of the following values.
--
-- -   @UNVERSIONED@ - List only unversioined models. This is the default
--     value if no @ModelPackageType@ is specified.
--
-- -   @VERSIONED@ - List only versioned models.
--
-- -   @BOTH@ - List both versioned and unversioned models.
listModelPackages_modelPackageType :: Lens.Lens' ListModelPackages (Prelude.Maybe ModelPackageType)
listModelPackages_modelPackageType = Lens.lens (\ListModelPackages' {modelPackageType} -> modelPackageType) (\s@ListModelPackages' {} a -> s {modelPackageType = a} :: ListModelPackages)

-- | A filter that returns only model packages created after the specified
-- time (timestamp).
listModelPackages_creationTimeAfter :: Lens.Lens' ListModelPackages (Prelude.Maybe Prelude.UTCTime)
listModelPackages_creationTimeAfter = Lens.lens (\ListModelPackages' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelPackages' {} a -> s {creationTimeAfter = a} :: ListModelPackages) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager ListModelPackages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelPackagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelPackagesResponse_modelPackageSummaryList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listModelPackages_nextToken
          Lens..~ rs
          Lens.^? listModelPackagesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListModelPackages where
  type
    AWSResponse ListModelPackages =
      ListModelPackagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelPackagesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "ModelPackageSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListModelPackages where
  hashWithSalt _salt ListModelPackages' {..} =
    _salt `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` modelApprovalStatus
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelPackageType
      `Prelude.hashWithSalt` creationTimeAfter

instance Prelude.NFData ListModelPackages where
  rnf ListModelPackages' {..} =
    Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelPackageType
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance Data.ToHeaders ListModelPackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListModelPackages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelPackages where
  toJSON ListModelPackages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ModelPackageGroupName" Data..=)
              Prelude.<$> modelPackageGroupName,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("ModelApprovalStatus" Data..=)
              Prelude.<$> modelApprovalStatus,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModelPackageType" Data..=)
              Prelude.<$> modelPackageType,
            ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Data.ToPath ListModelPackages where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelPackages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelPackagesResponse' smart constructor.
data ListModelPackagesResponse = ListModelPackagesResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of model packages, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @ModelPackageSummary@ objects, each of which lists a model
    -- package.
    modelPackageSummaryList :: [ModelPackageSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelPackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelPackagesResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model packages, use it in the subsequent request.
--
-- 'httpStatus', 'listModelPackagesResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageSummaryList', 'listModelPackagesResponse_modelPackageSummaryList' - An array of @ModelPackageSummary@ objects, each of which lists a model
-- package.
newListModelPackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelPackagesResponse
newListModelPackagesResponse pHttpStatus_ =
  ListModelPackagesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      modelPackageSummaryList = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model packages, use it in the subsequent request.
listModelPackagesResponse_nextToken :: Lens.Lens' ListModelPackagesResponse (Prelude.Maybe Prelude.Text)
listModelPackagesResponse_nextToken = Lens.lens (\ListModelPackagesResponse' {nextToken} -> nextToken) (\s@ListModelPackagesResponse' {} a -> s {nextToken = a} :: ListModelPackagesResponse)

-- | The response's http status code.
listModelPackagesResponse_httpStatus :: Lens.Lens' ListModelPackagesResponse Prelude.Int
listModelPackagesResponse_httpStatus = Lens.lens (\ListModelPackagesResponse' {httpStatus} -> httpStatus) (\s@ListModelPackagesResponse' {} a -> s {httpStatus = a} :: ListModelPackagesResponse)

-- | An array of @ModelPackageSummary@ objects, each of which lists a model
-- package.
listModelPackagesResponse_modelPackageSummaryList :: Lens.Lens' ListModelPackagesResponse [ModelPackageSummary]
listModelPackagesResponse_modelPackageSummaryList = Lens.lens (\ListModelPackagesResponse' {modelPackageSummaryList} -> modelPackageSummaryList) (\s@ListModelPackagesResponse' {} a -> s {modelPackageSummaryList = a} :: ListModelPackagesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListModelPackagesResponse where
  rnf ListModelPackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelPackageSummaryList
