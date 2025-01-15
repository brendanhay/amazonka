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
-- Module      : Amazonka.ECS.ListTaskDefinitionFamilies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definition families that are registered to your
-- account. This list includes task definition families that no longer have
-- any @ACTIVE@ task definition revisions.
--
-- You can filter out task definition families that don\'t contain any
-- @ACTIVE@ task definition revisions by setting the @status@ parameter to
-- @ACTIVE@. You can also filter the results with the @familyPrefix@
-- parameter.
--
-- This operation returns paginated results.
module Amazonka.ECS.ListTaskDefinitionFamilies
  ( -- * Creating a Request
    ListTaskDefinitionFamilies (..),
    newListTaskDefinitionFamilies,

    -- * Request Lenses
    listTaskDefinitionFamilies_familyPrefix,
    listTaskDefinitionFamilies_maxResults,
    listTaskDefinitionFamilies_nextToken,
    listTaskDefinitionFamilies_status,

    -- * Destructuring the Response
    ListTaskDefinitionFamiliesResponse (..),
    newListTaskDefinitionFamiliesResponse,

    -- * Response Lenses
    listTaskDefinitionFamiliesResponse_families,
    listTaskDefinitionFamiliesResponse_nextToken,
    listTaskDefinitionFamiliesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTaskDefinitionFamilies' smart constructor.
data ListTaskDefinitionFamilies = ListTaskDefinitionFamilies'
  { -- | The @familyPrefix@ is a string that\'s used to filter the results of
    -- @ListTaskDefinitionFamilies@. If you specify a @familyPrefix@, only task
    -- definition family names that begin with the @familyPrefix@ string are
    -- returned.
    familyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of task definition family results that
    -- @ListTaskDefinitionFamilies@ returned in paginated output. When this
    -- parameter is used, @ListTaskDefinitions@ only returns @maxResults@
    -- results in a single page along with a @nextToken@ response element. The
    -- remaining results of the initial request can be seen by sending another
    -- @ListTaskDefinitionFamilies@ request with the returned @nextToken@
    -- value. This value can be between 1 and 100. If this parameter isn\'t
    -- used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@
    -- request indicating that more results are available to fulfill the
    -- request and further calls will be needed. If @maxResults@ was provided,
    -- it is possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The task definition family status to filter the
    -- @ListTaskDefinitionFamilies@ results with. By default, both @ACTIVE@ and
    -- @INACTIVE@ task definition families are listed. If this parameter is set
    -- to @ACTIVE@, only task definition families that have an @ACTIVE@ task
    -- definition revision are returned. If this parameter is set to
    -- @INACTIVE@, only task definition families that do not have any @ACTIVE@
    -- task definition revisions are returned. If you paginate the resulting
    -- output, be sure to keep the @status@ value constant in each subsequent
    -- request.
    status :: Prelude.Maybe TaskDefinitionFamilyStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskDefinitionFamilies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'familyPrefix', 'listTaskDefinitionFamilies_familyPrefix' - The @familyPrefix@ is a string that\'s used to filter the results of
-- @ListTaskDefinitionFamilies@. If you specify a @familyPrefix@, only task
-- definition family names that begin with the @familyPrefix@ string are
-- returned.
--
-- 'maxResults', 'listTaskDefinitionFamilies_maxResults' - The maximum number of task definition family results that
-- @ListTaskDefinitionFamilies@ returned in paginated output. When this
-- parameter is used, @ListTaskDefinitions@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListTaskDefinitionFamilies@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter isn\'t
-- used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a
-- @nextToken@ value if applicable.
--
-- 'nextToken', 'listTaskDefinitionFamilies_nextToken' - The @nextToken@ value returned from a @ListTaskDefinitionFamilies@
-- request indicating that more results are available to fulfill the
-- request and further calls will be needed. If @maxResults@ was provided,
-- it is possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'status', 'listTaskDefinitionFamilies_status' - The task definition family status to filter the
-- @ListTaskDefinitionFamilies@ results with. By default, both @ACTIVE@ and
-- @INACTIVE@ task definition families are listed. If this parameter is set
-- to @ACTIVE@, only task definition families that have an @ACTIVE@ task
-- definition revision are returned. If this parameter is set to
-- @INACTIVE@, only task definition families that do not have any @ACTIVE@
-- task definition revisions are returned. If you paginate the resulting
-- output, be sure to keep the @status@ value constant in each subsequent
-- request.
newListTaskDefinitionFamilies ::
  ListTaskDefinitionFamilies
newListTaskDefinitionFamilies =
  ListTaskDefinitionFamilies'
    { familyPrefix =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The @familyPrefix@ is a string that\'s used to filter the results of
-- @ListTaskDefinitionFamilies@. If you specify a @familyPrefix@, only task
-- definition family names that begin with the @familyPrefix@ string are
-- returned.
listTaskDefinitionFamilies_familyPrefix :: Lens.Lens' ListTaskDefinitionFamilies (Prelude.Maybe Prelude.Text)
listTaskDefinitionFamilies_familyPrefix = Lens.lens (\ListTaskDefinitionFamilies' {familyPrefix} -> familyPrefix) (\s@ListTaskDefinitionFamilies' {} a -> s {familyPrefix = a} :: ListTaskDefinitionFamilies)

-- | The maximum number of task definition family results that
-- @ListTaskDefinitionFamilies@ returned in paginated output. When this
-- parameter is used, @ListTaskDefinitions@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListTaskDefinitionFamilies@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter isn\'t
-- used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a
-- @nextToken@ value if applicable.
listTaskDefinitionFamilies_maxResults :: Lens.Lens' ListTaskDefinitionFamilies (Prelude.Maybe Prelude.Int)
listTaskDefinitionFamilies_maxResults = Lens.lens (\ListTaskDefinitionFamilies' {maxResults} -> maxResults) (\s@ListTaskDefinitionFamilies' {} a -> s {maxResults = a} :: ListTaskDefinitionFamilies)

-- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@
-- request indicating that more results are available to fulfill the
-- request and further calls will be needed. If @maxResults@ was provided,
-- it is possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listTaskDefinitionFamilies_nextToken :: Lens.Lens' ListTaskDefinitionFamilies (Prelude.Maybe Prelude.Text)
listTaskDefinitionFamilies_nextToken = Lens.lens (\ListTaskDefinitionFamilies' {nextToken} -> nextToken) (\s@ListTaskDefinitionFamilies' {} a -> s {nextToken = a} :: ListTaskDefinitionFamilies)

-- | The task definition family status to filter the
-- @ListTaskDefinitionFamilies@ results with. By default, both @ACTIVE@ and
-- @INACTIVE@ task definition families are listed. If this parameter is set
-- to @ACTIVE@, only task definition families that have an @ACTIVE@ task
-- definition revision are returned. If this parameter is set to
-- @INACTIVE@, only task definition families that do not have any @ACTIVE@
-- task definition revisions are returned. If you paginate the resulting
-- output, be sure to keep the @status@ value constant in each subsequent
-- request.
listTaskDefinitionFamilies_status :: Lens.Lens' ListTaskDefinitionFamilies (Prelude.Maybe TaskDefinitionFamilyStatus)
listTaskDefinitionFamilies_status = Lens.lens (\ListTaskDefinitionFamilies' {status} -> status) (\s@ListTaskDefinitionFamilies' {} a -> s {status = a} :: ListTaskDefinitionFamilies)

instance Core.AWSPager ListTaskDefinitionFamilies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTaskDefinitionFamiliesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTaskDefinitionFamiliesResponse_families
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listTaskDefinitionFamilies_nextToken
              Lens..~ rs
              Lens.^? listTaskDefinitionFamiliesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListTaskDefinitionFamilies where
  type
    AWSResponse ListTaskDefinitionFamilies =
      ListTaskDefinitionFamiliesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTaskDefinitionFamiliesResponse'
            Prelude.<$> (x Data..?> "families" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTaskDefinitionFamilies where
  hashWithSalt _salt ListTaskDefinitionFamilies' {..} =
    _salt
      `Prelude.hashWithSalt` familyPrefix
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListTaskDefinitionFamilies where
  rnf ListTaskDefinitionFamilies' {..} =
    Prelude.rnf familyPrefix `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf status

instance Data.ToHeaders ListTaskDefinitionFamilies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.ListTaskDefinitionFamilies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTaskDefinitionFamilies where
  toJSON ListTaskDefinitionFamilies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("familyPrefix" Data..=) Prelude.<$> familyPrefix,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath ListTaskDefinitionFamilies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTaskDefinitionFamilies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTaskDefinitionFamiliesResponse' smart constructor.
data ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'
  { -- | The list of task definition family names that match the
    -- @ListTaskDefinitionFamilies@ request.
    families :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ value to include in a future
    -- @ListTaskDefinitionFamilies@ request. When the results of a
    -- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
    -- be used to retrieve the next page of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskDefinitionFamiliesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'families', 'listTaskDefinitionFamiliesResponse_families' - The list of task definition family names that match the
-- @ListTaskDefinitionFamilies@ request.
--
-- 'nextToken', 'listTaskDefinitionFamiliesResponse_nextToken' - The @nextToken@ value to include in a future
-- @ListTaskDefinitionFamilies@ request. When the results of a
-- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
--
-- 'httpStatus', 'listTaskDefinitionFamiliesResponse_httpStatus' - The response's http status code.
newListTaskDefinitionFamiliesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTaskDefinitionFamiliesResponse
newListTaskDefinitionFamiliesResponse pHttpStatus_ =
  ListTaskDefinitionFamiliesResponse'
    { families =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of task definition family names that match the
-- @ListTaskDefinitionFamilies@ request.
listTaskDefinitionFamiliesResponse_families :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Prelude.Maybe [Prelude.Text])
listTaskDefinitionFamiliesResponse_families = Lens.lens (\ListTaskDefinitionFamiliesResponse' {families} -> families) (\s@ListTaskDefinitionFamiliesResponse' {} a -> s {families = a} :: ListTaskDefinitionFamiliesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value to include in a future
-- @ListTaskDefinitionFamilies@ request. When the results of a
-- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
listTaskDefinitionFamiliesResponse_nextToken :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Prelude.Maybe Prelude.Text)
listTaskDefinitionFamiliesResponse_nextToken = Lens.lens (\ListTaskDefinitionFamiliesResponse' {nextToken} -> nextToken) (\s@ListTaskDefinitionFamiliesResponse' {} a -> s {nextToken = a} :: ListTaskDefinitionFamiliesResponse)

-- | The response's http status code.
listTaskDefinitionFamiliesResponse_httpStatus :: Lens.Lens' ListTaskDefinitionFamiliesResponse Prelude.Int
listTaskDefinitionFamiliesResponse_httpStatus = Lens.lens (\ListTaskDefinitionFamiliesResponse' {httpStatus} -> httpStatus) (\s@ListTaskDefinitionFamiliesResponse' {} a -> s {httpStatus = a} :: ListTaskDefinitionFamiliesResponse)

instance
  Prelude.NFData
    ListTaskDefinitionFamiliesResponse
  where
  rnf ListTaskDefinitionFamiliesResponse' {..} =
    Prelude.rnf families `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
