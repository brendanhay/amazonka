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
-- Module      : Network.AWS.ECS.ListTaskDefinitionFamilies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definition families that are registered to your
-- account (which may include task definition families that no longer have
-- any @ACTIVE@ task definition revisions).
--
-- You can filter out task definition families that do not contain any
-- @ACTIVE@ task definition revisions by setting the @status@ parameter to
-- @ACTIVE@. You can also filter the results with the @familyPrefix@
-- parameter.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTaskDefinitionFamilies
  ( -- * Creating a Request
    ListTaskDefinitionFamilies (..),
    newListTaskDefinitionFamilies,

    -- * Request Lenses
    listTaskDefinitionFamilies_nextToken,
    listTaskDefinitionFamilies_status,
    listTaskDefinitionFamilies_maxResults,
    listTaskDefinitionFamilies_familyPrefix,

    -- * Destructuring the Response
    ListTaskDefinitionFamiliesResponse (..),
    newListTaskDefinitionFamiliesResponse,

    -- * Response Lenses
    listTaskDefinitionFamiliesResponse_nextToken,
    listTaskDefinitionFamiliesResponse_families,
    listTaskDefinitionFamiliesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTaskDefinitionFamilies' smart constructor.
data ListTaskDefinitionFamilies = ListTaskDefinitionFamilies'
  { -- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@
    -- request indicating that more results are available to fulfill the
    -- request and further calls will be needed. If @maxResults@ was provided,
    -- it is possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The task definition family status with which to filter the
    -- @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and
    -- @INACTIVE@ task definition families are listed. If this parameter is set
    -- to @ACTIVE@, only task definition families that have an @ACTIVE@ task
    -- definition revision are returned. If this parameter is set to
    -- @INACTIVE@, only task definition families that do not have any @ACTIVE@
    -- task definition revisions are returned. If you paginate the resulting
    -- output, be sure to keep the @status@ value constant in each subsequent
    -- request.
    status :: Core.Maybe TaskDefinitionFamilyStatus,
    -- | The maximum number of task definition family results returned by
    -- @ListTaskDefinitionFamilies@ in paginated output. When this parameter is
    -- used, @ListTaskDefinitions@ only returns @maxResults@ results in a
    -- single page along with a @nextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @ListTaskDefinitionFamilies@ request with the returned @nextToken@
    -- value. This value can be between 1 and 100. If this parameter is not
    -- used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The @familyPrefix@ is a string that is used to filter the results of
    -- @ListTaskDefinitionFamilies@. If you specify a @familyPrefix@, only task
    -- definition family names that begin with the @familyPrefix@ string are
    -- returned.
    familyPrefix :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTaskDefinitionFamilies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'status', 'listTaskDefinitionFamilies_status' - The task definition family status with which to filter the
-- @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and
-- @INACTIVE@ task definition families are listed. If this parameter is set
-- to @ACTIVE@, only task definition families that have an @ACTIVE@ task
-- definition revision are returned. If this parameter is set to
-- @INACTIVE@, only task definition families that do not have any @ACTIVE@
-- task definition revisions are returned. If you paginate the resulting
-- output, be sure to keep the @status@ value constant in each subsequent
-- request.
--
-- 'maxResults', 'listTaskDefinitionFamilies_maxResults' - The maximum number of task definition family results returned by
-- @ListTaskDefinitionFamilies@ in paginated output. When this parameter is
-- used, @ListTaskDefinitions@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @ListTaskDefinitionFamilies@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter is not
-- used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a
-- @nextToken@ value if applicable.
--
-- 'familyPrefix', 'listTaskDefinitionFamilies_familyPrefix' - The @familyPrefix@ is a string that is used to filter the results of
-- @ListTaskDefinitionFamilies@. If you specify a @familyPrefix@, only task
-- definition family names that begin with the @familyPrefix@ string are
-- returned.
newListTaskDefinitionFamilies ::
  ListTaskDefinitionFamilies
newListTaskDefinitionFamilies =
  ListTaskDefinitionFamilies'
    { nextToken =
        Core.Nothing,
      status = Core.Nothing,
      maxResults = Core.Nothing,
      familyPrefix = Core.Nothing
    }

-- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@
-- request indicating that more results are available to fulfill the
-- request and further calls will be needed. If @maxResults@ was provided,
-- it is possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listTaskDefinitionFamilies_nextToken :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe Core.Text)
listTaskDefinitionFamilies_nextToken = Lens.lens (\ListTaskDefinitionFamilies' {nextToken} -> nextToken) (\s@ListTaskDefinitionFamilies' {} a -> s {nextToken = a} :: ListTaskDefinitionFamilies)

-- | The task definition family status with which to filter the
-- @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and
-- @INACTIVE@ task definition families are listed. If this parameter is set
-- to @ACTIVE@, only task definition families that have an @ACTIVE@ task
-- definition revision are returned. If this parameter is set to
-- @INACTIVE@, only task definition families that do not have any @ACTIVE@
-- task definition revisions are returned. If you paginate the resulting
-- output, be sure to keep the @status@ value constant in each subsequent
-- request.
listTaskDefinitionFamilies_status :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe TaskDefinitionFamilyStatus)
listTaskDefinitionFamilies_status = Lens.lens (\ListTaskDefinitionFamilies' {status} -> status) (\s@ListTaskDefinitionFamilies' {} a -> s {status = a} :: ListTaskDefinitionFamilies)

-- | The maximum number of task definition family results returned by
-- @ListTaskDefinitionFamilies@ in paginated output. When this parameter is
-- used, @ListTaskDefinitions@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @ListTaskDefinitionFamilies@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter is not
-- used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a
-- @nextToken@ value if applicable.
listTaskDefinitionFamilies_maxResults :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe Core.Int)
listTaskDefinitionFamilies_maxResults = Lens.lens (\ListTaskDefinitionFamilies' {maxResults} -> maxResults) (\s@ListTaskDefinitionFamilies' {} a -> s {maxResults = a} :: ListTaskDefinitionFamilies)

-- | The @familyPrefix@ is a string that is used to filter the results of
-- @ListTaskDefinitionFamilies@. If you specify a @familyPrefix@, only task
-- definition family names that begin with the @familyPrefix@ string are
-- returned.
listTaskDefinitionFamilies_familyPrefix :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe Core.Text)
listTaskDefinitionFamilies_familyPrefix = Lens.lens (\ListTaskDefinitionFamilies' {familyPrefix} -> familyPrefix) (\s@ListTaskDefinitionFamilies' {} a -> s {familyPrefix = a} :: ListTaskDefinitionFamilies)

instance Core.AWSPager ListTaskDefinitionFamilies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTaskDefinitionFamiliesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTaskDefinitionFamiliesResponse_families
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTaskDefinitionFamilies_nextToken
          Lens..~ rs
          Lens.^? listTaskDefinitionFamiliesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListTaskDefinitionFamilies where
  type
    AWSResponse ListTaskDefinitionFamilies =
      ListTaskDefinitionFamiliesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTaskDefinitionFamiliesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "families" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTaskDefinitionFamilies

instance Core.NFData ListTaskDefinitionFamilies

instance Core.ToHeaders ListTaskDefinitionFamilies where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListTaskDefinitionFamilies" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTaskDefinitionFamilies where
  toJSON ListTaskDefinitionFamilies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("familyPrefix" Core..=) Core.<$> familyPrefix
          ]
      )

instance Core.ToPath ListTaskDefinitionFamilies where
  toPath = Core.const "/"

instance Core.ToQuery ListTaskDefinitionFamilies where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTaskDefinitionFamiliesResponse' smart constructor.
data ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'
  { -- | The @nextToken@ value to include in a future
    -- @ListTaskDefinitionFamilies@ request. When the results of a
    -- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
    -- be used to retrieve the next page of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of task definition family names that match the
    -- @ListTaskDefinitionFamilies@ request.
    families :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTaskDefinitionFamiliesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTaskDefinitionFamiliesResponse_nextToken' - The @nextToken@ value to include in a future
-- @ListTaskDefinitionFamilies@ request. When the results of a
-- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
--
-- 'families', 'listTaskDefinitionFamiliesResponse_families' - The list of task definition family names that match the
-- @ListTaskDefinitionFamilies@ request.
--
-- 'httpStatus', 'listTaskDefinitionFamiliesResponse_httpStatus' - The response's http status code.
newListTaskDefinitionFamiliesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTaskDefinitionFamiliesResponse
newListTaskDefinitionFamiliesResponse pHttpStatus_ =
  ListTaskDefinitionFamiliesResponse'
    { nextToken =
        Core.Nothing,
      families = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future
-- @ListTaskDefinitionFamilies@ request. When the results of a
-- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
listTaskDefinitionFamiliesResponse_nextToken :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Core.Maybe Core.Text)
listTaskDefinitionFamiliesResponse_nextToken = Lens.lens (\ListTaskDefinitionFamiliesResponse' {nextToken} -> nextToken) (\s@ListTaskDefinitionFamiliesResponse' {} a -> s {nextToken = a} :: ListTaskDefinitionFamiliesResponse)

-- | The list of task definition family names that match the
-- @ListTaskDefinitionFamilies@ request.
listTaskDefinitionFamiliesResponse_families :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Core.Maybe [Core.Text])
listTaskDefinitionFamiliesResponse_families = Lens.lens (\ListTaskDefinitionFamiliesResponse' {families} -> families) (\s@ListTaskDefinitionFamiliesResponse' {} a -> s {families = a} :: ListTaskDefinitionFamiliesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTaskDefinitionFamiliesResponse_httpStatus :: Lens.Lens' ListTaskDefinitionFamiliesResponse Core.Int
listTaskDefinitionFamiliesResponse_httpStatus = Lens.lens (\ListTaskDefinitionFamiliesResponse' {httpStatus} -> httpStatus) (\s@ListTaskDefinitionFamiliesResponse' {} a -> s {httpStatus = a} :: ListTaskDefinitionFamiliesResponse)

instance
  Core.NFData
    ListTaskDefinitionFamiliesResponse
