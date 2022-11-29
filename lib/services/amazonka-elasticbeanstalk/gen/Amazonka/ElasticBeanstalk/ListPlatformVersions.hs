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
-- Module      : Amazonka.ElasticBeanstalk.ListPlatformVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform versions available for your account in an AWS Region.
-- Provides summary information about each platform version. Compare to
-- DescribePlatformVersion, which provides full details about a single
-- platform version.
--
-- For definitions of platform version and other platform-related terms,
-- see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary>.
--
-- This operation returns paginated results.
module Amazonka.ElasticBeanstalk.ListPlatformVersions
  ( -- * Creating a Request
    ListPlatformVersions (..),
    newListPlatformVersions,

    -- * Request Lenses
    listPlatformVersions_nextToken,
    listPlatformVersions_filters,
    listPlatformVersions_maxRecords,

    -- * Destructuring the Response
    ListPlatformVersionsResponse (..),
    newListPlatformVersionsResponse,

    -- * Response Lenses
    listPlatformVersionsResponse_nextToken,
    listPlatformVersionsResponse_platformSummaryList,
    listPlatformVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPlatformVersions' smart constructor.
data ListPlatformVersions = ListPlatformVersions'
  { -- | For a paginated request. Specify a token from a previous response page
    -- to retrieve the next response page. All other parameter values must be
    -- identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Criteria for restricting the resulting list of platform versions. The
    -- filter is interpreted as a logical conjunction (AND) of the separate
    -- @PlatformFilter@ terms.
    filters :: Prelude.Maybe [PlatformFilter],
    -- | The maximum number of platform version values returned in one call.
    maxRecords :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlatformVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlatformVersions_nextToken' - For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- 'filters', 'listPlatformVersions_filters' - Criteria for restricting the resulting list of platform versions. The
-- filter is interpreted as a logical conjunction (AND) of the separate
-- @PlatformFilter@ terms.
--
-- 'maxRecords', 'listPlatformVersions_maxRecords' - The maximum number of platform version values returned in one call.
newListPlatformVersions ::
  ListPlatformVersions
newListPlatformVersions =
  ListPlatformVersions'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
listPlatformVersions_nextToken :: Lens.Lens' ListPlatformVersions (Prelude.Maybe Prelude.Text)
listPlatformVersions_nextToken = Lens.lens (\ListPlatformVersions' {nextToken} -> nextToken) (\s@ListPlatformVersions' {} a -> s {nextToken = a} :: ListPlatformVersions)

-- | Criteria for restricting the resulting list of platform versions. The
-- filter is interpreted as a logical conjunction (AND) of the separate
-- @PlatformFilter@ terms.
listPlatformVersions_filters :: Lens.Lens' ListPlatformVersions (Prelude.Maybe [PlatformFilter])
listPlatformVersions_filters = Lens.lens (\ListPlatformVersions' {filters} -> filters) (\s@ListPlatformVersions' {} a -> s {filters = a} :: ListPlatformVersions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of platform version values returned in one call.
listPlatformVersions_maxRecords :: Lens.Lens' ListPlatformVersions (Prelude.Maybe Prelude.Natural)
listPlatformVersions_maxRecords = Lens.lens (\ListPlatformVersions' {maxRecords} -> maxRecords) (\s@ListPlatformVersions' {} a -> s {maxRecords = a} :: ListPlatformVersions)

instance Core.AWSPager ListPlatformVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPlatformVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPlatformVersionsResponse_platformSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPlatformVersions_nextToken
          Lens..~ rs
          Lens.^? listPlatformVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPlatformVersions where
  type
    AWSResponse ListPlatformVersions =
      ListPlatformVersionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListPlatformVersionsResult"
      ( \s h x ->
          ListPlatformVersionsResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "PlatformSummaryList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPlatformVersions where
  hashWithSalt _salt ListPlatformVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData ListPlatformVersions where
  rnf ListPlatformVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords

instance Core.ToHeaders ListPlatformVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPlatformVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPlatformVersions where
  toQuery ListPlatformVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListPlatformVersions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> filters),
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newListPlatformVersionsResponse' smart constructor.
data ListPlatformVersionsResponse = ListPlatformVersionsResponse'
  { -- | In a paginated request, if this value isn\'t @null@, it\'s the token
    -- that you can pass in a subsequent request to get the next response page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the platform versions.
    platformSummaryList :: Prelude.Maybe [PlatformSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlatformVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlatformVersionsResponse_nextToken' - In a paginated request, if this value isn\'t @null@, it\'s the token
-- that you can pass in a subsequent request to get the next response page.
--
-- 'platformSummaryList', 'listPlatformVersionsResponse_platformSummaryList' - Summary information about the platform versions.
--
-- 'httpStatus', 'listPlatformVersionsResponse_httpStatus' - The response's http status code.
newListPlatformVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPlatformVersionsResponse
newListPlatformVersionsResponse pHttpStatus_ =
  ListPlatformVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      platformSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | In a paginated request, if this value isn\'t @null@, it\'s the token
-- that you can pass in a subsequent request to get the next response page.
listPlatformVersionsResponse_nextToken :: Lens.Lens' ListPlatformVersionsResponse (Prelude.Maybe Prelude.Text)
listPlatformVersionsResponse_nextToken = Lens.lens (\ListPlatformVersionsResponse' {nextToken} -> nextToken) (\s@ListPlatformVersionsResponse' {} a -> s {nextToken = a} :: ListPlatformVersionsResponse)

-- | Summary information about the platform versions.
listPlatformVersionsResponse_platformSummaryList :: Lens.Lens' ListPlatformVersionsResponse (Prelude.Maybe [PlatformSummary])
listPlatformVersionsResponse_platformSummaryList = Lens.lens (\ListPlatformVersionsResponse' {platformSummaryList} -> platformSummaryList) (\s@ListPlatformVersionsResponse' {} a -> s {platformSummaryList = a} :: ListPlatformVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPlatformVersionsResponse_httpStatus :: Lens.Lens' ListPlatformVersionsResponse Prelude.Int
listPlatformVersionsResponse_httpStatus = Lens.lens (\ListPlatformVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPlatformVersionsResponse' {} a -> s {httpStatus = a} :: ListPlatformVersionsResponse)

instance Prelude.NFData ListPlatformVersionsResponse where
  rnf ListPlatformVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf platformSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
