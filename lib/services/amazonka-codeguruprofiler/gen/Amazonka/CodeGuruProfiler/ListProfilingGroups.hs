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
-- Module      : Amazonka.CodeGuruProfiler.ListProfilingGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of profiling groups. The profiling groups are returned as
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects.
module Amazonka.CodeGuruProfiler.ListProfilingGroups
  ( -- * Creating a Request
    ListProfilingGroups (..),
    newListProfilingGroups,

    -- * Request Lenses
    listProfilingGroups_includeDescription,
    listProfilingGroups_maxResults,
    listProfilingGroups_nextToken,

    -- * Destructuring the Response
    ListProfilingGroupsResponse (..),
    newListProfilingGroupsResponse,

    -- * Response Lenses
    listProfilingGroupsResponse_nextToken,
    listProfilingGroupsResponse_profilingGroups,
    listProfilingGroupsResponse_httpStatus,
    listProfilingGroupsResponse_profilingGroupNames,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the listProfilingGroupsRequest.
--
-- /See:/ 'newListProfilingGroups' smart constructor.
data ListProfilingGroups = ListProfilingGroups'
  { -- | A @Boolean@ value indicating whether to include a description. If
    -- @true@, then a list of
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
    -- objects that contain detailed information about profiling groups is
    -- returned. If @false@, then a list of profiling group names is returned.
    includeDescription :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of profiling groups results returned by
    -- @ListProfilingGroups@ in paginated output. When this parameter is used,
    -- @ListProfilingGroups@ only returns @maxResults@ results in a single page
    -- along with a @nextToken@ response element. The remaining results of the
    -- initial request can be seen by sending another @ListProfilingGroups@
    -- request with the returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListProfilingGroups@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfilingGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeDescription', 'listProfilingGroups_includeDescription' - A @Boolean@ value indicating whether to include a description. If
-- @true@, then a list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects that contain detailed information about profiling groups is
-- returned. If @false@, then a list of profiling group names is returned.
--
-- 'maxResults', 'listProfilingGroups_maxResults' - The maximum number of profiling groups results returned by
-- @ListProfilingGroups@ in paginated output. When this parameter is used,
-- @ListProfilingGroups@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListProfilingGroups@
-- request with the returned @nextToken@ value.
--
-- 'nextToken', 'listProfilingGroups_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListProfilingGroups@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
newListProfilingGroups ::
  ListProfilingGroups
newListProfilingGroups =
  ListProfilingGroups'
    { includeDescription =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A @Boolean@ value indicating whether to include a description. If
-- @true@, then a list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects that contain detailed information about profiling groups is
-- returned. If @false@, then a list of profiling group names is returned.
listProfilingGroups_includeDescription :: Lens.Lens' ListProfilingGroups (Prelude.Maybe Prelude.Bool)
listProfilingGroups_includeDescription = Lens.lens (\ListProfilingGroups' {includeDescription} -> includeDescription) (\s@ListProfilingGroups' {} a -> s {includeDescription = a} :: ListProfilingGroups)

-- | The maximum number of profiling groups results returned by
-- @ListProfilingGroups@ in paginated output. When this parameter is used,
-- @ListProfilingGroups@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListProfilingGroups@
-- request with the returned @nextToken@ value.
listProfilingGroups_maxResults :: Lens.Lens' ListProfilingGroups (Prelude.Maybe Prelude.Natural)
listProfilingGroups_maxResults = Lens.lens (\ListProfilingGroups' {maxResults} -> maxResults) (\s@ListProfilingGroups' {} a -> s {maxResults = a} :: ListProfilingGroups)

-- | The @nextToken@ value returned from a previous paginated
-- @ListProfilingGroups@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listProfilingGroups_nextToken :: Lens.Lens' ListProfilingGroups (Prelude.Maybe Prelude.Text)
listProfilingGroups_nextToken = Lens.lens (\ListProfilingGroups' {nextToken} -> nextToken) (\s@ListProfilingGroups' {} a -> s {nextToken = a} :: ListProfilingGroups)

instance Core.AWSRequest ListProfilingGroups where
  type
    AWSResponse ListProfilingGroups =
      ListProfilingGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfilingGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "profilingGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "profilingGroupNames"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListProfilingGroups where
  hashWithSalt _salt ListProfilingGroups' {..} =
    _salt
      `Prelude.hashWithSalt` includeDescription
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListProfilingGroups where
  rnf ListProfilingGroups' {..} =
    Prelude.rnf includeDescription
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListProfilingGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProfilingGroups where
  toPath = Prelude.const "/profilingGroups"

instance Data.ToQuery ListProfilingGroups where
  toQuery ListProfilingGroups' {..} =
    Prelude.mconcat
      [ "includeDescription" Data.=: includeDescription,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | The structure representing the listProfilingGroupsResponse.
--
-- /See:/ 'newListProfilingGroupsResponse' smart constructor.
data ListProfilingGroupsResponse = ListProfilingGroupsResponse'
  { -- | The @nextToken@ value to include in a future @ListProfilingGroups@
    -- request. When the results of a @ListProfilingGroups@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A returned list
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
    -- objects. A list of
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
    -- objects is returned only if @includeDescription@ is @true@, otherwise a
    -- list of profiling group names is returned.
    profilingGroups :: Prelude.Maybe [ProfilingGroupDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A returned list of profiling group names. A list of the names is
    -- returned only if @includeDescription@ is @false@, otherwise a list of
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
    -- objects is returned.
    profilingGroupNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfilingGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfilingGroupsResponse_nextToken' - The @nextToken@ value to include in a future @ListProfilingGroups@
-- request. When the results of a @ListProfilingGroups@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'profilingGroups', 'listProfilingGroupsResponse_profilingGroups' - A returned list
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects. A list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects is returned only if @includeDescription@ is @true@, otherwise a
-- list of profiling group names is returned.
--
-- 'httpStatus', 'listProfilingGroupsResponse_httpStatus' - The response's http status code.
--
-- 'profilingGroupNames', 'listProfilingGroupsResponse_profilingGroupNames' - A returned list of profiling group names. A list of the names is
-- returned only if @includeDescription@ is @false@, otherwise a list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects is returned.
newListProfilingGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfilingGroupsResponse
newListProfilingGroupsResponse pHttpStatus_ =
  ListProfilingGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      profilingGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      profilingGroupNames = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListProfilingGroups@
-- request. When the results of a @ListProfilingGroups@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listProfilingGroupsResponse_nextToken :: Lens.Lens' ListProfilingGroupsResponse (Prelude.Maybe Prelude.Text)
listProfilingGroupsResponse_nextToken = Lens.lens (\ListProfilingGroupsResponse' {nextToken} -> nextToken) (\s@ListProfilingGroupsResponse' {} a -> s {nextToken = a} :: ListProfilingGroupsResponse)

-- | A returned list
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects. A list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects is returned only if @includeDescription@ is @true@, otherwise a
-- list of profiling group names is returned.
listProfilingGroupsResponse_profilingGroups :: Lens.Lens' ListProfilingGroupsResponse (Prelude.Maybe [ProfilingGroupDescription])
listProfilingGroupsResponse_profilingGroups = Lens.lens (\ListProfilingGroupsResponse' {profilingGroups} -> profilingGroups) (\s@ListProfilingGroupsResponse' {} a -> s {profilingGroups = a} :: ListProfilingGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProfilingGroupsResponse_httpStatus :: Lens.Lens' ListProfilingGroupsResponse Prelude.Int
listProfilingGroupsResponse_httpStatus = Lens.lens (\ListProfilingGroupsResponse' {httpStatus} -> httpStatus) (\s@ListProfilingGroupsResponse' {} a -> s {httpStatus = a} :: ListProfilingGroupsResponse)

-- | A returned list of profiling group names. A list of the names is
-- returned only if @includeDescription@ is @false@, otherwise a list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- objects is returned.
listProfilingGroupsResponse_profilingGroupNames :: Lens.Lens' ListProfilingGroupsResponse [Prelude.Text]
listProfilingGroupsResponse_profilingGroupNames = Lens.lens (\ListProfilingGroupsResponse' {profilingGroupNames} -> profilingGroupNames) (\s@ListProfilingGroupsResponse' {} a -> s {profilingGroupNames = a} :: ListProfilingGroupsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProfilingGroupsResponse where
  rnf ListProfilingGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf profilingGroups
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profilingGroupNames
