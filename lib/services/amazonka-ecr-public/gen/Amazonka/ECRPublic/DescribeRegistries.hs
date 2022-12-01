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
-- Module      : Amazonka.ECRPublic.DescribeRegistries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details for a public registry.
--
-- This operation returns paginated results.
module Amazonka.ECRPublic.DescribeRegistries
  ( -- * Creating a Request
    DescribeRegistries (..),
    newDescribeRegistries,

    -- * Request Lenses
    describeRegistries_nextToken,
    describeRegistries_maxResults,

    -- * Destructuring the Response
    DescribeRegistriesResponse (..),
    newDescribeRegistriesResponse,

    -- * Response Lenses
    describeRegistriesResponse_nextToken,
    describeRegistriesResponse_httpStatus,
    describeRegistriesResponse_registries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRegistries' smart constructor.
data DescribeRegistries = DescribeRegistries'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeRegistries@ request where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value. This value
    -- is @null@ when there are no more results to return.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of repository results returned by
    -- @DescribeRegistries@ in paginated output. When this parameter is used,
    -- @DescribeRegistries@ only returns @maxResults@ results in a single page
    -- along with a @nextToken@ response element. The remaining results of the
    -- initial request can be seen by sending another @DescribeRegistries@
    -- request with the returned @nextToken@ value. This value can be between 1
    -- and 1000. If this parameter is not used, then @DescribeRegistries@
    -- returns up to 100 results and a @nextToken@ value, if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRegistries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRegistries_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeRegistries@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'describeRegistries_maxResults' - The maximum number of repository results returned by
-- @DescribeRegistries@ in paginated output. When this parameter is used,
-- @DescribeRegistries@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @DescribeRegistries@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 1000. If this parameter is not used, then @DescribeRegistries@
-- returns up to 100 results and a @nextToken@ value, if applicable.
newDescribeRegistries ::
  DescribeRegistries
newDescribeRegistries =
  DescribeRegistries'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeRegistries@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeRegistries_nextToken :: Lens.Lens' DescribeRegistries (Prelude.Maybe Prelude.Text)
describeRegistries_nextToken = Lens.lens (\DescribeRegistries' {nextToken} -> nextToken) (\s@DescribeRegistries' {} a -> s {nextToken = a} :: DescribeRegistries)

-- | The maximum number of repository results returned by
-- @DescribeRegistries@ in paginated output. When this parameter is used,
-- @DescribeRegistries@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @DescribeRegistries@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 1000. If this parameter is not used, then @DescribeRegistries@
-- returns up to 100 results and a @nextToken@ value, if applicable.
describeRegistries_maxResults :: Lens.Lens' DescribeRegistries (Prelude.Maybe Prelude.Natural)
describeRegistries_maxResults = Lens.lens (\DescribeRegistries' {maxResults} -> maxResults) (\s@DescribeRegistries' {} a -> s {maxResults = a} :: DescribeRegistries)

instance Core.AWSPager DescribeRegistries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRegistriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. describeRegistriesResponse_registries) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeRegistries_nextToken
          Lens..~ rs
          Lens.^? describeRegistriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeRegistries where
  type
    AWSResponse DescribeRegistries =
      DescribeRegistriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegistriesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "registries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeRegistries where
  hashWithSalt _salt DescribeRegistries' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeRegistries where
  rnf DescribeRegistries' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeRegistries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SpencerFrontendService.DescribeRegistries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeRegistries where
  toJSON DescribeRegistries' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeRegistries where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeRegistries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRegistriesResponse' smart constructor.
data DescribeRegistriesResponse = DescribeRegistriesResponse'
  { -- | The @nextToken@ value to include in a future @DescribeRepositories@
    -- request. When the results of a @DescribeRepositories@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object containing the details for a public registry.
    registries :: [Registry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRegistriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRegistriesResponse_nextToken' - The @nextToken@ value to include in a future @DescribeRepositories@
-- request. When the results of a @DescribeRepositories@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeRegistriesResponse_httpStatus' - The response's http status code.
--
-- 'registries', 'describeRegistriesResponse_registries' - An object containing the details for a public registry.
newDescribeRegistriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRegistriesResponse
newDescribeRegistriesResponse pHttpStatus_ =
  DescribeRegistriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      registries = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @DescribeRepositories@
-- request. When the results of a @DescribeRepositories@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeRegistriesResponse_nextToken :: Lens.Lens' DescribeRegistriesResponse (Prelude.Maybe Prelude.Text)
describeRegistriesResponse_nextToken = Lens.lens (\DescribeRegistriesResponse' {nextToken} -> nextToken) (\s@DescribeRegistriesResponse' {} a -> s {nextToken = a} :: DescribeRegistriesResponse)

-- | The response's http status code.
describeRegistriesResponse_httpStatus :: Lens.Lens' DescribeRegistriesResponse Prelude.Int
describeRegistriesResponse_httpStatus = Lens.lens (\DescribeRegistriesResponse' {httpStatus} -> httpStatus) (\s@DescribeRegistriesResponse' {} a -> s {httpStatus = a} :: DescribeRegistriesResponse)

-- | An object containing the details for a public registry.
describeRegistriesResponse_registries :: Lens.Lens' DescribeRegistriesResponse [Registry]
describeRegistriesResponse_registries = Lens.lens (\DescribeRegistriesResponse' {registries} -> registries) (\s@DescribeRegistriesResponse' {} a -> s {registries = a} :: DescribeRegistriesResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeRegistriesResponse where
  rnf DescribeRegistriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf registries
