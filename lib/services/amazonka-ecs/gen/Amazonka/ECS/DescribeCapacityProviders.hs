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
-- Module      : Amazonka.ECS.DescribeCapacityProviders
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your capacity providers.
module Amazonka.ECS.DescribeCapacityProviders
  ( -- * Creating a Request
    DescribeCapacityProviders (..),
    newDescribeCapacityProviders,

    -- * Request Lenses
    describeCapacityProviders_nextToken,
    describeCapacityProviders_maxResults,
    describeCapacityProviders_capacityProviders,
    describeCapacityProviders_include,

    -- * Destructuring the Response
    DescribeCapacityProvidersResponse (..),
    newDescribeCapacityProvidersResponse,

    -- * Response Lenses
    describeCapacityProvidersResponse_nextToken,
    describeCapacityProvidersResponse_failures,
    describeCapacityProvidersResponse_capacityProviders,
    describeCapacityProvidersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCapacityProviders' smart constructor.
data DescribeCapacityProviders = DescribeCapacityProviders'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeCapacityProviders@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of account setting results returned by
    -- @DescribeCapacityProviders@ in paginated output. When this parameter is
    -- used, @DescribeCapacityProviders@ only returns @maxResults@ results in a
    -- single page along with a @nextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @DescribeCapacityProviders@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 10. If this parameter is not used, then
    -- @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@
    -- value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The short name or full Amazon Resource Name (ARN) of one or more
    -- capacity providers. Up to @100@ capacity providers can be described in
    -- an action.
    capacityProviders :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether or not you want to see the resource tags for the
    -- capacity provider. If @TAGS@ is specified, the tags are included in the
    -- response. If this field is omitted, tags aren\'t included in the
    -- response.
    include :: Prelude.Maybe [CapacityProviderField]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCapacityProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCapacityProviders_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeCapacityProviders@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'describeCapacityProviders_maxResults' - The maximum number of account setting results returned by
-- @DescribeCapacityProviders@ in paginated output. When this parameter is
-- used, @DescribeCapacityProviders@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @DescribeCapacityProviders@ request with the returned @nextToken@ value.
-- This value can be between 1 and 10. If this parameter is not used, then
-- @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@
-- value if applicable.
--
-- 'capacityProviders', 'describeCapacityProviders_capacityProviders' - The short name or full Amazon Resource Name (ARN) of one or more
-- capacity providers. Up to @100@ capacity providers can be described in
-- an action.
--
-- 'include', 'describeCapacityProviders_include' - Specifies whether or not you want to see the resource tags for the
-- capacity provider. If @TAGS@ is specified, the tags are included in the
-- response. If this field is omitted, tags aren\'t included in the
-- response.
newDescribeCapacityProviders ::
  DescribeCapacityProviders
newDescribeCapacityProviders =
  DescribeCapacityProviders'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      capacityProviders = Prelude.Nothing,
      include = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeCapacityProviders@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeCapacityProviders_nextToken :: Lens.Lens' DescribeCapacityProviders (Prelude.Maybe Prelude.Text)
describeCapacityProviders_nextToken = Lens.lens (\DescribeCapacityProviders' {nextToken} -> nextToken) (\s@DescribeCapacityProviders' {} a -> s {nextToken = a} :: DescribeCapacityProviders)

-- | The maximum number of account setting results returned by
-- @DescribeCapacityProviders@ in paginated output. When this parameter is
-- used, @DescribeCapacityProviders@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @DescribeCapacityProviders@ request with the returned @nextToken@ value.
-- This value can be between 1 and 10. If this parameter is not used, then
-- @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@
-- value if applicable.
describeCapacityProviders_maxResults :: Lens.Lens' DescribeCapacityProviders (Prelude.Maybe Prelude.Int)
describeCapacityProviders_maxResults = Lens.lens (\DescribeCapacityProviders' {maxResults} -> maxResults) (\s@DescribeCapacityProviders' {} a -> s {maxResults = a} :: DescribeCapacityProviders)

-- | The short name or full Amazon Resource Name (ARN) of one or more
-- capacity providers. Up to @100@ capacity providers can be described in
-- an action.
describeCapacityProviders_capacityProviders :: Lens.Lens' DescribeCapacityProviders (Prelude.Maybe [Prelude.Text])
describeCapacityProviders_capacityProviders = Lens.lens (\DescribeCapacityProviders' {capacityProviders} -> capacityProviders) (\s@DescribeCapacityProviders' {} a -> s {capacityProviders = a} :: DescribeCapacityProviders) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether or not you want to see the resource tags for the
-- capacity provider. If @TAGS@ is specified, the tags are included in the
-- response. If this field is omitted, tags aren\'t included in the
-- response.
describeCapacityProviders_include :: Lens.Lens' DescribeCapacityProviders (Prelude.Maybe [CapacityProviderField])
describeCapacityProviders_include = Lens.lens (\DescribeCapacityProviders' {include} -> include) (\s@DescribeCapacityProviders' {} a -> s {include = a} :: DescribeCapacityProviders) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeCapacityProviders where
  type
    AWSResponse DescribeCapacityProviders =
      DescribeCapacityProvidersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCapacityProvidersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "capacityProviders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCapacityProviders where
  hashWithSalt _salt DescribeCapacityProviders' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` capacityProviders
      `Prelude.hashWithSalt` include

instance Prelude.NFData DescribeCapacityProviders where
  rnf DescribeCapacityProviders' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf capacityProviders
      `Prelude.seq` Prelude.rnf include

instance Data.ToHeaders DescribeCapacityProviders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DescribeCapacityProviders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCapacityProviders where
  toJSON DescribeCapacityProviders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("capacityProviders" Data..=)
              Prelude.<$> capacityProviders,
            ("include" Data..=) Prelude.<$> include
          ]
      )

instance Data.ToPath DescribeCapacityProviders where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCapacityProviders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCapacityProvidersResponse' smart constructor.
data DescribeCapacityProvidersResponse = DescribeCapacityProvidersResponse'
  { -- | The @nextToken@ value to include in a future @DescribeCapacityProviders@
    -- request. When the results of a @DescribeCapacityProviders@ request
    -- exceed @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The list of capacity providers.
    capacityProviders :: Prelude.Maybe [CapacityProvider],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCapacityProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCapacityProvidersResponse_nextToken' - The @nextToken@ value to include in a future @DescribeCapacityProviders@
-- request. When the results of a @DescribeCapacityProviders@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'failures', 'describeCapacityProvidersResponse_failures' - Any failures associated with the call.
--
-- 'capacityProviders', 'describeCapacityProvidersResponse_capacityProviders' - The list of capacity providers.
--
-- 'httpStatus', 'describeCapacityProvidersResponse_httpStatus' - The response's http status code.
newDescribeCapacityProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCapacityProvidersResponse
newDescribeCapacityProvidersResponse pHttpStatus_ =
  DescribeCapacityProvidersResponse'
    { nextToken =
        Prelude.Nothing,
      failures = Prelude.Nothing,
      capacityProviders = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeCapacityProviders@
-- request. When the results of a @DescribeCapacityProviders@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeCapacityProvidersResponse_nextToken :: Lens.Lens' DescribeCapacityProvidersResponse (Prelude.Maybe Prelude.Text)
describeCapacityProvidersResponse_nextToken = Lens.lens (\DescribeCapacityProvidersResponse' {nextToken} -> nextToken) (\s@DescribeCapacityProvidersResponse' {} a -> s {nextToken = a} :: DescribeCapacityProvidersResponse)

-- | Any failures associated with the call.
describeCapacityProvidersResponse_failures :: Lens.Lens' DescribeCapacityProvidersResponse (Prelude.Maybe [Failure])
describeCapacityProvidersResponse_failures = Lens.lens (\DescribeCapacityProvidersResponse' {failures} -> failures) (\s@DescribeCapacityProvidersResponse' {} a -> s {failures = a} :: DescribeCapacityProvidersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of capacity providers.
describeCapacityProvidersResponse_capacityProviders :: Lens.Lens' DescribeCapacityProvidersResponse (Prelude.Maybe [CapacityProvider])
describeCapacityProvidersResponse_capacityProviders = Lens.lens (\DescribeCapacityProvidersResponse' {capacityProviders} -> capacityProviders) (\s@DescribeCapacityProvidersResponse' {} a -> s {capacityProviders = a} :: DescribeCapacityProvidersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCapacityProvidersResponse_httpStatus :: Lens.Lens' DescribeCapacityProvidersResponse Prelude.Int
describeCapacityProvidersResponse_httpStatus = Lens.lens (\DescribeCapacityProvidersResponse' {httpStatus} -> httpStatus) (\s@DescribeCapacityProvidersResponse' {} a -> s {httpStatus = a} :: DescribeCapacityProvidersResponse)

instance
  Prelude.NFData
    DescribeCapacityProvidersResponse
  where
  rnf DescribeCapacityProvidersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf failures
      `Prelude.seq` Prelude.rnf capacityProviders
      `Prelude.seq` Prelude.rnf httpStatus
