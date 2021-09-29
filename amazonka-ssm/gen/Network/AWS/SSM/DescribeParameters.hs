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
-- Module      : Network.AWS.SSM.DescribeParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter.
--
-- Request results are returned on a best-effort basis. If you specify
-- @MaxResults@ in the request, the response includes information up to the
-- limit specified. The number of items returned, however, can be between
-- zero and the value of @MaxResults@. If the service reaches an internal
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point and a @NextToken@. You can specify
-- the @NextToken@ in a subsequent call to get the next set of results.
--
-- If you change the KMS key alias for the KMS key used to encrypt a
-- parameter, then you must also update the key alias the parameter uses to
-- reference KMS. Otherwise, @DescribeParameters@ retrieves whatever the
-- original key alias was referencing.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeParameters
  ( -- * Creating a Request
    DescribeParameters (..),
    newDescribeParameters,

    -- * Request Lenses
    describeParameters_nextToken,
    describeParameters_maxResults,
    describeParameters_parameterFilters,
    describeParameters_filters,

    -- * Destructuring the Response
    DescribeParametersResponse (..),
    newDescribeParametersResponse,

    -- * Response Lenses
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters to limit the request results.
    parameterFilters :: Prelude.Maybe [ParameterStringFilter],
    -- | This data type is deprecated. Instead, use @ParameterFilters@.
    filters :: Prelude.Maybe [ParametersFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeParameters_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeParameters_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'parameterFilters', 'describeParameters_parameterFilters' - Filters to limit the request results.
--
-- 'filters', 'describeParameters_filters' - This data type is deprecated. Instead, use @ParameterFilters@.
newDescribeParameters ::
  DescribeParameters
newDescribeParameters =
  DescribeParameters'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      parameterFilters = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeParameters_nextToken :: Lens.Lens' DescribeParameters (Prelude.Maybe Prelude.Text)
describeParameters_nextToken = Lens.lens (\DescribeParameters' {nextToken} -> nextToken) (\s@DescribeParameters' {} a -> s {nextToken = a} :: DescribeParameters)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeParameters_maxResults :: Lens.Lens' DescribeParameters (Prelude.Maybe Prelude.Natural)
describeParameters_maxResults = Lens.lens (\DescribeParameters' {maxResults} -> maxResults) (\s@DescribeParameters' {} a -> s {maxResults = a} :: DescribeParameters)

-- | Filters to limit the request results.
describeParameters_parameterFilters :: Lens.Lens' DescribeParameters (Prelude.Maybe [ParameterStringFilter])
describeParameters_parameterFilters = Lens.lens (\DescribeParameters' {parameterFilters} -> parameterFilters) (\s@DescribeParameters' {} a -> s {parameterFilters = a} :: DescribeParameters) Prelude.. Lens.mapping Lens._Coerce

-- | This data type is deprecated. Instead, use @ParameterFilters@.
describeParameters_filters :: Lens.Lens' DescribeParameters (Prelude.Maybe [ParametersFilter])
describeParameters_filters = Lens.lens (\DescribeParameters' {filters} -> filters) (\s@DescribeParameters' {} a -> s {filters = a} :: DescribeParameters) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeParametersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeParametersResponse_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeParameters_nextToken
          Lens..~ rs
          Lens.^? describeParametersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeParameters where
  type
    AWSResponse DescribeParameters =
      DescribeParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeParametersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeParameters

instance Prelude.NFData DescribeParameters

instance Core.ToHeaders DescribeParameters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeParameters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeParameters where
  toJSON DescribeParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ParameterFilters" Core..=)
              Prelude.<$> parameterFilters,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath DescribeParameters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeParameters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeParametersResponse' smart constructor.
data DescribeParametersResponse = DescribeParametersResponse'
  { -- | The token to use when requesting the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Parameters returned by the request.
    parameters :: Prelude.Maybe [ParameterMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeParametersResponse_nextToken' - The token to use when requesting the next set of items.
--
-- 'parameters', 'describeParametersResponse_parameters' - Parameters returned by the request.
--
-- 'httpStatus', 'describeParametersResponse_httpStatus' - The response's http status code.
newDescribeParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeParametersResponse
newDescribeParametersResponse pHttpStatus_ =
  DescribeParametersResponse'
    { nextToken =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items.
describeParametersResponse_nextToken :: Lens.Lens' DescribeParametersResponse (Prelude.Maybe Prelude.Text)
describeParametersResponse_nextToken = Lens.lens (\DescribeParametersResponse' {nextToken} -> nextToken) (\s@DescribeParametersResponse' {} a -> s {nextToken = a} :: DescribeParametersResponse)

-- | Parameters returned by the request.
describeParametersResponse_parameters :: Lens.Lens' DescribeParametersResponse (Prelude.Maybe [ParameterMetadata])
describeParametersResponse_parameters = Lens.lens (\DescribeParametersResponse' {parameters} -> parameters) (\s@DescribeParametersResponse' {} a -> s {parameters = a} :: DescribeParametersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeParametersResponse_httpStatus :: Lens.Lens' DescribeParametersResponse Prelude.Int
describeParametersResponse_httpStatus = Lens.lens (\DescribeParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeParametersResponse' {} a -> s {httpStatus = a} :: DescribeParametersResponse)

instance Prelude.NFData DescribeParametersResponse
