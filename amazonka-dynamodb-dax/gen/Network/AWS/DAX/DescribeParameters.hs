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
-- Module      : Network.AWS.DAX.DescribeParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular parameter group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeParameters
  ( -- * Creating a Request
    DescribeParameters (..),
    newDescribeParameters,

    -- * Request Lenses
    describeParameters_nextToken,
    describeParameters_maxResults,
    describeParameters_source,
    describeParameters_parameterGroupName,

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
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Core.Maybe Core.Int,
    -- | How the parameter is defined. For example, @system@ denotes a
    -- system-defined parameter.
    source :: Core.Maybe Core.Text,
    -- | The name of the parameter group.
    parameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeParameters_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'maxResults', 'describeParameters_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'source', 'describeParameters_source' - How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
--
-- 'parameterGroupName', 'describeParameters_parameterGroupName' - The name of the parameter group.
newDescribeParameters ::
  -- | 'parameterGroupName'
  Core.Text ->
  DescribeParameters
newDescribeParameters pParameterGroupName_ =
  DescribeParameters'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      source = Core.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeParameters_nextToken :: Lens.Lens' DescribeParameters (Core.Maybe Core.Text)
describeParameters_nextToken = Lens.lens (\DescribeParameters' {nextToken} -> nextToken) (\s@DescribeParameters' {} a -> s {nextToken = a} :: DescribeParameters)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeParameters_maxResults :: Lens.Lens' DescribeParameters (Core.Maybe Core.Int)
describeParameters_maxResults = Lens.lens (\DescribeParameters' {maxResults} -> maxResults) (\s@DescribeParameters' {} a -> s {maxResults = a} :: DescribeParameters)

-- | How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
describeParameters_source :: Lens.Lens' DescribeParameters (Core.Maybe Core.Text)
describeParameters_source = Lens.lens (\DescribeParameters' {source} -> source) (\s@DescribeParameters' {} a -> s {source = a} :: DescribeParameters)

-- | The name of the parameter group.
describeParameters_parameterGroupName :: Lens.Lens' DescribeParameters Core.Text
describeParameters_parameterGroupName = Lens.lens (\DescribeParameters' {parameterGroupName} -> parameterGroupName) (\s@DescribeParameters' {} a -> s {parameterGroupName = a} :: DescribeParameters)

instance Core.AWSPager DescribeParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeParametersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeParametersResponse_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeParameters_nextToken
          Lens..~ rs
          Lens.^? describeParametersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeParameters where
  type
    AWSResponse DescribeParameters =
      DescribeParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeParametersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Parameters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeParameters

instance Core.NFData DescribeParameters

instance Core.ToHeaders DescribeParameters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.DescribeParameters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeParameters where
  toJSON DescribeParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Source" Core..=) Core.<$> source,
            Core.Just
              ("ParameterGroupName" Core..= parameterGroupName)
          ]
      )

instance Core.ToPath DescribeParameters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeParameters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeParametersResponse' smart constructor.
data DescribeParametersResponse = DescribeParametersResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of parameters within a parameter group. Each element in the list
    -- represents one parameter.
    parameters :: Core.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeParametersResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'parameters', 'describeParametersResponse_parameters' - A list of parameters within a parameter group. Each element in the list
-- represents one parameter.
--
-- 'httpStatus', 'describeParametersResponse_httpStatus' - The response's http status code.
newDescribeParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeParametersResponse
newDescribeParametersResponse pHttpStatus_ =
  DescribeParametersResponse'
    { nextToken =
        Core.Nothing,
      parameters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeParametersResponse_nextToken :: Lens.Lens' DescribeParametersResponse (Core.Maybe Core.Text)
describeParametersResponse_nextToken = Lens.lens (\DescribeParametersResponse' {nextToken} -> nextToken) (\s@DescribeParametersResponse' {} a -> s {nextToken = a} :: DescribeParametersResponse)

-- | A list of parameters within a parameter group. Each element in the list
-- represents one parameter.
describeParametersResponse_parameters :: Lens.Lens' DescribeParametersResponse (Core.Maybe [Parameter])
describeParametersResponse_parameters = Lens.lens (\DescribeParametersResponse' {parameters} -> parameters) (\s@DescribeParametersResponse' {} a -> s {parameters = a} :: DescribeParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeParametersResponse_httpStatus :: Lens.Lens' DescribeParametersResponse Core.Int
describeParametersResponse_httpStatus = Lens.lens (\DescribeParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeParametersResponse' {} a -> s {httpStatus = a} :: DescribeParametersResponse)

instance Core.NFData DescribeParametersResponse
