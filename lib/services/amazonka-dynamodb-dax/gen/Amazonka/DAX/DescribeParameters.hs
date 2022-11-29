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
-- Module      : Amazonka.DAX.DescribeParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular parameter group.
--
-- This operation returns paginated results.
module Amazonka.DAX.DescribeParameters
  ( -- * Creating a Request
    DescribeParameters (..),
    newDescribeParameters,

    -- * Request Lenses
    describeParameters_nextToken,
    describeParameters_source,
    describeParameters_maxResults,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | How the parameter is defined. For example, @system@ denotes a
    -- system-defined parameter.
    source :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the parameter group.
    parameterGroupName :: Prelude.Text
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
-- 'nextToken', 'describeParameters_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'source', 'describeParameters_source' - How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
--
-- 'maxResults', 'describeParameters_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'parameterGroupName', 'describeParameters_parameterGroupName' - The name of the parameter group.
newDescribeParameters ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  DescribeParameters
newDescribeParameters pParameterGroupName_ =
  DescribeParameters'
    { nextToken = Prelude.Nothing,
      source = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeParameters_nextToken :: Lens.Lens' DescribeParameters (Prelude.Maybe Prelude.Text)
describeParameters_nextToken = Lens.lens (\DescribeParameters' {nextToken} -> nextToken) (\s@DescribeParameters' {} a -> s {nextToken = a} :: DescribeParameters)

-- | How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
describeParameters_source :: Lens.Lens' DescribeParameters (Prelude.Maybe Prelude.Text)
describeParameters_source = Lens.lens (\DescribeParameters' {source} -> source) (\s@DescribeParameters' {} a -> s {source = a} :: DescribeParameters)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeParameters_maxResults :: Lens.Lens' DescribeParameters (Prelude.Maybe Prelude.Int)
describeParameters_maxResults = Lens.lens (\DescribeParameters' {maxResults} -> maxResults) (\s@DescribeParameters' {} a -> s {maxResults = a} :: DescribeParameters)

-- | The name of the parameter group.
describeParameters_parameterGroupName :: Lens.Lens' DescribeParameters Prelude.Text
describeParameters_parameterGroupName = Lens.lens (\DescribeParameters' {parameterGroupName} -> parameterGroupName) (\s@DescribeParameters' {} a -> s {parameterGroupName = a} :: DescribeParameters)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeParametersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeParameters where
  hashWithSalt _salt DescribeParameters' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData DescribeParameters where
  rnf DescribeParameters' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf parameterGroupName

instance Core.ToHeaders DescribeParameters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.DescribeParameters" ::
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
            ("Source" Core..=) Prelude.<$> source,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ParameterGroupName" Core..= parameterGroupName)
          ]
      )

instance Core.ToPath DescribeParameters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeParameters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeParametersResponse' smart constructor.
data DescribeParametersResponse = DescribeParametersResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of parameters within a parameter group. Each element in the list
    -- represents one parameter.
    parameters :: Prelude.Maybe [Parameter],
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
-- 'nextToken', 'describeParametersResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'parameters', 'describeParametersResponse_parameters' - A list of parameters within a parameter group. Each element in the list
-- represents one parameter.
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

-- | Provides an identifier to allow retrieval of paginated results.
describeParametersResponse_nextToken :: Lens.Lens' DescribeParametersResponse (Prelude.Maybe Prelude.Text)
describeParametersResponse_nextToken = Lens.lens (\DescribeParametersResponse' {nextToken} -> nextToken) (\s@DescribeParametersResponse' {} a -> s {nextToken = a} :: DescribeParametersResponse)

-- | A list of parameters within a parameter group. Each element in the list
-- represents one parameter.
describeParametersResponse_parameters :: Lens.Lens' DescribeParametersResponse (Prelude.Maybe [Parameter])
describeParametersResponse_parameters = Lens.lens (\DescribeParametersResponse' {parameters} -> parameters) (\s@DescribeParametersResponse' {} a -> s {parameters = a} :: DescribeParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeParametersResponse_httpStatus :: Lens.Lens' DescribeParametersResponse Prelude.Int
describeParametersResponse_httpStatus = Lens.lens (\DescribeParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeParametersResponse' {} a -> s {httpStatus = a} :: DescribeParametersResponse)

instance Prelude.NFData DescribeParametersResponse where
  rnf DescribeParametersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
