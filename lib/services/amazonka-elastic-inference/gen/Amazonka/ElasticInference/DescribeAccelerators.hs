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
-- Module      : Amazonka.ElasticInference.DescribeAccelerators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes information over a provided set of accelerators belonging to
-- an account.
--
-- This operation returns paginated results.
module Amazonka.ElasticInference.DescribeAccelerators
  ( -- * Creating a Request
    DescribeAccelerators (..),
    newDescribeAccelerators,

    -- * Request Lenses
    describeAccelerators_acceleratorIds,
    describeAccelerators_filters,
    describeAccelerators_maxResults,
    describeAccelerators_nextToken,

    -- * Destructuring the Response
    DescribeAcceleratorsResponse (..),
    newDescribeAcceleratorsResponse,

    -- * Response Lenses
    describeAcceleratorsResponse_acceleratorSet,
    describeAcceleratorsResponse_nextToken,
    describeAcceleratorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticInference.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccelerators' smart constructor.
data DescribeAccelerators = DescribeAccelerators'
  { -- | The IDs of the accelerators to describe.
    acceleratorIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters. Filter names and values are case-sensitive. Valid
    -- filter names are: accelerator-types: can provide a list of accelerator
    -- type names to filter for. instance-id: can provide a list of EC2
    -- instance ids to filter for.
    filters :: Prelude.Maybe [Filter],
    -- | The total number of items to return in the command\'s output. If the
    -- total number of items available is more than the value specified, a
    -- NextToken is provided in the command\'s output. To resume pagination,
    -- provide the NextToken value in the starting-token argument of a
    -- subsequent command. Do not use the NextToken response element directly
    -- outside of the AWS CLI.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccelerators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorIds', 'describeAccelerators_acceleratorIds' - The IDs of the accelerators to describe.
--
-- 'filters', 'describeAccelerators_filters' - One or more filters. Filter names and values are case-sensitive. Valid
-- filter names are: accelerator-types: can provide a list of accelerator
-- type names to filter for. instance-id: can provide a list of EC2
-- instance ids to filter for.
--
-- 'maxResults', 'describeAccelerators_maxResults' - The total number of items to return in the command\'s output. If the
-- total number of items available is more than the value specified, a
-- NextToken is provided in the command\'s output. To resume pagination,
-- provide the NextToken value in the starting-token argument of a
-- subsequent command. Do not use the NextToken response element directly
-- outside of the AWS CLI.
--
-- 'nextToken', 'describeAccelerators_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
newDescribeAccelerators ::
  DescribeAccelerators
newDescribeAccelerators =
  DescribeAccelerators'
    { acceleratorIds =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The IDs of the accelerators to describe.
describeAccelerators_acceleratorIds :: Lens.Lens' DescribeAccelerators (Prelude.Maybe [Prelude.Text])
describeAccelerators_acceleratorIds = Lens.lens (\DescribeAccelerators' {acceleratorIds} -> acceleratorIds) (\s@DescribeAccelerators' {} a -> s {acceleratorIds = a} :: DescribeAccelerators) Prelude.. Lens.mapping Lens.coerced

-- | One or more filters. Filter names and values are case-sensitive. Valid
-- filter names are: accelerator-types: can provide a list of accelerator
-- type names to filter for. instance-id: can provide a list of EC2
-- instance ids to filter for.
describeAccelerators_filters :: Lens.Lens' DescribeAccelerators (Prelude.Maybe [Filter])
describeAccelerators_filters = Lens.lens (\DescribeAccelerators' {filters} -> filters) (\s@DescribeAccelerators' {} a -> s {filters = a} :: DescribeAccelerators) Prelude.. Lens.mapping Lens.coerced

-- | The total number of items to return in the command\'s output. If the
-- total number of items available is more than the value specified, a
-- NextToken is provided in the command\'s output. To resume pagination,
-- provide the NextToken value in the starting-token argument of a
-- subsequent command. Do not use the NextToken response element directly
-- outside of the AWS CLI.
describeAccelerators_maxResults :: Lens.Lens' DescribeAccelerators (Prelude.Maybe Prelude.Natural)
describeAccelerators_maxResults = Lens.lens (\DescribeAccelerators' {maxResults} -> maxResults) (\s@DescribeAccelerators' {} a -> s {maxResults = a} :: DescribeAccelerators)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
describeAccelerators_nextToken :: Lens.Lens' DescribeAccelerators (Prelude.Maybe Prelude.Text)
describeAccelerators_nextToken = Lens.lens (\DescribeAccelerators' {nextToken} -> nextToken) (\s@DescribeAccelerators' {} a -> s {nextToken = a} :: DescribeAccelerators)

instance Core.AWSPager DescribeAccelerators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAcceleratorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAcceleratorsResponse_acceleratorSet
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeAccelerators_nextToken
          Lens..~ rs
          Lens.^? describeAcceleratorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeAccelerators where
  type
    AWSResponse DescribeAccelerators =
      DescribeAcceleratorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAcceleratorsResponse'
            Prelude.<$> (x Data..?> "acceleratorSet" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccelerators where
  hashWithSalt _salt DescribeAccelerators' {..} =
    _salt
      `Prelude.hashWithSalt` acceleratorIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeAccelerators where
  rnf DescribeAccelerators' {..} =
    Prelude.rnf acceleratorIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeAccelerators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccelerators where
  toJSON DescribeAccelerators' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("acceleratorIds" Data..=)
              Prelude.<$> acceleratorIds,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeAccelerators where
  toPath = Prelude.const "/describe-accelerators"

instance Data.ToQuery DescribeAccelerators where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAcceleratorsResponse' smart constructor.
data DescribeAcceleratorsResponse = DescribeAcceleratorsResponse'
  { -- | The details of the Elastic Inference Accelerators.
    acceleratorSet :: Prelude.Maybe [ElasticInferenceAccelerator],
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAcceleratorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorSet', 'describeAcceleratorsResponse_acceleratorSet' - The details of the Elastic Inference Accelerators.
--
-- 'nextToken', 'describeAcceleratorsResponse_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'httpStatus', 'describeAcceleratorsResponse_httpStatus' - The response's http status code.
newDescribeAcceleratorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAcceleratorsResponse
newDescribeAcceleratorsResponse pHttpStatus_ =
  DescribeAcceleratorsResponse'
    { acceleratorSet =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the Elastic Inference Accelerators.
describeAcceleratorsResponse_acceleratorSet :: Lens.Lens' DescribeAcceleratorsResponse (Prelude.Maybe [ElasticInferenceAccelerator])
describeAcceleratorsResponse_acceleratorSet = Lens.lens (\DescribeAcceleratorsResponse' {acceleratorSet} -> acceleratorSet) (\s@DescribeAcceleratorsResponse' {} a -> s {acceleratorSet = a} :: DescribeAcceleratorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
describeAcceleratorsResponse_nextToken :: Lens.Lens' DescribeAcceleratorsResponse (Prelude.Maybe Prelude.Text)
describeAcceleratorsResponse_nextToken = Lens.lens (\DescribeAcceleratorsResponse' {nextToken} -> nextToken) (\s@DescribeAcceleratorsResponse' {} a -> s {nextToken = a} :: DescribeAcceleratorsResponse)

-- | The response's http status code.
describeAcceleratorsResponse_httpStatus :: Lens.Lens' DescribeAcceleratorsResponse Prelude.Int
describeAcceleratorsResponse_httpStatus = Lens.lens (\DescribeAcceleratorsResponse' {httpStatus} -> httpStatus) (\s@DescribeAcceleratorsResponse' {} a -> s {httpStatus = a} :: DescribeAcceleratorsResponse)

instance Prelude.NFData DescribeAcceleratorsResponse where
  rnf DescribeAcceleratorsResponse' {..} =
    Prelude.rnf acceleratorSet
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
