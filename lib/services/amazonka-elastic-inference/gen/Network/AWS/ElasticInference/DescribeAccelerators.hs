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
-- Module      : Network.AWS.ElasticInference.DescribeAccelerators
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes information over a provided set of accelerators belonging to
-- an account.
--
-- This operation returns paginated results.
module Network.AWS.ElasticInference.DescribeAccelerators
  ( -- * Creating a Request
    DescribeAccelerators (..),
    newDescribeAccelerators,

    -- * Request Lenses
    describeAccelerators_filters,
    describeAccelerators_nextToken,
    describeAccelerators_maxResults,
    describeAccelerators_acceleratorIds,

    -- * Destructuring the Response
    DescribeAcceleratorsResponse (..),
    newDescribeAcceleratorsResponse,

    -- * Response Lenses
    describeAcceleratorsResponse_acceleratorSet,
    describeAcceleratorsResponse_nextToken,
    describeAcceleratorsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticInference.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAccelerators' smart constructor.
data DescribeAccelerators = DescribeAccelerators'
  { -- | One or more filters. Filter names and values are case-sensitive. Valid
    -- filter names are: accelerator-types: can provide a list of accelerator
    -- type names to filter for. instance-id: can provide a list of EC2
    -- instance ids to filter for.
    filters :: Prelude.Maybe [Filter],
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of items to return in the command\'s output. If the
    -- total number of items available is more than the value specified, a
    -- NextToken is provided in the command\'s output. To resume pagination,
    -- provide the NextToken value in the starting-token argument of a
    -- subsequent command. Do not use the NextToken response element directly
    -- outside of the AWS CLI.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the accelerators to describe.
    acceleratorIds :: Prelude.Maybe [Prelude.Text]
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
-- 'filters', 'describeAccelerators_filters' - One or more filters. Filter names and values are case-sensitive. Valid
-- filter names are: accelerator-types: can provide a list of accelerator
-- type names to filter for. instance-id: can provide a list of EC2
-- instance ids to filter for.
--
-- 'nextToken', 'describeAccelerators_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'maxResults', 'describeAccelerators_maxResults' - The total number of items to return in the command\'s output. If the
-- total number of items available is more than the value specified, a
-- NextToken is provided in the command\'s output. To resume pagination,
-- provide the NextToken value in the starting-token argument of a
-- subsequent command. Do not use the NextToken response element directly
-- outside of the AWS CLI.
--
-- 'acceleratorIds', 'describeAccelerators_acceleratorIds' - The IDs of the accelerators to describe.
newDescribeAccelerators ::
  DescribeAccelerators
newDescribeAccelerators =
  DescribeAccelerators'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      acceleratorIds = Prelude.Nothing
    }

-- | One or more filters. Filter names and values are case-sensitive. Valid
-- filter names are: accelerator-types: can provide a list of accelerator
-- type names to filter for. instance-id: can provide a list of EC2
-- instance ids to filter for.
describeAccelerators_filters :: Lens.Lens' DescribeAccelerators (Prelude.Maybe [Filter])
describeAccelerators_filters = Lens.lens (\DescribeAccelerators' {filters} -> filters) (\s@DescribeAccelerators' {} a -> s {filters = a} :: DescribeAccelerators) Prelude.. Lens.mapping Lens.coerced

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
describeAccelerators_nextToken :: Lens.Lens' DescribeAccelerators (Prelude.Maybe Prelude.Text)
describeAccelerators_nextToken = Lens.lens (\DescribeAccelerators' {nextToken} -> nextToken) (\s@DescribeAccelerators' {} a -> s {nextToken = a} :: DescribeAccelerators)

-- | The total number of items to return in the command\'s output. If the
-- total number of items available is more than the value specified, a
-- NextToken is provided in the command\'s output. To resume pagination,
-- provide the NextToken value in the starting-token argument of a
-- subsequent command. Do not use the NextToken response element directly
-- outside of the AWS CLI.
describeAccelerators_maxResults :: Lens.Lens' DescribeAccelerators (Prelude.Maybe Prelude.Natural)
describeAccelerators_maxResults = Lens.lens (\DescribeAccelerators' {maxResults} -> maxResults) (\s@DescribeAccelerators' {} a -> s {maxResults = a} :: DescribeAccelerators)

-- | The IDs of the accelerators to describe.
describeAccelerators_acceleratorIds :: Lens.Lens' DescribeAccelerators (Prelude.Maybe [Prelude.Text])
describeAccelerators_acceleratorIds = Lens.lens (\DescribeAccelerators' {acceleratorIds} -> acceleratorIds) (\s@DescribeAccelerators' {} a -> s {acceleratorIds = a} :: DescribeAccelerators) Prelude.. Lens.mapping Lens.coerced

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAccelerators_nextToken
          Lens..~ rs
          Lens.^? describeAcceleratorsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAccelerators where
  type
    AWSResponse DescribeAccelerators =
      DescribeAcceleratorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAcceleratorsResponse'
            Prelude.<$> (x Core..?> "acceleratorSet" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccelerators

instance Prelude.NFData DescribeAccelerators

instance Core.ToHeaders DescribeAccelerators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAccelerators where
  toJSON DescribeAccelerators' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("acceleratorIds" Core..=)
              Prelude.<$> acceleratorIds
          ]
      )

instance Core.ToPath DescribeAccelerators where
  toPath = Prelude.const "/describe-accelerators"

instance Core.ToQuery DescribeAccelerators where
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

instance Prelude.NFData DescribeAcceleratorsResponse
