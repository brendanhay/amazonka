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
-- Module      : Network.AWS.SSM.DescribeActivations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes details about the activation, such as the date and time the
-- activation was created, its expiration date, the IAM role assigned to
-- the instances in the activation, and the number of instances registered
-- by using this activation.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeActivations
  ( -- * Creating a Request
    DescribeActivations (..),
    newDescribeActivations,

    -- * Request Lenses
    describeActivations_nextToken,
    describeActivations_maxResults,
    describeActivations_filters,

    -- * Destructuring the Response
    DescribeActivationsResponse (..),
    newDescribeActivationsResponse,

    -- * Response Lenses
    describeActivationsResponse_nextToken,
    describeActivationsResponse_activationList,
    describeActivationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeActivations' smart constructor.
data DescribeActivations = DescribeActivations'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter to view information about your activations.
    filters :: Core.Maybe [DescribeActivationsFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeActivations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeActivations_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeActivations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeActivations_filters' - A filter to view information about your activations.
newDescribeActivations ::
  DescribeActivations
newDescribeActivations =
  DescribeActivations'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeActivations_nextToken :: Lens.Lens' DescribeActivations (Core.Maybe Core.Text)
describeActivations_nextToken = Lens.lens (\DescribeActivations' {nextToken} -> nextToken) (\s@DescribeActivations' {} a -> s {nextToken = a} :: DescribeActivations)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeActivations_maxResults :: Lens.Lens' DescribeActivations (Core.Maybe Core.Natural)
describeActivations_maxResults = Lens.lens (\DescribeActivations' {maxResults} -> maxResults) (\s@DescribeActivations' {} a -> s {maxResults = a} :: DescribeActivations)

-- | A filter to view information about your activations.
describeActivations_filters :: Lens.Lens' DescribeActivations (Core.Maybe [DescribeActivationsFilter])
describeActivations_filters = Lens.lens (\DescribeActivations' {filters} -> filters) (\s@DescribeActivations' {} a -> s {filters = a} :: DescribeActivations) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeActivations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeActivationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeActivationsResponse_activationList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeActivations_nextToken
          Lens..~ rs
          Lens.^? describeActivationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeActivations where
  type
    AWSResponse DescribeActivations =
      DescribeActivationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ActivationList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeActivations

instance Core.NFData DescribeActivations

instance Core.ToHeaders DescribeActivations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DescribeActivations" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeActivations where
  toJSON DescribeActivations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribeActivations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeActivations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeActivationsResponse' smart constructor.
data DescribeActivationsResponse = DescribeActivationsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of activations for your AWS account.
    activationList :: Core.Maybe [Activation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeActivationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeActivationsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'activationList', 'describeActivationsResponse_activationList' - A list of activations for your AWS account.
--
-- 'httpStatus', 'describeActivationsResponse_httpStatus' - The response's http status code.
newDescribeActivationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeActivationsResponse
newDescribeActivationsResponse pHttpStatus_ =
  DescribeActivationsResponse'
    { nextToken =
        Core.Nothing,
      activationList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeActivationsResponse_nextToken :: Lens.Lens' DescribeActivationsResponse (Core.Maybe Core.Text)
describeActivationsResponse_nextToken = Lens.lens (\DescribeActivationsResponse' {nextToken} -> nextToken) (\s@DescribeActivationsResponse' {} a -> s {nextToken = a} :: DescribeActivationsResponse)

-- | A list of activations for your AWS account.
describeActivationsResponse_activationList :: Lens.Lens' DescribeActivationsResponse (Core.Maybe [Activation])
describeActivationsResponse_activationList = Lens.lens (\DescribeActivationsResponse' {activationList} -> activationList) (\s@DescribeActivationsResponse' {} a -> s {activationList = a} :: DescribeActivationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeActivationsResponse_httpStatus :: Lens.Lens' DescribeActivationsResponse Core.Int
describeActivationsResponse_httpStatus = Lens.lens (\DescribeActivationsResponse' {httpStatus} -> httpStatus) (\s@DescribeActivationsResponse' {} a -> s {httpStatus = a} :: DescribeActivationsResponse)

instance Core.NFData DescribeActivationsResponse
