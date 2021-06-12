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
-- Module      : Network.AWS.EC2.DescribeIamInstanceProfileAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IAM instance profile associations.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeIamInstanceProfileAssociations
  ( -- * Creating a Request
    DescribeIamInstanceProfileAssociations (..),
    newDescribeIamInstanceProfileAssociations,

    -- * Request Lenses
    describeIamInstanceProfileAssociations_nextToken,
    describeIamInstanceProfileAssociations_maxResults,
    describeIamInstanceProfileAssociations_associationIds,
    describeIamInstanceProfileAssociations_filters,

    -- * Destructuring the Response
    DescribeIamInstanceProfileAssociationsResponse (..),
    newDescribeIamInstanceProfileAssociationsResponse,

    -- * Response Lenses
    describeIamInstanceProfileAssociationsResponse_nextToken,
    describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations,
    describeIamInstanceProfileAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIamInstanceProfileAssociations' smart constructor.
data DescribeIamInstanceProfileAssociations = DescribeIamInstanceProfileAssociations'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The IAM instance profile associations.
    associationIds :: Core.Maybe [Core.Text],
    -- | The filters.
    --
    -- -   @instance-id@ - The ID of the instance.
    --
    -- -   @state@ - The state of the association (@associating@ | @associated@
    --     | @disassociating@).
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIamInstanceProfileAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeIamInstanceProfileAssociations_nextToken' - The token to request the next page of results.
--
-- 'maxResults', 'describeIamInstanceProfileAssociations_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'associationIds', 'describeIamInstanceProfileAssociations_associationIds' - The IAM instance profile associations.
--
-- 'filters', 'describeIamInstanceProfileAssociations_filters' - The filters.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @state@ - The state of the association (@associating@ | @associated@
--     | @disassociating@).
newDescribeIamInstanceProfileAssociations ::
  DescribeIamInstanceProfileAssociations
newDescribeIamInstanceProfileAssociations =
  DescribeIamInstanceProfileAssociations'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      associationIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to request the next page of results.
describeIamInstanceProfileAssociations_nextToken :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe Core.Text)
describeIamInstanceProfileAssociations_nextToken = Lens.lens (\DescribeIamInstanceProfileAssociations' {nextToken} -> nextToken) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {nextToken = a} :: DescribeIamInstanceProfileAssociations)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
describeIamInstanceProfileAssociations_maxResults :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe Core.Natural)
describeIamInstanceProfileAssociations_maxResults = Lens.lens (\DescribeIamInstanceProfileAssociations' {maxResults} -> maxResults) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {maxResults = a} :: DescribeIamInstanceProfileAssociations)

-- | The IAM instance profile associations.
describeIamInstanceProfileAssociations_associationIds :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe [Core.Text])
describeIamInstanceProfileAssociations_associationIds = Lens.lens (\DescribeIamInstanceProfileAssociations' {associationIds} -> associationIds) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {associationIds = a} :: DescribeIamInstanceProfileAssociations) Core.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @state@ - The state of the association (@associating@ | @associated@
--     | @disassociating@).
describeIamInstanceProfileAssociations_filters :: Lens.Lens' DescribeIamInstanceProfileAssociations (Core.Maybe [Filter])
describeIamInstanceProfileAssociations_filters = Lens.lens (\DescribeIamInstanceProfileAssociations' {filters} -> filters) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {filters = a} :: DescribeIamInstanceProfileAssociations) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeIamInstanceProfileAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeIamInstanceProfileAssociations_nextToken
          Lens..~ rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeIamInstanceProfileAssociations
  where
  type
    AWSResponse
      DescribeIamInstanceProfileAssociations =
      DescribeIamInstanceProfileAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIamInstanceProfileAssociationsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "iamInstanceProfileAssociationSet"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeIamInstanceProfileAssociations

instance
  Core.NFData
    DescribeIamInstanceProfileAssociations

instance
  Core.ToHeaders
    DescribeIamInstanceProfileAssociations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeIamInstanceProfileAssociations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeIamInstanceProfileAssociations
  where
  toQuery DescribeIamInstanceProfileAssociations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeIamInstanceProfileAssociations" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "AssociationId"
              Core.<$> associationIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeIamInstanceProfileAssociationsResponse' smart constructor.
data DescribeIamInstanceProfileAssociationsResponse = DescribeIamInstanceProfileAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the IAM instance profile associations.
    iamInstanceProfileAssociations :: Core.Maybe [IamInstanceProfileAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIamInstanceProfileAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeIamInstanceProfileAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'iamInstanceProfileAssociations', 'describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations' - Information about the IAM instance profile associations.
--
-- 'httpStatus', 'describeIamInstanceProfileAssociationsResponse_httpStatus' - The response's http status code.
newDescribeIamInstanceProfileAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeIamInstanceProfileAssociationsResponse
newDescribeIamInstanceProfileAssociationsResponse
  pHttpStatus_ =
    DescribeIamInstanceProfileAssociationsResponse'
      { nextToken =
          Core.Nothing,
        iamInstanceProfileAssociations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIamInstanceProfileAssociationsResponse_nextToken :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Core.Maybe Core.Text)
describeIamInstanceProfileAssociationsResponse_nextToken = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {nextToken = a} :: DescribeIamInstanceProfileAssociationsResponse)

-- | Information about the IAM instance profile associations.
describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Core.Maybe [IamInstanceProfileAssociation])
describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {iamInstanceProfileAssociations} -> iamInstanceProfileAssociations) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {iamInstanceProfileAssociations = a} :: DescribeIamInstanceProfileAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeIamInstanceProfileAssociationsResponse_httpStatus :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse Core.Int
describeIamInstanceProfileAssociationsResponse_httpStatus = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeIamInstanceProfileAssociationsResponse)

instance
  Core.NFData
    DescribeIamInstanceProfileAssociationsResponse
