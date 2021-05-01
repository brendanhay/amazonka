{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIamInstanceProfileAssociations' smart constructor.
data DescribeIamInstanceProfileAssociations = DescribeIamInstanceProfileAssociations'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IAM instance profile associations.
    associationIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @instance-id@ - The ID of the instance.
    --
    -- -   @state@ - The state of the association (@associating@ | @associated@
    --     | @disassociating@).
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      associationIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to request the next page of results.
describeIamInstanceProfileAssociations_nextToken :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe Prelude.Text)
describeIamInstanceProfileAssociations_nextToken = Lens.lens (\DescribeIamInstanceProfileAssociations' {nextToken} -> nextToken) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {nextToken = a} :: DescribeIamInstanceProfileAssociations)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
describeIamInstanceProfileAssociations_maxResults :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe Prelude.Natural)
describeIamInstanceProfileAssociations_maxResults = Lens.lens (\DescribeIamInstanceProfileAssociations' {maxResults} -> maxResults) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {maxResults = a} :: DescribeIamInstanceProfileAssociations)

-- | The IAM instance profile associations.
describeIamInstanceProfileAssociations_associationIds :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe [Prelude.Text])
describeIamInstanceProfileAssociations_associationIds = Lens.lens (\DescribeIamInstanceProfileAssociations' {associationIds} -> associationIds) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {associationIds = a} :: DescribeIamInstanceProfileAssociations) Prelude.. Lens.mapping Prelude._Coerce

-- | The filters.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @state@ - The state of the association (@associating@ | @associated@
--     | @disassociating@).
describeIamInstanceProfileAssociations_filters :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe [Filter])
describeIamInstanceProfileAssociations_filters = Lens.lens (\DescribeIamInstanceProfileAssociations' {filters} -> filters) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {filters = a} :: DescribeIamInstanceProfileAssociations) Prelude.. Lens.mapping Prelude._Coerce

instance
  Pager.AWSPager
    DescribeIamInstanceProfileAssociations
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeIamInstanceProfileAssociations_nextToken
          Lens..~ rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeIamInstanceProfileAssociations
  where
  type
    Rs DescribeIamInstanceProfileAssociations =
      DescribeIamInstanceProfileAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIamInstanceProfileAssociationsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
              Prelude.<*> ( x Prelude..@? "iamInstanceProfileAssociationSet"
                              Prelude..!@ Prelude.mempty
                              Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeIamInstanceProfileAssociations

instance
  Prelude.NFData
    DescribeIamInstanceProfileAssociations

instance
  Prelude.ToHeaders
    DescribeIamInstanceProfileAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeIamInstanceProfileAssociations
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeIamInstanceProfileAssociations
  where
  toQuery DescribeIamInstanceProfileAssociations' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeIamInstanceProfileAssociations" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          ( Prelude.toQueryList "AssociationId"
              Prelude.<$> associationIds
          ),
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeIamInstanceProfileAssociationsResponse' smart constructor.
data DescribeIamInstanceProfileAssociationsResponse = DescribeIamInstanceProfileAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the IAM instance profile associations.
    iamInstanceProfileAssociations :: Prelude.Maybe [IamInstanceProfileAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeIamInstanceProfileAssociationsResponse
newDescribeIamInstanceProfileAssociationsResponse
  pHttpStatus_ =
    DescribeIamInstanceProfileAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        iamInstanceProfileAssociations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIamInstanceProfileAssociationsResponse_nextToken :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Prelude.Maybe Prelude.Text)
describeIamInstanceProfileAssociationsResponse_nextToken = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {nextToken = a} :: DescribeIamInstanceProfileAssociationsResponse)

-- | Information about the IAM instance profile associations.
describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Prelude.Maybe [IamInstanceProfileAssociation])
describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {iamInstanceProfileAssociations} -> iamInstanceProfileAssociations) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {iamInstanceProfileAssociations = a} :: DescribeIamInstanceProfileAssociationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeIamInstanceProfileAssociationsResponse_httpStatus :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse Prelude.Int
describeIamInstanceProfileAssociationsResponse_httpStatus = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeIamInstanceProfileAssociationsResponse)

instance
  Prelude.NFData
    DescribeIamInstanceProfileAssociationsResponse
