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
-- Module      : Amazonka.EC2.DescribeIamInstanceProfileAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IAM instance profile associations.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeIamInstanceProfileAssociations
  ( -- * Creating a Request
    DescribeIamInstanceProfileAssociations (..),
    newDescribeIamInstanceProfileAssociations,

    -- * Request Lenses
    describeIamInstanceProfileAssociations_associationIds,
    describeIamInstanceProfileAssociations_filters,
    describeIamInstanceProfileAssociations_maxResults,
    describeIamInstanceProfileAssociations_nextToken,

    -- * Destructuring the Response
    DescribeIamInstanceProfileAssociationsResponse (..),
    newDescribeIamInstanceProfileAssociationsResponse,

    -- * Response Lenses
    describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations,
    describeIamInstanceProfileAssociationsResponse_nextToken,
    describeIamInstanceProfileAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIamInstanceProfileAssociations' smart constructor.
data DescribeIamInstanceProfileAssociations = DescribeIamInstanceProfileAssociations'
  { -- | The IAM instance profile associations.
    associationIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @instance-id@ - The ID of the instance.
    --
    -- -   @state@ - The state of the association (@associating@ | @associated@
    --     | @disassociating@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIamInstanceProfileAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationIds', 'describeIamInstanceProfileAssociations_associationIds' - The IAM instance profile associations.
--
-- 'filters', 'describeIamInstanceProfileAssociations_filters' - The filters.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @state@ - The state of the association (@associating@ | @associated@
--     | @disassociating@).
--
-- 'maxResults', 'describeIamInstanceProfileAssociations_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'describeIamInstanceProfileAssociations_nextToken' - The token to request the next page of results.
newDescribeIamInstanceProfileAssociations ::
  DescribeIamInstanceProfileAssociations
newDescribeIamInstanceProfileAssociations =
  DescribeIamInstanceProfileAssociations'
    { associationIds =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The IAM instance profile associations.
describeIamInstanceProfileAssociations_associationIds :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe [Prelude.Text])
describeIamInstanceProfileAssociations_associationIds = Lens.lens (\DescribeIamInstanceProfileAssociations' {associationIds} -> associationIds) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {associationIds = a} :: DescribeIamInstanceProfileAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @state@ - The state of the association (@associating@ | @associated@
--     | @disassociating@).
describeIamInstanceProfileAssociations_filters :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe [Filter])
describeIamInstanceProfileAssociations_filters = Lens.lens (\DescribeIamInstanceProfileAssociations' {filters} -> filters) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {filters = a} :: DescribeIamInstanceProfileAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
describeIamInstanceProfileAssociations_maxResults :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe Prelude.Natural)
describeIamInstanceProfileAssociations_maxResults = Lens.lens (\DescribeIamInstanceProfileAssociations' {maxResults} -> maxResults) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {maxResults = a} :: DescribeIamInstanceProfileAssociations)

-- | The token to request the next page of results.
describeIamInstanceProfileAssociations_nextToken :: Lens.Lens' DescribeIamInstanceProfileAssociations (Prelude.Maybe Prelude.Text)
describeIamInstanceProfileAssociations_nextToken = Lens.lens (\DescribeIamInstanceProfileAssociations' {nextToken} -> nextToken) (\s@DescribeIamInstanceProfileAssociations' {} a -> s {nextToken = a} :: DescribeIamInstanceProfileAssociations)

instance
  Core.AWSPager
    DescribeIamInstanceProfileAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeIamInstanceProfileAssociations_nextToken
          Lens..~ rs
            Lens.^? describeIamInstanceProfileAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeIamInstanceProfileAssociations
  where
  type
    AWSResponse
      DescribeIamInstanceProfileAssociations =
      DescribeIamInstanceProfileAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIamInstanceProfileAssociationsResponse'
            Prelude.<$> ( x Data..@? "iamInstanceProfileAssociationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> (x Data..@? "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeIamInstanceProfileAssociations
  where
  hashWithSalt
    _salt
    DescribeIamInstanceProfileAssociations' {..} =
      _salt `Prelude.hashWithSalt` associationIds
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeIamInstanceProfileAssociations
  where
  rnf DescribeIamInstanceProfileAssociations' {..} =
    Prelude.rnf associationIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeIamInstanceProfileAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeIamInstanceProfileAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeIamInstanceProfileAssociations
  where
  toQuery DescribeIamInstanceProfileAssociations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeIamInstanceProfileAssociations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AssociationId"
              Prelude.<$> associationIds
          ),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeIamInstanceProfileAssociationsResponse' smart constructor.
data DescribeIamInstanceProfileAssociationsResponse = DescribeIamInstanceProfileAssociationsResponse'
  { -- | Information about the IAM instance profile associations.
    iamInstanceProfileAssociations :: Prelude.Maybe [IamInstanceProfileAssociation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIamInstanceProfileAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamInstanceProfileAssociations', 'describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations' - Information about the IAM instance profile associations.
--
-- 'nextToken', 'describeIamInstanceProfileAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeIamInstanceProfileAssociationsResponse_httpStatus' - The response's http status code.
newDescribeIamInstanceProfileAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIamInstanceProfileAssociationsResponse
newDescribeIamInstanceProfileAssociationsResponse
  pHttpStatus_ =
    DescribeIamInstanceProfileAssociationsResponse'
      { iamInstanceProfileAssociations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the IAM instance profile associations.
describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Prelude.Maybe [IamInstanceProfileAssociation])
describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {iamInstanceProfileAssociations} -> iamInstanceProfileAssociations) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {iamInstanceProfileAssociations = a} :: DescribeIamInstanceProfileAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIamInstanceProfileAssociationsResponse_nextToken :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse (Prelude.Maybe Prelude.Text)
describeIamInstanceProfileAssociationsResponse_nextToken = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {nextToken = a} :: DescribeIamInstanceProfileAssociationsResponse)

-- | The response's http status code.
describeIamInstanceProfileAssociationsResponse_httpStatus :: Lens.Lens' DescribeIamInstanceProfileAssociationsResponse Prelude.Int
describeIamInstanceProfileAssociationsResponse_httpStatus = Lens.lens (\DescribeIamInstanceProfileAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeIamInstanceProfileAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeIamInstanceProfileAssociationsResponse)

instance
  Prelude.NFData
    DescribeIamInstanceProfileAssociationsResponse
  where
  rnf
    DescribeIamInstanceProfileAssociationsResponse' {..} =
      Prelude.rnf iamInstanceProfileAssociations
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
