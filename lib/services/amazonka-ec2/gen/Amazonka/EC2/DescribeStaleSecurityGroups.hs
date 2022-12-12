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
-- Module      : Amazonka.EC2.DescribeStaleSecurityGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the stale security group rules for security groups
-- in a specified VPC. Rules are stale when they reference a deleted
-- security group in the same VPC or in a peer VPC, or if they reference a
-- security group in a peer VPC for which the VPC peering connection has
-- been deleted.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeStaleSecurityGroups
  ( -- * Creating a Request
    DescribeStaleSecurityGroups (..),
    newDescribeStaleSecurityGroups,

    -- * Request Lenses
    describeStaleSecurityGroups_dryRun,
    describeStaleSecurityGroups_maxResults,
    describeStaleSecurityGroups_nextToken,
    describeStaleSecurityGroups_vpcId,

    -- * Destructuring the Response
    DescribeStaleSecurityGroupsResponse (..),
    newDescribeStaleSecurityGroupsResponse,

    -- * Response Lenses
    describeStaleSecurityGroupsResponse_nextToken,
    describeStaleSecurityGroupsResponse_staleSecurityGroupSet,
    describeStaleSecurityGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStaleSecurityGroups' smart constructor.
data DescribeStaleSecurityGroups = DescribeStaleSecurityGroups'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items to return for this request. The request
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a prior call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStaleSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeStaleSecurityGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeStaleSecurityGroups_maxResults' - The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describeStaleSecurityGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a prior call.)
--
-- 'vpcId', 'describeStaleSecurityGroups_vpcId' - The ID of the VPC.
newDescribeStaleSecurityGroups ::
  -- | 'vpcId'
  Prelude.Text ->
  DescribeStaleSecurityGroups
newDescribeStaleSecurityGroups pVpcId_ =
  DescribeStaleSecurityGroups'
    { dryRun =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeStaleSecurityGroups_dryRun :: Lens.Lens' DescribeStaleSecurityGroups (Prelude.Maybe Prelude.Bool)
describeStaleSecurityGroups_dryRun = Lens.lens (\DescribeStaleSecurityGroups' {dryRun} -> dryRun) (\s@DescribeStaleSecurityGroups' {} a -> s {dryRun = a} :: DescribeStaleSecurityGroups)

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeStaleSecurityGroups_maxResults :: Lens.Lens' DescribeStaleSecurityGroups (Prelude.Maybe Prelude.Natural)
describeStaleSecurityGroups_maxResults = Lens.lens (\DescribeStaleSecurityGroups' {maxResults} -> maxResults) (\s@DescribeStaleSecurityGroups' {} a -> s {maxResults = a} :: DescribeStaleSecurityGroups)

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
describeStaleSecurityGroups_nextToken :: Lens.Lens' DescribeStaleSecurityGroups (Prelude.Maybe Prelude.Text)
describeStaleSecurityGroups_nextToken = Lens.lens (\DescribeStaleSecurityGroups' {nextToken} -> nextToken) (\s@DescribeStaleSecurityGroups' {} a -> s {nextToken = a} :: DescribeStaleSecurityGroups)

-- | The ID of the VPC.
describeStaleSecurityGroups_vpcId :: Lens.Lens' DescribeStaleSecurityGroups Prelude.Text
describeStaleSecurityGroups_vpcId = Lens.lens (\DescribeStaleSecurityGroups' {vpcId} -> vpcId) (\s@DescribeStaleSecurityGroups' {} a -> s {vpcId = a} :: DescribeStaleSecurityGroups)

instance Core.AWSPager DescribeStaleSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStaleSecurityGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStaleSecurityGroupsResponse_staleSecurityGroupSet
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeStaleSecurityGroups_nextToken
          Lens..~ rs
          Lens.^? describeStaleSecurityGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeStaleSecurityGroups where
  type
    AWSResponse DescribeStaleSecurityGroups =
      DescribeStaleSecurityGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeStaleSecurityGroupsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "staleSecurityGroupSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStaleSecurityGroups where
  hashWithSalt _salt DescribeStaleSecurityGroups' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData DescribeStaleSecurityGroups where
  rnf DescribeStaleSecurityGroups' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders DescribeStaleSecurityGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeStaleSecurityGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStaleSecurityGroups where
  toQuery DescribeStaleSecurityGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeStaleSecurityGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newDescribeStaleSecurityGroupsResponse' smart constructor.
data DescribeStaleSecurityGroupsResponse = DescribeStaleSecurityGroupsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the stale security groups.
    staleSecurityGroupSet :: Prelude.Maybe [StaleSecurityGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStaleSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStaleSecurityGroupsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'staleSecurityGroupSet', 'describeStaleSecurityGroupsResponse_staleSecurityGroupSet' - Information about the stale security groups.
--
-- 'httpStatus', 'describeStaleSecurityGroupsResponse_httpStatus' - The response's http status code.
newDescribeStaleSecurityGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStaleSecurityGroupsResponse
newDescribeStaleSecurityGroupsResponse pHttpStatus_ =
  DescribeStaleSecurityGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      staleSecurityGroupSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeStaleSecurityGroupsResponse_nextToken :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Prelude.Maybe Prelude.Text)
describeStaleSecurityGroupsResponse_nextToken = Lens.lens (\DescribeStaleSecurityGroupsResponse' {nextToken} -> nextToken) (\s@DescribeStaleSecurityGroupsResponse' {} a -> s {nextToken = a} :: DescribeStaleSecurityGroupsResponse)

-- | Information about the stale security groups.
describeStaleSecurityGroupsResponse_staleSecurityGroupSet :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Prelude.Maybe [StaleSecurityGroup])
describeStaleSecurityGroupsResponse_staleSecurityGroupSet = Lens.lens (\DescribeStaleSecurityGroupsResponse' {staleSecurityGroupSet} -> staleSecurityGroupSet) (\s@DescribeStaleSecurityGroupsResponse' {} a -> s {staleSecurityGroupSet = a} :: DescribeStaleSecurityGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStaleSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeStaleSecurityGroupsResponse Prelude.Int
describeStaleSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeStaleSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeStaleSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeStaleSecurityGroupsResponse)

instance
  Prelude.NFData
    DescribeStaleSecurityGroupsResponse
  where
  rnf DescribeStaleSecurityGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf staleSecurityGroupSet
      `Prelude.seq` Prelude.rnf httpStatus
