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
-- Module      : Network.AWS.EC2.DescribeVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ClassicLink DNS support status of one or more VPCs. If
-- enabled, the DNS hostname of a linked EC2-Classic instance resolves to
-- its private IP address when addressed from an instance in the VPC to
-- which it\'s linked. Similarly, the DNS hostname of an instance in a VPC
-- resolves to its private IP address when addressed from a linked
-- EC2-Classic instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcClassicLinkDnsSupport
  ( -- * Creating a Request
    DescribeVpcClassicLinkDnsSupport (..),
    newDescribeVpcClassicLinkDnsSupport,

    -- * Request Lenses
    describeVpcClassicLinkDnsSupport_nextToken,
    describeVpcClassicLinkDnsSupport_maxResults,
    describeVpcClassicLinkDnsSupport_vpcIds,

    -- * Destructuring the Response
    DescribeVpcClassicLinkDnsSupportResponse (..),
    newDescribeVpcClassicLinkDnsSupportResponse,

    -- * Response Lenses
    describeVpcClassicLinkDnsSupportResponse_nextToken,
    describeVpcClassicLinkDnsSupportResponse_vpcs,
    describeVpcClassicLinkDnsSupportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcClassicLinkDnsSupport' smart constructor.
data DescribeVpcClassicLinkDnsSupport = DescribeVpcClassicLinkDnsSupport'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more VPC IDs.
    vpcIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcClassicLinkDnsSupport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcClassicLinkDnsSupport_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'describeVpcClassicLinkDnsSupport_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'vpcIds', 'describeVpcClassicLinkDnsSupport_vpcIds' - One or more VPC IDs.
newDescribeVpcClassicLinkDnsSupport ::
  DescribeVpcClassicLinkDnsSupport
newDescribeVpcClassicLinkDnsSupport =
  DescribeVpcClassicLinkDnsSupport'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      vpcIds = Prelude.Nothing
    }

-- | The token for the next page of results.
describeVpcClassicLinkDnsSupport_nextToken :: Lens.Lens' DescribeVpcClassicLinkDnsSupport (Prelude.Maybe Prelude.Text)
describeVpcClassicLinkDnsSupport_nextToken = Lens.lens (\DescribeVpcClassicLinkDnsSupport' {nextToken} -> nextToken) (\s@DescribeVpcClassicLinkDnsSupport' {} a -> s {nextToken = a} :: DescribeVpcClassicLinkDnsSupport)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeVpcClassicLinkDnsSupport_maxResults :: Lens.Lens' DescribeVpcClassicLinkDnsSupport (Prelude.Maybe Prelude.Natural)
describeVpcClassicLinkDnsSupport_maxResults = Lens.lens (\DescribeVpcClassicLinkDnsSupport' {maxResults} -> maxResults) (\s@DescribeVpcClassicLinkDnsSupport' {} a -> s {maxResults = a} :: DescribeVpcClassicLinkDnsSupport)

-- | One or more VPC IDs.
describeVpcClassicLinkDnsSupport_vpcIds :: Lens.Lens' DescribeVpcClassicLinkDnsSupport (Prelude.Maybe [Prelude.Text])
describeVpcClassicLinkDnsSupport_vpcIds = Lens.lens (\DescribeVpcClassicLinkDnsSupport' {vpcIds} -> vpcIds) (\s@DescribeVpcClassicLinkDnsSupport' {} a -> s {vpcIds = a} :: DescribeVpcClassicLinkDnsSupport) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeVpcClassicLinkDnsSupport
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcClassicLinkDnsSupportResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcClassicLinkDnsSupportResponse_vpcs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVpcClassicLinkDnsSupport_nextToken
          Lens..~ rs
          Lens.^? describeVpcClassicLinkDnsSupportResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcClassicLinkDnsSupport
  where
  type
    AWSResponse DescribeVpcClassicLinkDnsSupport =
      DescribeVpcClassicLinkDnsSupportResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcClassicLinkDnsSupportResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "vpcs" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcClassicLinkDnsSupport

instance
  Prelude.NFData
    DescribeVpcClassicLinkDnsSupport

instance
  Core.ToHeaders
    DescribeVpcClassicLinkDnsSupport
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeVpcClassicLinkDnsSupport where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeVpcClassicLinkDnsSupport
  where
  toQuery DescribeVpcClassicLinkDnsSupport' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeVpcClassicLinkDnsSupport" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "VpcIds" Prelude.<$> vpcIds)
      ]

-- | /See:/ 'newDescribeVpcClassicLinkDnsSupportResponse' smart constructor.
data DescribeVpcClassicLinkDnsSupportResponse = DescribeVpcClassicLinkDnsSupportResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the ClassicLink DNS support status of the VPCs.
    vpcs :: Prelude.Maybe [ClassicLinkDnsSupport],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcClassicLinkDnsSupportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcClassicLinkDnsSupportResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'vpcs', 'describeVpcClassicLinkDnsSupportResponse_vpcs' - Information about the ClassicLink DNS support status of the VPCs.
--
-- 'httpStatus', 'describeVpcClassicLinkDnsSupportResponse_httpStatus' - The response's http status code.
newDescribeVpcClassicLinkDnsSupportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcClassicLinkDnsSupportResponse
newDescribeVpcClassicLinkDnsSupportResponse
  pHttpStatus_ =
    DescribeVpcClassicLinkDnsSupportResponse'
      { nextToken =
          Prelude.Nothing,
        vpcs = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcClassicLinkDnsSupportResponse_nextToken :: Lens.Lens' DescribeVpcClassicLinkDnsSupportResponse (Prelude.Maybe Prelude.Text)
describeVpcClassicLinkDnsSupportResponse_nextToken = Lens.lens (\DescribeVpcClassicLinkDnsSupportResponse' {nextToken} -> nextToken) (\s@DescribeVpcClassicLinkDnsSupportResponse' {} a -> s {nextToken = a} :: DescribeVpcClassicLinkDnsSupportResponse)

-- | Information about the ClassicLink DNS support status of the VPCs.
describeVpcClassicLinkDnsSupportResponse_vpcs :: Lens.Lens' DescribeVpcClassicLinkDnsSupportResponse (Prelude.Maybe [ClassicLinkDnsSupport])
describeVpcClassicLinkDnsSupportResponse_vpcs = Lens.lens (\DescribeVpcClassicLinkDnsSupportResponse' {vpcs} -> vpcs) (\s@DescribeVpcClassicLinkDnsSupportResponse' {} a -> s {vpcs = a} :: DescribeVpcClassicLinkDnsSupportResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcClassicLinkDnsSupportResponse_httpStatus :: Lens.Lens' DescribeVpcClassicLinkDnsSupportResponse Prelude.Int
describeVpcClassicLinkDnsSupportResponse_httpStatus = Lens.lens (\DescribeVpcClassicLinkDnsSupportResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcClassicLinkDnsSupportResponse' {} a -> s {httpStatus = a} :: DescribeVpcClassicLinkDnsSupportResponse)

instance
  Prelude.NFData
    DescribeVpcClassicLinkDnsSupportResponse
