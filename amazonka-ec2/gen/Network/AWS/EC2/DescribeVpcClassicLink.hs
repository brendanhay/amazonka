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
-- Module      : Network.AWS.EC2.DescribeVpcClassicLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ClassicLink status of one or more VPCs.
module Network.AWS.EC2.DescribeVpcClassicLink
  ( -- * Creating a Request
    DescribeVpcClassicLink (..),
    newDescribeVpcClassicLink,

    -- * Request Lenses
    describeVpcClassicLink_dryRun,
    describeVpcClassicLink_filters,
    describeVpcClassicLink_vpcIds,

    -- * Destructuring the Response
    DescribeVpcClassicLinkResponse (..),
    newDescribeVpcClassicLinkResponse,

    -- * Response Lenses
    describeVpcClassicLinkResponse_vpcs,
    describeVpcClassicLinkResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcClassicLink' smart constructor.
data DescribeVpcClassicLink = DescribeVpcClassicLink'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    -- -   @is-classic-link-enabled@ - Whether the VPC is enabled for
    --     ClassicLink (@true@ | @false@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    filters :: Core.Maybe [Filter],
    -- | One or more VPCs for which you want to describe the ClassicLink status.
    vpcIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcClassicLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpcClassicLink_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVpcClassicLink_filters' - One or more filters.
--
-- -   @is-classic-link-enabled@ - Whether the VPC is enabled for
--     ClassicLink (@true@ | @false@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- 'vpcIds', 'describeVpcClassicLink_vpcIds' - One or more VPCs for which you want to describe the ClassicLink status.
newDescribeVpcClassicLink ::
  DescribeVpcClassicLink
newDescribeVpcClassicLink =
  DescribeVpcClassicLink'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      vpcIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcClassicLink_dryRun :: Lens.Lens' DescribeVpcClassicLink (Core.Maybe Core.Bool)
describeVpcClassicLink_dryRun = Lens.lens (\DescribeVpcClassicLink' {dryRun} -> dryRun) (\s@DescribeVpcClassicLink' {} a -> s {dryRun = a} :: DescribeVpcClassicLink)

-- | One or more filters.
--
-- -   @is-classic-link-enabled@ - Whether the VPC is enabled for
--     ClassicLink (@true@ | @false@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
describeVpcClassicLink_filters :: Lens.Lens' DescribeVpcClassicLink (Core.Maybe [Filter])
describeVpcClassicLink_filters = Lens.lens (\DescribeVpcClassicLink' {filters} -> filters) (\s@DescribeVpcClassicLink' {} a -> s {filters = a} :: DescribeVpcClassicLink) Core.. Lens.mapping Lens._Coerce

-- | One or more VPCs for which you want to describe the ClassicLink status.
describeVpcClassicLink_vpcIds :: Lens.Lens' DescribeVpcClassicLink (Core.Maybe [Core.Text])
describeVpcClassicLink_vpcIds = Lens.lens (\DescribeVpcClassicLink' {vpcIds} -> vpcIds) (\s@DescribeVpcClassicLink' {} a -> s {vpcIds = a} :: DescribeVpcClassicLink) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeVpcClassicLink where
  type
    AWSResponse DescribeVpcClassicLink =
      DescribeVpcClassicLinkResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcClassicLinkResponse'
            Core.<$> ( x Core..@? "vpcSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVpcClassicLink

instance Core.NFData DescribeVpcClassicLink

instance Core.ToHeaders DescribeVpcClassicLink where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVpcClassicLink where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVpcClassicLink where
  toQuery DescribeVpcClassicLink' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeVpcClassicLink" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          (Core.toQueryList "VpcId" Core.<$> vpcIds)
      ]

-- | /See:/ 'newDescribeVpcClassicLinkResponse' smart constructor.
data DescribeVpcClassicLinkResponse = DescribeVpcClassicLinkResponse'
  { -- | The ClassicLink status of one or more VPCs.
    vpcs :: Core.Maybe [VpcClassicLink],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcClassicLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcs', 'describeVpcClassicLinkResponse_vpcs' - The ClassicLink status of one or more VPCs.
--
-- 'httpStatus', 'describeVpcClassicLinkResponse_httpStatus' - The response's http status code.
newDescribeVpcClassicLinkResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcClassicLinkResponse
newDescribeVpcClassicLinkResponse pHttpStatus_ =
  DescribeVpcClassicLinkResponse'
    { vpcs =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ClassicLink status of one or more VPCs.
describeVpcClassicLinkResponse_vpcs :: Lens.Lens' DescribeVpcClassicLinkResponse (Core.Maybe [VpcClassicLink])
describeVpcClassicLinkResponse_vpcs = Lens.lens (\DescribeVpcClassicLinkResponse' {vpcs} -> vpcs) (\s@DescribeVpcClassicLinkResponse' {} a -> s {vpcs = a} :: DescribeVpcClassicLinkResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcClassicLinkResponse_httpStatus :: Lens.Lens' DescribeVpcClassicLinkResponse Core.Int
describeVpcClassicLinkResponse_httpStatus = Lens.lens (\DescribeVpcClassicLinkResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcClassicLinkResponse' {} a -> s {httpStatus = a} :: DescribeVpcClassicLinkResponse)

instance Core.NFData DescribeVpcClassicLinkResponse
