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
-- Module      : Network.AWS.EC2.DescribePrincipalIdFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for the root user and all IAM roles and
-- IAM users that have explicitly specified a longer ID (17-character ID)
-- preference.
--
-- By default, all IAM roles and IAM users default to the same ID settings
-- as the root user, unless they explicitly override the settings. This
-- request is useful for identifying those IAM users and IAM roles that
-- have overridden the default ID settings.
--
-- The following resource types support longer IDs: @bundle@ |
-- @conversion-task@ | @customer-gateway@ | @dhcp-options@ |
-- @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ |
-- @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ |
-- @network-acl@ | @network-acl-association@ | @network-interface@ |
-- @network-interface-attachment@ | @prefix-list@ | @reservation@ |
-- @route-table@ | @route-table-association@ | @security-group@ |
-- @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ |
-- @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ |
-- @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePrincipalIdFormat
  ( -- * Creating a Request
    DescribePrincipalIdFormat (..),
    newDescribePrincipalIdFormat,

    -- * Request Lenses
    describePrincipalIdFormat_nextToken,
    describePrincipalIdFormat_dryRun,
    describePrincipalIdFormat_maxResults,
    describePrincipalIdFormat_resources,

    -- * Destructuring the Response
    DescribePrincipalIdFormatResponse (..),
    newDescribePrincipalIdFormatResponse,

    -- * Response Lenses
    describePrincipalIdFormatResponse_nextToken,
    describePrincipalIdFormatResponse_principals,
    describePrincipalIdFormatResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePrincipalIdFormat' smart constructor.
data DescribePrincipalIdFormat = DescribePrincipalIdFormat'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned NextToken
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
    -- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
    -- @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ |
    -- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
    -- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
    -- @reservation@ | @route-table@ | @route-table-association@ |
    -- @security-group@ | @snapshot@ | @subnet@ |
    -- @subnet-cidr-block-association@ | @volume@ | @vpc@ |
    -- @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@
    -- | @vpn-connection@ | @vpn-gateway@
    resources :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePrincipalIdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePrincipalIdFormat_nextToken' - The token to request the next page of results.
--
-- 'dryRun', 'describePrincipalIdFormat_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describePrincipalIdFormat_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned NextToken
-- value.
--
-- 'resources', 'describePrincipalIdFormat_resources' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @reservation@ | @route-table@ | @route-table-association@ |
-- @security-group@ | @snapshot@ | @subnet@ |
-- @subnet-cidr-block-association@ | @volume@ | @vpc@ |
-- @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@
-- | @vpn-connection@ | @vpn-gateway@
newDescribePrincipalIdFormat ::
  DescribePrincipalIdFormat
newDescribePrincipalIdFormat =
  DescribePrincipalIdFormat'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      resources = Core.Nothing
    }

-- | The token to request the next page of results.
describePrincipalIdFormat_nextToken :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Text)
describePrincipalIdFormat_nextToken = Lens.lens (\DescribePrincipalIdFormat' {nextToken} -> nextToken) (\s@DescribePrincipalIdFormat' {} a -> s {nextToken = a} :: DescribePrincipalIdFormat)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describePrincipalIdFormat_dryRun :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Bool)
describePrincipalIdFormat_dryRun = Lens.lens (\DescribePrincipalIdFormat' {dryRun} -> dryRun) (\s@DescribePrincipalIdFormat' {} a -> s {dryRun = a} :: DescribePrincipalIdFormat)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned NextToken
-- value.
describePrincipalIdFormat_maxResults :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe Core.Natural)
describePrincipalIdFormat_maxResults = Lens.lens (\DescribePrincipalIdFormat' {maxResults} -> maxResults) (\s@DescribePrincipalIdFormat' {} a -> s {maxResults = a} :: DescribePrincipalIdFormat)

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @reservation@ | @route-table@ | @route-table-association@ |
-- @security-group@ | @snapshot@ | @subnet@ |
-- @subnet-cidr-block-association@ | @volume@ | @vpc@ |
-- @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@
-- | @vpn-connection@ | @vpn-gateway@
describePrincipalIdFormat_resources :: Lens.Lens' DescribePrincipalIdFormat (Core.Maybe [Core.Text])
describePrincipalIdFormat_resources = Lens.lens (\DescribePrincipalIdFormat' {resources} -> resources) (\s@DescribePrincipalIdFormat' {} a -> s {resources = a} :: DescribePrincipalIdFormat) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribePrincipalIdFormat where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePrincipalIdFormatResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePrincipalIdFormatResponse_principals
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePrincipalIdFormat_nextToken
          Lens..~ rs
          Lens.^? describePrincipalIdFormatResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribePrincipalIdFormat where
  type
    AWSResponse DescribePrincipalIdFormat =
      DescribePrincipalIdFormatResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePrincipalIdFormatResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "principalSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePrincipalIdFormat

instance Core.NFData DescribePrincipalIdFormat

instance Core.ToHeaders DescribePrincipalIdFormat where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribePrincipalIdFormat where
  toPath = Core.const "/"

instance Core.ToQuery DescribePrincipalIdFormat where
  toQuery DescribePrincipalIdFormat' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribePrincipalIdFormat" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Resource" Core.<$> resources)
      ]

-- | /See:/ 'newDescribePrincipalIdFormatResponse' smart constructor.
data DescribePrincipalIdFormatResponse = DescribePrincipalIdFormatResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the ID format settings for the ARN.
    principals :: Core.Maybe [PrincipalIdFormat],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePrincipalIdFormatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePrincipalIdFormatResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'principals', 'describePrincipalIdFormatResponse_principals' - Information about the ID format settings for the ARN.
--
-- 'httpStatus', 'describePrincipalIdFormatResponse_httpStatus' - The response's http status code.
newDescribePrincipalIdFormatResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePrincipalIdFormatResponse
newDescribePrincipalIdFormatResponse pHttpStatus_ =
  DescribePrincipalIdFormatResponse'
    { nextToken =
        Core.Nothing,
      principals = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describePrincipalIdFormatResponse_nextToken :: Lens.Lens' DescribePrincipalIdFormatResponse (Core.Maybe Core.Text)
describePrincipalIdFormatResponse_nextToken = Lens.lens (\DescribePrincipalIdFormatResponse' {nextToken} -> nextToken) (\s@DescribePrincipalIdFormatResponse' {} a -> s {nextToken = a} :: DescribePrincipalIdFormatResponse)

-- | Information about the ID format settings for the ARN.
describePrincipalIdFormatResponse_principals :: Lens.Lens' DescribePrincipalIdFormatResponse (Core.Maybe [PrincipalIdFormat])
describePrincipalIdFormatResponse_principals = Lens.lens (\DescribePrincipalIdFormatResponse' {principals} -> principals) (\s@DescribePrincipalIdFormatResponse' {} a -> s {principals = a} :: DescribePrincipalIdFormatResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePrincipalIdFormatResponse_httpStatus :: Lens.Lens' DescribePrincipalIdFormatResponse Core.Int
describePrincipalIdFormatResponse_httpStatus = Lens.lens (\DescribePrincipalIdFormatResponse' {httpStatus} -> httpStatus) (\s@DescribePrincipalIdFormatResponse' {} a -> s {httpStatus = a} :: DescribePrincipalIdFormatResponse)

instance
  Core.NFData
    DescribePrincipalIdFormatResponse
