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
-- Module      : Network.AWS.EC2.DescribeDhcpOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeDhcpOptions
  ( -- * Creating a Request
    DescribeDhcpOptions (..),
    newDescribeDhcpOptions,

    -- * Request Lenses
    describeDhcpOptions_nextToken,
    describeDhcpOptions_dhcpOptionsIds,
    describeDhcpOptions_dryRun,
    describeDhcpOptions_maxResults,
    describeDhcpOptions_filters,

    -- * Destructuring the Response
    DescribeDhcpOptionsResponse (..),
    newDescribeDhcpOptionsResponse,

    -- * Response Lenses
    describeDhcpOptionsResponse_nextToken,
    describeDhcpOptionsResponse_dhcpOptions,
    describeDhcpOptionsResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDhcpOptions' smart constructor.
data DescribeDhcpOptions = DescribeDhcpOptions'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of one or more DHCP options sets.
    --
    -- Default: Describes all your DHCP options sets.
    dhcpOptionsIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters.
    --
    -- -   @dhcp-options-id@ - The ID of a DHCP options set.
    --
    -- -   @key@ - The key for one of the options (for example, @domain-name@).
    --
    -- -   @value@ - The value for one of the options.
    --
    -- -   @owner-id@ - The ID of the AWS account that owns the DHCP options
    --     set.
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
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeDhcpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDhcpOptions_nextToken' - The token for the next page of results.
--
-- 'dhcpOptionsIds', 'describeDhcpOptions_dhcpOptionsIds' - The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
--
-- 'dryRun', 'describeDhcpOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeDhcpOptions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeDhcpOptions_filters' - One or more filters.
--
-- -   @dhcp-options-id@ - The ID of a DHCP options set.
--
-- -   @key@ - The key for one of the options (for example, @domain-name@).
--
-- -   @value@ - The value for one of the options.
--
-- -   @owner-id@ - The ID of the AWS account that owns the DHCP options
--     set.
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
newDescribeDhcpOptions ::
  DescribeDhcpOptions
newDescribeDhcpOptions =
  DescribeDhcpOptions'
    { nextToken = Prelude.Nothing,
      dhcpOptionsIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeDhcpOptions_nextToken :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe Prelude.Text)
describeDhcpOptions_nextToken = Lens.lens (\DescribeDhcpOptions' {nextToken} -> nextToken) (\s@DescribeDhcpOptions' {} a -> s {nextToken = a} :: DescribeDhcpOptions)

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
describeDhcpOptions_dhcpOptionsIds :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe [Prelude.Text])
describeDhcpOptions_dhcpOptionsIds = Lens.lens (\DescribeDhcpOptions' {dhcpOptionsIds} -> dhcpOptionsIds) (\s@DescribeDhcpOptions' {} a -> s {dhcpOptionsIds = a} :: DescribeDhcpOptions) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeDhcpOptions_dryRun :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe Prelude.Bool)
describeDhcpOptions_dryRun = Lens.lens (\DescribeDhcpOptions' {dryRun} -> dryRun) (\s@DescribeDhcpOptions' {} a -> s {dryRun = a} :: DescribeDhcpOptions)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeDhcpOptions_maxResults :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe Prelude.Natural)
describeDhcpOptions_maxResults = Lens.lens (\DescribeDhcpOptions' {maxResults} -> maxResults) (\s@DescribeDhcpOptions' {} a -> s {maxResults = a} :: DescribeDhcpOptions)

-- | One or more filters.
--
-- -   @dhcp-options-id@ - The ID of a DHCP options set.
--
-- -   @key@ - The key for one of the options (for example, @domain-name@).
--
-- -   @value@ - The value for one of the options.
--
-- -   @owner-id@ - The ID of the AWS account that owns the DHCP options
--     set.
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
describeDhcpOptions_filters :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe [Filter])
describeDhcpOptions_filters = Lens.lens (\DescribeDhcpOptions' {filters} -> filters) (\s@DescribeDhcpOptions' {} a -> s {filters = a} :: DescribeDhcpOptions) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeDhcpOptions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeDhcpOptionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeDhcpOptionsResponse_dhcpOptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeDhcpOptions_nextToken
          Lens..~ rs
          Lens.^? describeDhcpOptionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeDhcpOptions where
  type
    Rs DescribeDhcpOptions =
      DescribeDhcpOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeDhcpOptionsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "dhcpOptionsSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDhcpOptions

instance Prelude.NFData DescribeDhcpOptions

instance Prelude.ToHeaders DescribeDhcpOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeDhcpOptions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeDhcpOptions where
  toQuery DescribeDhcpOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeDhcpOptions" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        Prelude.toQuery
          ( Prelude.toQueryList "DhcpOptionsId"
              Prelude.<$> dhcpOptionsIds
          ),
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeDhcpOptionsResponse' smart constructor.
data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more DHCP options sets.
    dhcpOptions :: Prelude.Maybe [DhcpOptions],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeDhcpOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDhcpOptionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'dhcpOptions', 'describeDhcpOptionsResponse_dhcpOptions' - Information about one or more DHCP options sets.
--
-- 'httpStatus', 'describeDhcpOptionsResponse_httpStatus' - The response's http status code.
newDescribeDhcpOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDhcpOptionsResponse
newDescribeDhcpOptionsResponse pHttpStatus_ =
  DescribeDhcpOptionsResponse'
    { nextToken =
        Prelude.Nothing,
      dhcpOptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeDhcpOptionsResponse_nextToken :: Lens.Lens' DescribeDhcpOptionsResponse (Prelude.Maybe Prelude.Text)
describeDhcpOptionsResponse_nextToken = Lens.lens (\DescribeDhcpOptionsResponse' {nextToken} -> nextToken) (\s@DescribeDhcpOptionsResponse' {} a -> s {nextToken = a} :: DescribeDhcpOptionsResponse)

-- | Information about one or more DHCP options sets.
describeDhcpOptionsResponse_dhcpOptions :: Lens.Lens' DescribeDhcpOptionsResponse (Prelude.Maybe [DhcpOptions])
describeDhcpOptionsResponse_dhcpOptions = Lens.lens (\DescribeDhcpOptionsResponse' {dhcpOptions} -> dhcpOptions) (\s@DescribeDhcpOptionsResponse' {} a -> s {dhcpOptions = a} :: DescribeDhcpOptionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeDhcpOptionsResponse_httpStatus :: Lens.Lens' DescribeDhcpOptionsResponse Prelude.Int
describeDhcpOptionsResponse_httpStatus = Lens.lens (\DescribeDhcpOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeDhcpOptionsResponse' {} a -> s {httpStatus = a} :: DescribeDhcpOptionsResponse)

instance Prelude.NFData DescribeDhcpOptionsResponse
