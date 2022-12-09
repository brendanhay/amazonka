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
-- Module      : Amazonka.EC2.DescribeDhcpOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your DHCP options sets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP options sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeDhcpOptions
  ( -- * Creating a Request
    DescribeDhcpOptions (..),
    newDescribeDhcpOptions,

    -- * Request Lenses
    describeDhcpOptions_dhcpOptionsIds,
    describeDhcpOptions_dryRun,
    describeDhcpOptions_filters,
    describeDhcpOptions_maxResults,
    describeDhcpOptions_nextToken,

    -- * Destructuring the Response
    DescribeDhcpOptionsResponse (..),
    newDescribeDhcpOptionsResponse,

    -- * Response Lenses
    describeDhcpOptionsResponse_dhcpOptions,
    describeDhcpOptionsResponse_nextToken,
    describeDhcpOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDhcpOptions' smart constructor.
data DescribeDhcpOptions = DescribeDhcpOptions'
  { -- | The IDs of one or more DHCP options sets.
    --
    -- Default: Describes all your DHCP options sets.
    dhcpOptionsIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @dhcp-options-id@ - The ID of a DHCP options set.
    --
    -- -   @key@ - The key for one of the options (for example, @domain-name@).
    --
    -- -   @value@ - The value for one of the options.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     DHCP options set.
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
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDhcpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'filters', 'describeDhcpOptions_filters' - One or more filters.
--
-- -   @dhcp-options-id@ - The ID of a DHCP options set.
--
-- -   @key@ - The key for one of the options (for example, @domain-name@).
--
-- -   @value@ - The value for one of the options.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     DHCP options set.
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
-- 'maxResults', 'describeDhcpOptions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeDhcpOptions_nextToken' - The token for the next page of results.
newDescribeDhcpOptions ::
  DescribeDhcpOptions
newDescribeDhcpOptions =
  DescribeDhcpOptions'
    { dhcpOptionsIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
describeDhcpOptions_dhcpOptionsIds :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe [Prelude.Text])
describeDhcpOptions_dhcpOptionsIds = Lens.lens (\DescribeDhcpOptions' {dhcpOptionsIds} -> dhcpOptionsIds) (\s@DescribeDhcpOptions' {} a -> s {dhcpOptionsIds = a} :: DescribeDhcpOptions) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeDhcpOptions_dryRun :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe Prelude.Bool)
describeDhcpOptions_dryRun = Lens.lens (\DescribeDhcpOptions' {dryRun} -> dryRun) (\s@DescribeDhcpOptions' {} a -> s {dryRun = a} :: DescribeDhcpOptions)

-- | One or more filters.
--
-- -   @dhcp-options-id@ - The ID of a DHCP options set.
--
-- -   @key@ - The key for one of the options (for example, @domain-name@).
--
-- -   @value@ - The value for one of the options.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     DHCP options set.
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
describeDhcpOptions_filters = Lens.lens (\DescribeDhcpOptions' {filters} -> filters) (\s@DescribeDhcpOptions' {} a -> s {filters = a} :: DescribeDhcpOptions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeDhcpOptions_maxResults :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe Prelude.Natural)
describeDhcpOptions_maxResults = Lens.lens (\DescribeDhcpOptions' {maxResults} -> maxResults) (\s@DescribeDhcpOptions' {} a -> s {maxResults = a} :: DescribeDhcpOptions)

-- | The token for the next page of results.
describeDhcpOptions_nextToken :: Lens.Lens' DescribeDhcpOptions (Prelude.Maybe Prelude.Text)
describeDhcpOptions_nextToken = Lens.lens (\DescribeDhcpOptions' {nextToken} -> nextToken) (\s@DescribeDhcpOptions' {} a -> s {nextToken = a} :: DescribeDhcpOptions)

instance Core.AWSPager DescribeDhcpOptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDhcpOptionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDhcpOptionsResponse_dhcpOptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDhcpOptions_nextToken
          Lens..~ rs
          Lens.^? describeDhcpOptionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDhcpOptions where
  type
    AWSResponse DescribeDhcpOptions =
      DescribeDhcpOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeDhcpOptionsResponse'
            Prelude.<$> ( x Data..@? "dhcpOptionsSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDhcpOptions where
  hashWithSalt _salt DescribeDhcpOptions' {..} =
    _salt `Prelude.hashWithSalt` dhcpOptionsIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeDhcpOptions where
  rnf DescribeDhcpOptions' {..} =
    Prelude.rnf dhcpOptionsIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeDhcpOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDhcpOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDhcpOptions where
  toQuery DescribeDhcpOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDhcpOptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "DhcpOptionsId"
              Prelude.<$> dhcpOptionsIds
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeDhcpOptionsResponse' smart constructor.
data DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse'
  { -- | Information about one or more DHCP options sets.
    dhcpOptions :: Prelude.Maybe [DhcpOptions],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDhcpOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dhcpOptions', 'describeDhcpOptionsResponse_dhcpOptions' - Information about one or more DHCP options sets.
--
-- 'nextToken', 'describeDhcpOptionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeDhcpOptionsResponse_httpStatus' - The response's http status code.
newDescribeDhcpOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDhcpOptionsResponse
newDescribeDhcpOptionsResponse pHttpStatus_ =
  DescribeDhcpOptionsResponse'
    { dhcpOptions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more DHCP options sets.
describeDhcpOptionsResponse_dhcpOptions :: Lens.Lens' DescribeDhcpOptionsResponse (Prelude.Maybe [DhcpOptions])
describeDhcpOptionsResponse_dhcpOptions = Lens.lens (\DescribeDhcpOptionsResponse' {dhcpOptions} -> dhcpOptions) (\s@DescribeDhcpOptionsResponse' {} a -> s {dhcpOptions = a} :: DescribeDhcpOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeDhcpOptionsResponse_nextToken :: Lens.Lens' DescribeDhcpOptionsResponse (Prelude.Maybe Prelude.Text)
describeDhcpOptionsResponse_nextToken = Lens.lens (\DescribeDhcpOptionsResponse' {nextToken} -> nextToken) (\s@DescribeDhcpOptionsResponse' {} a -> s {nextToken = a} :: DescribeDhcpOptionsResponse)

-- | The response's http status code.
describeDhcpOptionsResponse_httpStatus :: Lens.Lens' DescribeDhcpOptionsResponse Prelude.Int
describeDhcpOptionsResponse_httpStatus = Lens.lens (\DescribeDhcpOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeDhcpOptionsResponse' {} a -> s {httpStatus = a} :: DescribeDhcpOptionsResponse)

instance Prelude.NFData DescribeDhcpOptionsResponse where
  rnf DescribeDhcpOptionsResponse' {..} =
    Prelude.rnf dhcpOptions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
