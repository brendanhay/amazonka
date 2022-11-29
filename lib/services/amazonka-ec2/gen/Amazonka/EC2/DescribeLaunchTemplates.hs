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
-- Module      : Amazonka.EC2.DescribeLaunchTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch templates.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeLaunchTemplates
  ( -- * Creating a Request
    DescribeLaunchTemplates (..),
    newDescribeLaunchTemplates,

    -- * Request Lenses
    describeLaunchTemplates_nextToken,
    describeLaunchTemplates_launchTemplateNames,
    describeLaunchTemplates_filters,
    describeLaunchTemplates_dryRun,
    describeLaunchTemplates_maxResults,
    describeLaunchTemplates_launchTemplateIds,

    -- * Destructuring the Response
    DescribeLaunchTemplatesResponse (..),
    newDescribeLaunchTemplatesResponse,

    -- * Response Lenses
    describeLaunchTemplatesResponse_nextToken,
    describeLaunchTemplatesResponse_launchTemplates,
    describeLaunchTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLaunchTemplates' smart constructor.
data DescribeLaunchTemplates = DescribeLaunchTemplates'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more launch template names.
    launchTemplateNames :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters.
    --
    -- -   @create-time@ - The time the launch template was created.
    --
    -- -   @launch-template-name@ - The name of the launch template.
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
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 1 and 200.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more launch template IDs.
    launchTemplateIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLaunchTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLaunchTemplates_nextToken' - The token to request the next page of results.
--
-- 'launchTemplateNames', 'describeLaunchTemplates_launchTemplateNames' - One or more launch template names.
--
-- 'filters', 'describeLaunchTemplates_filters' - One or more filters.
--
-- -   @create-time@ - The time the launch template was created.
--
-- -   @launch-template-name@ - The name of the launch template.
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
-- 'dryRun', 'describeLaunchTemplates_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLaunchTemplates_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 1 and 200.
--
-- 'launchTemplateIds', 'describeLaunchTemplates_launchTemplateIds' - One or more launch template IDs.
newDescribeLaunchTemplates ::
  DescribeLaunchTemplates
newDescribeLaunchTemplates =
  DescribeLaunchTemplates'
    { nextToken =
        Prelude.Nothing,
      launchTemplateNames = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      launchTemplateIds = Prelude.Nothing
    }

-- | The token to request the next page of results.
describeLaunchTemplates_nextToken :: Lens.Lens' DescribeLaunchTemplates (Prelude.Maybe Prelude.Text)
describeLaunchTemplates_nextToken = Lens.lens (\DescribeLaunchTemplates' {nextToken} -> nextToken) (\s@DescribeLaunchTemplates' {} a -> s {nextToken = a} :: DescribeLaunchTemplates)

-- | One or more launch template names.
describeLaunchTemplates_launchTemplateNames :: Lens.Lens' DescribeLaunchTemplates (Prelude.Maybe [Prelude.Text])
describeLaunchTemplates_launchTemplateNames = Lens.lens (\DescribeLaunchTemplates' {launchTemplateNames} -> launchTemplateNames) (\s@DescribeLaunchTemplates' {} a -> s {launchTemplateNames = a} :: DescribeLaunchTemplates) Prelude.. Lens.mapping Lens.coerced

-- | One or more filters.
--
-- -   @create-time@ - The time the launch template was created.
--
-- -   @launch-template-name@ - The name of the launch template.
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
describeLaunchTemplates_filters :: Lens.Lens' DescribeLaunchTemplates (Prelude.Maybe [Filter])
describeLaunchTemplates_filters = Lens.lens (\DescribeLaunchTemplates' {filters} -> filters) (\s@DescribeLaunchTemplates' {} a -> s {filters = a} :: DescribeLaunchTemplates) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLaunchTemplates_dryRun :: Lens.Lens' DescribeLaunchTemplates (Prelude.Maybe Prelude.Bool)
describeLaunchTemplates_dryRun = Lens.lens (\DescribeLaunchTemplates' {dryRun} -> dryRun) (\s@DescribeLaunchTemplates' {} a -> s {dryRun = a} :: DescribeLaunchTemplates)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 1 and 200.
describeLaunchTemplates_maxResults :: Lens.Lens' DescribeLaunchTemplates (Prelude.Maybe Prelude.Natural)
describeLaunchTemplates_maxResults = Lens.lens (\DescribeLaunchTemplates' {maxResults} -> maxResults) (\s@DescribeLaunchTemplates' {} a -> s {maxResults = a} :: DescribeLaunchTemplates)

-- | One or more launch template IDs.
describeLaunchTemplates_launchTemplateIds :: Lens.Lens' DescribeLaunchTemplates (Prelude.Maybe [Prelude.Text])
describeLaunchTemplates_launchTemplateIds = Lens.lens (\DescribeLaunchTemplates' {launchTemplateIds} -> launchTemplateIds) (\s@DescribeLaunchTemplates' {} a -> s {launchTemplateIds = a} :: DescribeLaunchTemplates) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeLaunchTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLaunchTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLaunchTemplatesResponse_launchTemplates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeLaunchTemplates_nextToken
          Lens..~ rs
          Lens.^? describeLaunchTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeLaunchTemplates where
  type
    AWSResponse DescribeLaunchTemplates =
      DescribeLaunchTemplatesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLaunchTemplatesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "launchTemplates" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLaunchTemplates where
  hashWithSalt _salt DescribeLaunchTemplates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` launchTemplateNames
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` launchTemplateIds

instance Prelude.NFData DescribeLaunchTemplates where
  rnf DescribeLaunchTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf launchTemplateNames
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf launchTemplateIds

instance Core.ToHeaders DescribeLaunchTemplates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLaunchTemplates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLaunchTemplates where
  toQuery DescribeLaunchTemplates' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeLaunchTemplates" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "LaunchTemplateName"
              Prelude.<$> launchTemplateNames
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "LaunchTemplateId"
              Prelude.<$> launchTemplateIds
          )
      ]

-- | /See:/ 'newDescribeLaunchTemplatesResponse' smart constructor.
data DescribeLaunchTemplatesResponse = DescribeLaunchTemplatesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the launch templates.
    launchTemplates :: Prelude.Maybe [LaunchTemplate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLaunchTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLaunchTemplatesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'launchTemplates', 'describeLaunchTemplatesResponse_launchTemplates' - Information about the launch templates.
--
-- 'httpStatus', 'describeLaunchTemplatesResponse_httpStatus' - The response's http status code.
newDescribeLaunchTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLaunchTemplatesResponse
newDescribeLaunchTemplatesResponse pHttpStatus_ =
  DescribeLaunchTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      launchTemplates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLaunchTemplatesResponse_nextToken :: Lens.Lens' DescribeLaunchTemplatesResponse (Prelude.Maybe Prelude.Text)
describeLaunchTemplatesResponse_nextToken = Lens.lens (\DescribeLaunchTemplatesResponse' {nextToken} -> nextToken) (\s@DescribeLaunchTemplatesResponse' {} a -> s {nextToken = a} :: DescribeLaunchTemplatesResponse)

-- | Information about the launch templates.
describeLaunchTemplatesResponse_launchTemplates :: Lens.Lens' DescribeLaunchTemplatesResponse (Prelude.Maybe [LaunchTemplate])
describeLaunchTemplatesResponse_launchTemplates = Lens.lens (\DescribeLaunchTemplatesResponse' {launchTemplates} -> launchTemplates) (\s@DescribeLaunchTemplatesResponse' {} a -> s {launchTemplates = a} :: DescribeLaunchTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLaunchTemplatesResponse_httpStatus :: Lens.Lens' DescribeLaunchTemplatesResponse Prelude.Int
describeLaunchTemplatesResponse_httpStatus = Lens.lens (\DescribeLaunchTemplatesResponse' {httpStatus} -> httpStatus) (\s@DescribeLaunchTemplatesResponse' {} a -> s {httpStatus = a} :: DescribeLaunchTemplatesResponse)

instance
  Prelude.NFData
    DescribeLaunchTemplatesResponse
  where
  rnf DescribeLaunchTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf launchTemplates
      `Prelude.seq` Prelude.rnf httpStatus
