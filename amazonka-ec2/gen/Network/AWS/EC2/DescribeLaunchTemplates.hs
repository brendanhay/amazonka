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
-- Module      : Network.AWS.EC2.DescribeLaunchTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch templates.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLaunchTemplates
  ( -- * Creating a Request
    DescribeLaunchTemplates (..),
    newDescribeLaunchTemplates,

    -- * Request Lenses
    describeLaunchTemplates_nextToken,
    describeLaunchTemplates_launchTemplateNames,
    describeLaunchTemplates_dryRun,
    describeLaunchTemplates_maxResults,
    describeLaunchTemplates_launchTemplateIds,
    describeLaunchTemplates_filters,

    -- * Destructuring the Response
    DescribeLaunchTemplatesResponse (..),
    newDescribeLaunchTemplatesResponse,

    -- * Response Lenses
    describeLaunchTemplatesResponse_nextToken,
    describeLaunchTemplatesResponse_launchTemplates,
    describeLaunchTemplatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLaunchTemplates' smart constructor.
data DescribeLaunchTemplates = DescribeLaunchTemplates'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | One or more launch template names.
    launchTemplateNames :: Core.Maybe [Core.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 1 and 200.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more launch template IDs.
    launchTemplateIds :: Core.Maybe [Core.Text],
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newDescribeLaunchTemplates ::
  DescribeLaunchTemplates
newDescribeLaunchTemplates =
  DescribeLaunchTemplates'
    { nextToken = Core.Nothing,
      launchTemplateNames = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      launchTemplateIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to request the next page of results.
describeLaunchTemplates_nextToken :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Text)
describeLaunchTemplates_nextToken = Lens.lens (\DescribeLaunchTemplates' {nextToken} -> nextToken) (\s@DescribeLaunchTemplates' {} a -> s {nextToken = a} :: DescribeLaunchTemplates)

-- | One or more launch template names.
describeLaunchTemplates_launchTemplateNames :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Core.Text])
describeLaunchTemplates_launchTemplateNames = Lens.lens (\DescribeLaunchTemplates' {launchTemplateNames} -> launchTemplateNames) (\s@DescribeLaunchTemplates' {} a -> s {launchTemplateNames = a} :: DescribeLaunchTemplates) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLaunchTemplates_dryRun :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Bool)
describeLaunchTemplates_dryRun = Lens.lens (\DescribeLaunchTemplates' {dryRun} -> dryRun) (\s@DescribeLaunchTemplates' {} a -> s {dryRun = a} :: DescribeLaunchTemplates)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 1 and 200.
describeLaunchTemplates_maxResults :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe Core.Natural)
describeLaunchTemplates_maxResults = Lens.lens (\DescribeLaunchTemplates' {maxResults} -> maxResults) (\s@DescribeLaunchTemplates' {} a -> s {maxResults = a} :: DescribeLaunchTemplates)

-- | One or more launch template IDs.
describeLaunchTemplates_launchTemplateIds :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Core.Text])
describeLaunchTemplates_launchTemplateIds = Lens.lens (\DescribeLaunchTemplates' {launchTemplateIds} -> launchTemplateIds) (\s@DescribeLaunchTemplates' {} a -> s {launchTemplateIds = a} :: DescribeLaunchTemplates) Core.. Lens.mapping Lens._Coerce

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
describeLaunchTemplates_filters :: Lens.Lens' DescribeLaunchTemplates (Core.Maybe [Filter])
describeLaunchTemplates_filters = Lens.lens (\DescribeLaunchTemplates' {filters} -> filters) (\s@DescribeLaunchTemplates' {} a -> s {filters = a} :: DescribeLaunchTemplates) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeLaunchTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLaunchTemplatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLaunchTemplatesResponse_launchTemplates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeLaunchTemplates_nextToken
          Lens..~ rs
          Lens.^? describeLaunchTemplatesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeLaunchTemplates where
  type
    AWSResponse DescribeLaunchTemplates =
      DescribeLaunchTemplatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLaunchTemplatesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "launchTemplates" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLaunchTemplates

instance Core.NFData DescribeLaunchTemplates

instance Core.ToHeaders DescribeLaunchTemplates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLaunchTemplates where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLaunchTemplates where
  toQuery DescribeLaunchTemplates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeLaunchTemplates" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "LaunchTemplateName"
              Core.<$> launchTemplateNames
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "LaunchTemplateId"
              Core.<$> launchTemplateIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeLaunchTemplatesResponse' smart constructor.
data DescribeLaunchTemplatesResponse = DescribeLaunchTemplatesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the launch templates.
    launchTemplates :: Core.Maybe [LaunchTemplate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeLaunchTemplatesResponse
newDescribeLaunchTemplatesResponse pHttpStatus_ =
  DescribeLaunchTemplatesResponse'
    { nextToken =
        Core.Nothing,
      launchTemplates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLaunchTemplatesResponse_nextToken :: Lens.Lens' DescribeLaunchTemplatesResponse (Core.Maybe Core.Text)
describeLaunchTemplatesResponse_nextToken = Lens.lens (\DescribeLaunchTemplatesResponse' {nextToken} -> nextToken) (\s@DescribeLaunchTemplatesResponse' {} a -> s {nextToken = a} :: DescribeLaunchTemplatesResponse)

-- | Information about the launch templates.
describeLaunchTemplatesResponse_launchTemplates :: Lens.Lens' DescribeLaunchTemplatesResponse (Core.Maybe [LaunchTemplate])
describeLaunchTemplatesResponse_launchTemplates = Lens.lens (\DescribeLaunchTemplatesResponse' {launchTemplates} -> launchTemplates) (\s@DescribeLaunchTemplatesResponse' {} a -> s {launchTemplates = a} :: DescribeLaunchTemplatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLaunchTemplatesResponse_httpStatus :: Lens.Lens' DescribeLaunchTemplatesResponse Core.Int
describeLaunchTemplatesResponse_httpStatus = Lens.lens (\DescribeLaunchTemplatesResponse' {httpStatus} -> httpStatus) (\s@DescribeLaunchTemplatesResponse' {} a -> s {httpStatus = a} :: DescribeLaunchTemplatesResponse)

instance Core.NFData DescribeLaunchTemplatesResponse
