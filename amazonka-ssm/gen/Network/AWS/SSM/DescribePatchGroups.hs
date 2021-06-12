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
-- Module      : Network.AWS.SSM.DescribePatchGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patch groups that have been registered with patch baselines.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchGroups
  ( -- * Creating a Request
    DescribePatchGroups (..),
    newDescribePatchGroups,

    -- * Request Lenses
    describePatchGroups_nextToken,
    describePatchGroups_maxResults,
    describePatchGroups_filters,

    -- * Destructuring the Response
    DescribePatchGroupsResponse (..),
    newDescribePatchGroupsResponse,

    -- * Response Lenses
    describePatchGroupsResponse_mappings,
    describePatchGroupsResponse_nextToken,
    describePatchGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribePatchGroups' smart constructor.
data DescribePatchGroups = DescribePatchGroups'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of patch groups to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    --
    -- For @DescribePatchGroups@,valid filter keys include the following:
    --
    -- -   @NAME_PREFIX@: The name of the patch group. Wildcards (*) are
    --     accepted.
    --
    -- -   @OPERATING_SYSTEM@: The supported operating system type to return
    --     results for. For valid operating system values, see
    --     GetDefaultPatchBaselineRequest$OperatingSystem in
    --     CreatePatchBaseline.
    --
    --     Examples:
    --
    --     -   @--filters Key=NAME_PREFIX,Values=MyPatchGroup*@
    --
    --     -   @--filters Key=OPERATING_SYSTEM,Values=AMAZON_LINUX_2@
    filters :: Core.Maybe [PatchOrchestratorFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePatchGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describePatchGroups_maxResults' - The maximum number of patch groups to return (per page).
--
-- 'filters', 'describePatchGroups_filters' - One or more filters. Use a filter to return a more specific list of
-- results.
--
-- For @DescribePatchGroups@,valid filter keys include the following:
--
-- -   @NAME_PREFIX@: The name of the patch group. Wildcards (*) are
--     accepted.
--
-- -   @OPERATING_SYSTEM@: The supported operating system type to return
--     results for. For valid operating system values, see
--     GetDefaultPatchBaselineRequest$OperatingSystem in
--     CreatePatchBaseline.
--
--     Examples:
--
--     -   @--filters Key=NAME_PREFIX,Values=MyPatchGroup*@
--
--     -   @--filters Key=OPERATING_SYSTEM,Values=AMAZON_LINUX_2@
newDescribePatchGroups ::
  DescribePatchGroups
newDescribePatchGroups =
  DescribePatchGroups'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describePatchGroups_nextToken :: Lens.Lens' DescribePatchGroups (Core.Maybe Core.Text)
describePatchGroups_nextToken = Lens.lens (\DescribePatchGroups' {nextToken} -> nextToken) (\s@DescribePatchGroups' {} a -> s {nextToken = a} :: DescribePatchGroups)

-- | The maximum number of patch groups to return (per page).
describePatchGroups_maxResults :: Lens.Lens' DescribePatchGroups (Core.Maybe Core.Natural)
describePatchGroups_maxResults = Lens.lens (\DescribePatchGroups' {maxResults} -> maxResults) (\s@DescribePatchGroups' {} a -> s {maxResults = a} :: DescribePatchGroups)

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- For @DescribePatchGroups@,valid filter keys include the following:
--
-- -   @NAME_PREFIX@: The name of the patch group. Wildcards (*) are
--     accepted.
--
-- -   @OPERATING_SYSTEM@: The supported operating system type to return
--     results for. For valid operating system values, see
--     GetDefaultPatchBaselineRequest$OperatingSystem in
--     CreatePatchBaseline.
--
--     Examples:
--
--     -   @--filters Key=NAME_PREFIX,Values=MyPatchGroup*@
--
--     -   @--filters Key=OPERATING_SYSTEM,Values=AMAZON_LINUX_2@
describePatchGroups_filters :: Lens.Lens' DescribePatchGroups (Core.Maybe [PatchOrchestratorFilter])
describePatchGroups_filters = Lens.lens (\DescribePatchGroups' {filters} -> filters) (\s@DescribePatchGroups' {} a -> s {filters = a} :: DescribePatchGroups) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribePatchGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePatchGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePatchGroupsResponse_mappings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePatchGroups_nextToken
          Lens..~ rs
          Lens.^? describePatchGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribePatchGroups where
  type
    AWSResponse DescribePatchGroups =
      DescribePatchGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchGroupsResponse'
            Core.<$> (x Core..?> "Mappings" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePatchGroups

instance Core.NFData DescribePatchGroups

instance Core.ToHeaders DescribePatchGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DescribePatchGroups" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePatchGroups where
  toJSON DescribePatchGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribePatchGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribePatchGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePatchGroupsResponse' smart constructor.
data DescribePatchGroupsResponse = DescribePatchGroupsResponse'
  { -- | Each entry in the array contains:
    --
    -- PatchGroup: string (between 1 and 256 characters, Regex:
    -- ^([\\p{L}\\p{Z}\\p{N}_.:\/=+\\-\@]*)$)
    --
    -- PatchBaselineIdentity: A PatchBaselineIdentity element.
    mappings :: Core.Maybe [PatchGroupPatchBaselineMapping],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePatchGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mappings', 'describePatchGroupsResponse_mappings' - Each entry in the array contains:
--
-- PatchGroup: string (between 1 and 256 characters, Regex:
-- ^([\\p{L}\\p{Z}\\p{N}_.:\/=+\\-\@]*)$)
--
-- PatchBaselineIdentity: A PatchBaselineIdentity element.
--
-- 'nextToken', 'describePatchGroupsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'httpStatus', 'describePatchGroupsResponse_httpStatus' - The response's http status code.
newDescribePatchGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePatchGroupsResponse
newDescribePatchGroupsResponse pHttpStatus_ =
  DescribePatchGroupsResponse'
    { mappings =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Each entry in the array contains:
--
-- PatchGroup: string (between 1 and 256 characters, Regex:
-- ^([\\p{L}\\p{Z}\\p{N}_.:\/=+\\-\@]*)$)
--
-- PatchBaselineIdentity: A PatchBaselineIdentity element.
describePatchGroupsResponse_mappings :: Lens.Lens' DescribePatchGroupsResponse (Core.Maybe [PatchGroupPatchBaselineMapping])
describePatchGroupsResponse_mappings = Lens.lens (\DescribePatchGroupsResponse' {mappings} -> mappings) (\s@DescribePatchGroupsResponse' {} a -> s {mappings = a} :: DescribePatchGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describePatchGroupsResponse_nextToken :: Lens.Lens' DescribePatchGroupsResponse (Core.Maybe Core.Text)
describePatchGroupsResponse_nextToken = Lens.lens (\DescribePatchGroupsResponse' {nextToken} -> nextToken) (\s@DescribePatchGroupsResponse' {} a -> s {nextToken = a} :: DescribePatchGroupsResponse)

-- | The response's http status code.
describePatchGroupsResponse_httpStatus :: Lens.Lens' DescribePatchGroupsResponse Core.Int
describePatchGroupsResponse_httpStatus = Lens.lens (\DescribePatchGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribePatchGroupsResponse' {} a -> s {httpStatus = a} :: DescribePatchGroupsResponse)

instance Core.NFData DescribePatchGroupsResponse
