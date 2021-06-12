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
-- Module      : Network.AWS.SSM.DescribeAvailablePatches
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patches eligible to be included in a patch baseline.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAvailablePatches
  ( -- * Creating a Request
    DescribeAvailablePatches (..),
    newDescribeAvailablePatches,

    -- * Request Lenses
    describeAvailablePatches_nextToken,
    describeAvailablePatches_maxResults,
    describeAvailablePatches_filters,

    -- * Destructuring the Response
    DescribeAvailablePatchesResponse (..),
    newDescribeAvailablePatchesResponse,

    -- * Response Lenses
    describeAvailablePatchesResponse_nextToken,
    describeAvailablePatchesResponse_patches,
    describeAvailablePatchesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAvailablePatches' smart constructor.
data DescribeAvailablePatches = DescribeAvailablePatches'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters used to scope down the returned patches.
    filters :: Core.Maybe [PatchOrchestratorFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAvailablePatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAvailablePatches_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeAvailablePatches_maxResults' - The maximum number of patches to return (per page).
--
-- 'filters', 'describeAvailablePatches_filters' - Filters used to scope down the returned patches.
newDescribeAvailablePatches ::
  DescribeAvailablePatches
newDescribeAvailablePatches =
  DescribeAvailablePatches'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAvailablePatches_nextToken :: Lens.Lens' DescribeAvailablePatches (Core.Maybe Core.Text)
describeAvailablePatches_nextToken = Lens.lens (\DescribeAvailablePatches' {nextToken} -> nextToken) (\s@DescribeAvailablePatches' {} a -> s {nextToken = a} :: DescribeAvailablePatches)

-- | The maximum number of patches to return (per page).
describeAvailablePatches_maxResults :: Lens.Lens' DescribeAvailablePatches (Core.Maybe Core.Natural)
describeAvailablePatches_maxResults = Lens.lens (\DescribeAvailablePatches' {maxResults} -> maxResults) (\s@DescribeAvailablePatches' {} a -> s {maxResults = a} :: DescribeAvailablePatches)

-- | Filters used to scope down the returned patches.
describeAvailablePatches_filters :: Lens.Lens' DescribeAvailablePatches (Core.Maybe [PatchOrchestratorFilter])
describeAvailablePatches_filters = Lens.lens (\DescribeAvailablePatches' {filters} -> filters) (\s@DescribeAvailablePatches' {} a -> s {filters = a} :: DescribeAvailablePatches) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeAvailablePatches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAvailablePatchesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAvailablePatchesResponse_patches
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAvailablePatches_nextToken
          Lens..~ rs
          Lens.^? describeAvailablePatchesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeAvailablePatches where
  type
    AWSResponse DescribeAvailablePatches =
      DescribeAvailablePatchesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAvailablePatchesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Patches" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAvailablePatches

instance Core.NFData DescribeAvailablePatches

instance Core.ToHeaders DescribeAvailablePatches where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeAvailablePatches" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAvailablePatches where
  toJSON DescribeAvailablePatches' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribeAvailablePatches where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAvailablePatches where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAvailablePatchesResponse' smart constructor.
data DescribeAvailablePatchesResponse = DescribeAvailablePatchesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of patches. Each entry in the array is a patch structure.
    patches :: Core.Maybe [Patch],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAvailablePatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAvailablePatchesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'patches', 'describeAvailablePatchesResponse_patches' - An array of patches. Each entry in the array is a patch structure.
--
-- 'httpStatus', 'describeAvailablePatchesResponse_httpStatus' - The response's http status code.
newDescribeAvailablePatchesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAvailablePatchesResponse
newDescribeAvailablePatchesResponse pHttpStatus_ =
  DescribeAvailablePatchesResponse'
    { nextToken =
        Core.Nothing,
      patches = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeAvailablePatchesResponse_nextToken :: Lens.Lens' DescribeAvailablePatchesResponse (Core.Maybe Core.Text)
describeAvailablePatchesResponse_nextToken = Lens.lens (\DescribeAvailablePatchesResponse' {nextToken} -> nextToken) (\s@DescribeAvailablePatchesResponse' {} a -> s {nextToken = a} :: DescribeAvailablePatchesResponse)

-- | An array of patches. Each entry in the array is a patch structure.
describeAvailablePatchesResponse_patches :: Lens.Lens' DescribeAvailablePatchesResponse (Core.Maybe [Patch])
describeAvailablePatchesResponse_patches = Lens.lens (\DescribeAvailablePatchesResponse' {patches} -> patches) (\s@DescribeAvailablePatchesResponse' {} a -> s {patches = a} :: DescribeAvailablePatchesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAvailablePatchesResponse_httpStatus :: Lens.Lens' DescribeAvailablePatchesResponse Core.Int
describeAvailablePatchesResponse_httpStatus = Lens.lens (\DescribeAvailablePatchesResponse' {httpStatus} -> httpStatus) (\s@DescribeAvailablePatchesResponse' {} a -> s {httpStatus = a} :: DescribeAvailablePatchesResponse)

instance Core.NFData DescribeAvailablePatchesResponse
