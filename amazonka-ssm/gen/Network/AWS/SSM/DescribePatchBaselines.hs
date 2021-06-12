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
-- Module      : Network.AWS.SSM.DescribePatchBaselines
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the patch baselines in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchBaselines
  ( -- * Creating a Request
    DescribePatchBaselines (..),
    newDescribePatchBaselines,

    -- * Request Lenses
    describePatchBaselines_nextToken,
    describePatchBaselines_maxResults,
    describePatchBaselines_filters,

    -- * Destructuring the Response
    DescribePatchBaselinesResponse (..),
    newDescribePatchBaselinesResponse,

    -- * Response Lenses
    describePatchBaselinesResponse_nextToken,
    describePatchBaselinesResponse_baselineIdentities,
    describePatchBaselinesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribePatchBaselines' smart constructor.
data DescribePatchBaselines = DescribePatchBaselines'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of patch baselines to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | Each element in the array is a structure containing:
    --
    -- Key: (string, \"NAME_PREFIX\" or \"OWNER\")
    --
    -- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
    filters :: Core.Maybe [PatchOrchestratorFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePatchBaselines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchBaselines_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describePatchBaselines_maxResults' - The maximum number of patch baselines to return (per page).
--
-- 'filters', 'describePatchBaselines_filters' - Each element in the array is a structure containing:
--
-- Key: (string, \"NAME_PREFIX\" or \"OWNER\")
--
-- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
newDescribePatchBaselines ::
  DescribePatchBaselines
newDescribePatchBaselines =
  DescribePatchBaselines'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describePatchBaselines_nextToken :: Lens.Lens' DescribePatchBaselines (Core.Maybe Core.Text)
describePatchBaselines_nextToken = Lens.lens (\DescribePatchBaselines' {nextToken} -> nextToken) (\s@DescribePatchBaselines' {} a -> s {nextToken = a} :: DescribePatchBaselines)

-- | The maximum number of patch baselines to return (per page).
describePatchBaselines_maxResults :: Lens.Lens' DescribePatchBaselines (Core.Maybe Core.Natural)
describePatchBaselines_maxResults = Lens.lens (\DescribePatchBaselines' {maxResults} -> maxResults) (\s@DescribePatchBaselines' {} a -> s {maxResults = a} :: DescribePatchBaselines)

-- | Each element in the array is a structure containing:
--
-- Key: (string, \"NAME_PREFIX\" or \"OWNER\")
--
-- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
describePatchBaselines_filters :: Lens.Lens' DescribePatchBaselines (Core.Maybe [PatchOrchestratorFilter])
describePatchBaselines_filters = Lens.lens (\DescribePatchBaselines' {filters} -> filters) (\s@DescribePatchBaselines' {} a -> s {filters = a} :: DescribePatchBaselines) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribePatchBaselines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePatchBaselinesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePatchBaselinesResponse_baselineIdentities
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePatchBaselines_nextToken
          Lens..~ rs
          Lens.^? describePatchBaselinesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribePatchBaselines where
  type
    AWSResponse DescribePatchBaselines =
      DescribePatchBaselinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchBaselinesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "BaselineIdentities"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePatchBaselines

instance Core.NFData DescribePatchBaselines

instance Core.ToHeaders DescribePatchBaselines where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribePatchBaselines" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePatchBaselines where
  toJSON DescribePatchBaselines' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribePatchBaselines where
  toPath = Core.const "/"

instance Core.ToQuery DescribePatchBaselines where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePatchBaselinesResponse' smart constructor.
data DescribePatchBaselinesResponse = DescribePatchBaselinesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of PatchBaselineIdentity elements.
    baselineIdentities :: Core.Maybe [PatchBaselineIdentity],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePatchBaselinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePatchBaselinesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'baselineIdentities', 'describePatchBaselinesResponse_baselineIdentities' - An array of PatchBaselineIdentity elements.
--
-- 'httpStatus', 'describePatchBaselinesResponse_httpStatus' - The response's http status code.
newDescribePatchBaselinesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePatchBaselinesResponse
newDescribePatchBaselinesResponse pHttpStatus_ =
  DescribePatchBaselinesResponse'
    { nextToken =
        Core.Nothing,
      baselineIdentities = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describePatchBaselinesResponse_nextToken :: Lens.Lens' DescribePatchBaselinesResponse (Core.Maybe Core.Text)
describePatchBaselinesResponse_nextToken = Lens.lens (\DescribePatchBaselinesResponse' {nextToken} -> nextToken) (\s@DescribePatchBaselinesResponse' {} a -> s {nextToken = a} :: DescribePatchBaselinesResponse)

-- | An array of PatchBaselineIdentity elements.
describePatchBaselinesResponse_baselineIdentities :: Lens.Lens' DescribePatchBaselinesResponse (Core.Maybe [PatchBaselineIdentity])
describePatchBaselinesResponse_baselineIdentities = Lens.lens (\DescribePatchBaselinesResponse' {baselineIdentities} -> baselineIdentities) (\s@DescribePatchBaselinesResponse' {} a -> s {baselineIdentities = a} :: DescribePatchBaselinesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePatchBaselinesResponse_httpStatus :: Lens.Lens' DescribePatchBaselinesResponse Core.Int
describePatchBaselinesResponse_httpStatus = Lens.lens (\DescribePatchBaselinesResponse' {httpStatus} -> httpStatus) (\s@DescribePatchBaselinesResponse' {} a -> s {httpStatus = a} :: DescribePatchBaselinesResponse)

instance Core.NFData DescribePatchBaselinesResponse
