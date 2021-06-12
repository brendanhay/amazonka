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
-- Module      : Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current effective patches (the patch and the approval
-- state) for the specified patch baseline. Note that this API applies only
-- to Windows patch baselines.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
  ( -- * Creating a Request
    DescribeEffectivePatchesForPatchBaseline (..),
    newDescribeEffectivePatchesForPatchBaseline,

    -- * Request Lenses
    describeEffectivePatchesForPatchBaseline_nextToken,
    describeEffectivePatchesForPatchBaseline_maxResults,
    describeEffectivePatchesForPatchBaseline_baselineId,

    -- * Destructuring the Response
    DescribeEffectivePatchesForPatchBaselineResponse (..),
    newDescribeEffectivePatchesForPatchBaselineResponse,

    -- * Response Lenses
    describeEffectivePatchesForPatchBaselineResponse_nextToken,
    describeEffectivePatchesForPatchBaselineResponse_effectivePatches,
    describeEffectivePatchesForPatchBaselineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeEffectivePatchesForPatchBaseline' smart constructor.
data DescribeEffectivePatchesForPatchBaseline = DescribeEffectivePatchesForPatchBaseline'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the patch baseline to retrieve the effective patches for.
    baselineId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEffectivePatchesForPatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEffectivePatchesForPatchBaseline_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeEffectivePatchesForPatchBaseline_maxResults' - The maximum number of patches to return (per page).
--
-- 'baselineId', 'describeEffectivePatchesForPatchBaseline_baselineId' - The ID of the patch baseline to retrieve the effective patches for.
newDescribeEffectivePatchesForPatchBaseline ::
  -- | 'baselineId'
  Core.Text ->
  DescribeEffectivePatchesForPatchBaseline
newDescribeEffectivePatchesForPatchBaseline
  pBaselineId_ =
    DescribeEffectivePatchesForPatchBaseline'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        baselineId = pBaselineId_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeEffectivePatchesForPatchBaseline_nextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Core.Maybe Core.Text)
describeEffectivePatchesForPatchBaseline_nextToken = Lens.lens (\DescribeEffectivePatchesForPatchBaseline' {nextToken} -> nextToken) (\s@DescribeEffectivePatchesForPatchBaseline' {} a -> s {nextToken = a} :: DescribeEffectivePatchesForPatchBaseline)

-- | The maximum number of patches to return (per page).
describeEffectivePatchesForPatchBaseline_maxResults :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Core.Maybe Core.Natural)
describeEffectivePatchesForPatchBaseline_maxResults = Lens.lens (\DescribeEffectivePatchesForPatchBaseline' {maxResults} -> maxResults) (\s@DescribeEffectivePatchesForPatchBaseline' {} a -> s {maxResults = a} :: DescribeEffectivePatchesForPatchBaseline)

-- | The ID of the patch baseline to retrieve the effective patches for.
describeEffectivePatchesForPatchBaseline_baselineId :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline Core.Text
describeEffectivePatchesForPatchBaseline_baselineId = Lens.lens (\DescribeEffectivePatchesForPatchBaseline' {baselineId} -> baselineId) (\s@DescribeEffectivePatchesForPatchBaseline' {} a -> s {baselineId = a} :: DescribeEffectivePatchesForPatchBaseline)

instance
  Core.AWSPager
    DescribeEffectivePatchesForPatchBaseline
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEffectivePatchesForPatchBaselineResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEffectivePatchesForPatchBaselineResponse_effectivePatches
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEffectivePatchesForPatchBaseline_nextToken
          Lens..~ rs
            Lens.^? describeEffectivePatchesForPatchBaselineResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeEffectivePatchesForPatchBaseline
  where
  type
    AWSResponse
      DescribeEffectivePatchesForPatchBaseline =
      DescribeEffectivePatchesForPatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEffectivePatchesForPatchBaselineResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> (x Core..?> "EffectivePatches" Core..!@ Core.mempty)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeEffectivePatchesForPatchBaseline

instance
  Core.NFData
    DescribeEffectivePatchesForPatchBaseline

instance
  Core.ToHeaders
    DescribeEffectivePatchesForPatchBaseline
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeEffectivePatchesForPatchBaseline" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeEffectivePatchesForPatchBaseline
  where
  toJSON DescribeEffectivePatchesForPatchBaseline' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("BaselineId" Core..= baselineId)
          ]
      )

instance
  Core.ToPath
    DescribeEffectivePatchesForPatchBaseline
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeEffectivePatchesForPatchBaseline
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEffectivePatchesForPatchBaselineResponse' smart constructor.
data DescribeEffectivePatchesForPatchBaselineResponse = DescribeEffectivePatchesForPatchBaselineResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of patches and patch status.
    effectivePatches :: Core.Maybe [EffectivePatch],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEffectivePatchesForPatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEffectivePatchesForPatchBaselineResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'effectivePatches', 'describeEffectivePatchesForPatchBaselineResponse_effectivePatches' - An array of patches and patch status.
--
-- 'httpStatus', 'describeEffectivePatchesForPatchBaselineResponse_httpStatus' - The response's http status code.
newDescribeEffectivePatchesForPatchBaselineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEffectivePatchesForPatchBaselineResponse
newDescribeEffectivePatchesForPatchBaselineResponse
  pHttpStatus_ =
    DescribeEffectivePatchesForPatchBaselineResponse'
      { nextToken =
          Core.Nothing,
        effectivePatches =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeEffectivePatchesForPatchBaselineResponse_nextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Core.Maybe Core.Text)
describeEffectivePatchesForPatchBaselineResponse_nextToken = Lens.lens (\DescribeEffectivePatchesForPatchBaselineResponse' {nextToken} -> nextToken) (\s@DescribeEffectivePatchesForPatchBaselineResponse' {} a -> s {nextToken = a} :: DescribeEffectivePatchesForPatchBaselineResponse)

-- | An array of patches and patch status.
describeEffectivePatchesForPatchBaselineResponse_effectivePatches :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Core.Maybe [EffectivePatch])
describeEffectivePatchesForPatchBaselineResponse_effectivePatches = Lens.lens (\DescribeEffectivePatchesForPatchBaselineResponse' {effectivePatches} -> effectivePatches) (\s@DescribeEffectivePatchesForPatchBaselineResponse' {} a -> s {effectivePatches = a} :: DescribeEffectivePatchesForPatchBaselineResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEffectivePatchesForPatchBaselineResponse_httpStatus :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse Core.Int
describeEffectivePatchesForPatchBaselineResponse_httpStatus = Lens.lens (\DescribeEffectivePatchesForPatchBaselineResponse' {httpStatus} -> httpStatus) (\s@DescribeEffectivePatchesForPatchBaselineResponse' {} a -> s {httpStatus = a} :: DescribeEffectivePatchesForPatchBaselineResponse)

instance
  Core.NFData
    DescribeEffectivePatchesForPatchBaselineResponse
