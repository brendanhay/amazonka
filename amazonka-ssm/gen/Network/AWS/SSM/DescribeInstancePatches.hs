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
-- Module      : Network.AWS.SSM.DescribeInstancePatches
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the patches on the specified instance and
-- their state relative to the patch baseline being used for the instance.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatches
  ( -- * Creating a Request
    DescribeInstancePatches (..),
    newDescribeInstancePatches,

    -- * Request Lenses
    describeInstancePatches_nextToken,
    describeInstancePatches_maxResults,
    describeInstancePatches_filters,
    describeInstancePatches_instanceId,

    -- * Destructuring the Response
    DescribeInstancePatchesResponse (..),
    newDescribeInstancePatchesResponse,

    -- * Response Lenses
    describeInstancePatchesResponse_nextToken,
    describeInstancePatchesResponse_patches,
    describeInstancePatchesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInstancePatches' smart constructor.
data DescribeInstancePatches = DescribeInstancePatches'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | An array of structures. Each entry in the array is a structure
    -- containing a Key, Value combination. Valid values for Key are
    -- @Classification@ | @KBId@ | @Severity@ | @State@.
    filters :: Core.Maybe [PatchOrchestratorFilter],
    -- | The ID of the instance whose patch state information should be
    -- retrieved.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancePatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatches_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeInstancePatches_maxResults' - The maximum number of patches to return (per page).
--
-- 'filters', 'describeInstancePatches_filters' - An array of structures. Each entry in the array is a structure
-- containing a Key, Value combination. Valid values for Key are
-- @Classification@ | @KBId@ | @Severity@ | @State@.
--
-- 'instanceId', 'describeInstancePatches_instanceId' - The ID of the instance whose patch state information should be
-- retrieved.
newDescribeInstancePatches ::
  -- | 'instanceId'
  Core.Text ->
  DescribeInstancePatches
newDescribeInstancePatches pInstanceId_ =
  DescribeInstancePatches'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatches_nextToken :: Lens.Lens' DescribeInstancePatches (Core.Maybe Core.Text)
describeInstancePatches_nextToken = Lens.lens (\DescribeInstancePatches' {nextToken} -> nextToken) (\s@DescribeInstancePatches' {} a -> s {nextToken = a} :: DescribeInstancePatches)

-- | The maximum number of patches to return (per page).
describeInstancePatches_maxResults :: Lens.Lens' DescribeInstancePatches (Core.Maybe Core.Natural)
describeInstancePatches_maxResults = Lens.lens (\DescribeInstancePatches' {maxResults} -> maxResults) (\s@DescribeInstancePatches' {} a -> s {maxResults = a} :: DescribeInstancePatches)

-- | An array of structures. Each entry in the array is a structure
-- containing a Key, Value combination. Valid values for Key are
-- @Classification@ | @KBId@ | @Severity@ | @State@.
describeInstancePatches_filters :: Lens.Lens' DescribeInstancePatches (Core.Maybe [PatchOrchestratorFilter])
describeInstancePatches_filters = Lens.lens (\DescribeInstancePatches' {filters} -> filters) (\s@DescribeInstancePatches' {} a -> s {filters = a} :: DescribeInstancePatches) Core.. Lens.mapping Lens._Coerce

-- | The ID of the instance whose patch state information should be
-- retrieved.
describeInstancePatches_instanceId :: Lens.Lens' DescribeInstancePatches Core.Text
describeInstancePatches_instanceId = Lens.lens (\DescribeInstancePatches' {instanceId} -> instanceId) (\s@DescribeInstancePatches' {} a -> s {instanceId = a} :: DescribeInstancePatches)

instance Core.AWSPager DescribeInstancePatches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchesResponse_patches
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeInstancePatches_nextToken
          Lens..~ rs
          Lens.^? describeInstancePatchesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeInstancePatches where
  type
    AWSResponse DescribeInstancePatches =
      DescribeInstancePatchesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Patches" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstancePatches

instance Core.NFData DescribeInstancePatches

instance Core.ToHeaders DescribeInstancePatches where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeInstancePatches" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeInstancePatches where
  toJSON DescribeInstancePatches' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath DescribeInstancePatches where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInstancePatches where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeInstancePatchesResponse' smart constructor.
data DescribeInstancePatchesResponse = DescribeInstancePatchesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Each entry in the array is a structure containing:
    --
    -- Title (string)
    --
    -- KBId (string)
    --
    -- Classification (string)
    --
    -- Severity (string)
    --
    -- State (string, such as \"INSTALLED\" or \"FAILED\")
    --
    -- InstalledTime (DateTime)
    --
    -- InstalledBy (string)
    patches :: Core.Maybe [PatchComplianceData],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancePatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'patches', 'describeInstancePatchesResponse_patches' - Each entry in the array is a structure containing:
--
-- Title (string)
--
-- KBId (string)
--
-- Classification (string)
--
-- Severity (string)
--
-- State (string, such as \"INSTALLED\" or \"FAILED\")
--
-- InstalledTime (DateTime)
--
-- InstalledBy (string)
--
-- 'httpStatus', 'describeInstancePatchesResponse_httpStatus' - The response's http status code.
newDescribeInstancePatchesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstancePatchesResponse
newDescribeInstancePatchesResponse pHttpStatus_ =
  DescribeInstancePatchesResponse'
    { nextToken =
        Core.Nothing,
      patches = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstancePatchesResponse_nextToken :: Lens.Lens' DescribeInstancePatchesResponse (Core.Maybe Core.Text)
describeInstancePatchesResponse_nextToken = Lens.lens (\DescribeInstancePatchesResponse' {nextToken} -> nextToken) (\s@DescribeInstancePatchesResponse' {} a -> s {nextToken = a} :: DescribeInstancePatchesResponse)

-- | Each entry in the array is a structure containing:
--
-- Title (string)
--
-- KBId (string)
--
-- Classification (string)
--
-- Severity (string)
--
-- State (string, such as \"INSTALLED\" or \"FAILED\")
--
-- InstalledTime (DateTime)
--
-- InstalledBy (string)
describeInstancePatchesResponse_patches :: Lens.Lens' DescribeInstancePatchesResponse (Core.Maybe [PatchComplianceData])
describeInstancePatchesResponse_patches = Lens.lens (\DescribeInstancePatchesResponse' {patches} -> patches) (\s@DescribeInstancePatchesResponse' {} a -> s {patches = a} :: DescribeInstancePatchesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstancePatchesResponse_httpStatus :: Lens.Lens' DescribeInstancePatchesResponse Core.Int
describeInstancePatchesResponse_httpStatus = Lens.lens (\DescribeInstancePatchesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchesResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchesResponse)

instance Core.NFData DescribeInstancePatchesResponse
