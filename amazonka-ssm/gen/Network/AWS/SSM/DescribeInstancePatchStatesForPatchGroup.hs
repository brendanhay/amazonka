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
-- Module      : Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state for the instances in the specified
-- patch group.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
  ( -- * Creating a Request
    DescribeInstancePatchStatesForPatchGroup (..),
    newDescribeInstancePatchStatesForPatchGroup,

    -- * Request Lenses
    describeInstancePatchStatesForPatchGroup_nextToken,
    describeInstancePatchStatesForPatchGroup_maxResults,
    describeInstancePatchStatesForPatchGroup_filters,
    describeInstancePatchStatesForPatchGroup_patchGroup,

    -- * Destructuring the Response
    DescribeInstancePatchStatesForPatchGroupResponse (..),
    newDescribeInstancePatchStatesForPatchGroupResponse,

    -- * Response Lenses
    describeInstancePatchStatesForPatchGroupResponse_nextToken,
    describeInstancePatchStatesForPatchGroupResponse_instancePatchStates,
    describeInstancePatchStatesForPatchGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInstancePatchStatesForPatchGroup' smart constructor.
data DescribeInstancePatchStatesForPatchGroup = DescribeInstancePatchStatesForPatchGroup'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | Each entry in the array is a structure containing:
    --
    -- Key (string between 1 and 200 characters)
    --
    -- Values (array containing a single string)
    --
    -- Type (string \"Equal\", \"NotEqual\", \"LessThan\", \"GreaterThan\")
    filters :: Core.Maybe [InstancePatchStateFilter],
    -- | The name of the patch group for which the patch state information should
    -- be retrieved.
    patchGroup :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStatesForPatchGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchStatesForPatchGroup_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeInstancePatchStatesForPatchGroup_maxResults' - The maximum number of patches to return (per page).
--
-- 'filters', 'describeInstancePatchStatesForPatchGroup_filters' - Each entry in the array is a structure containing:
--
-- Key (string between 1 and 200 characters)
--
-- Values (array containing a single string)
--
-- Type (string \"Equal\", \"NotEqual\", \"LessThan\", \"GreaterThan\")
--
-- 'patchGroup', 'describeInstancePatchStatesForPatchGroup_patchGroup' - The name of the patch group for which the patch state information should
-- be retrieved.
newDescribeInstancePatchStatesForPatchGroup ::
  -- | 'patchGroup'
  Core.Text ->
  DescribeInstancePatchStatesForPatchGroup
newDescribeInstancePatchStatesForPatchGroup
  pPatchGroup_ =
    DescribeInstancePatchStatesForPatchGroup'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        filters = Core.Nothing,
        patchGroup = pPatchGroup_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatchStatesForPatchGroup_nextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Core.Maybe Core.Text)
describeInstancePatchStatesForPatchGroup_nextToken = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {nextToken} -> nextToken) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {nextToken = a} :: DescribeInstancePatchStatesForPatchGroup)

-- | The maximum number of patches to return (per page).
describeInstancePatchStatesForPatchGroup_maxResults :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Core.Maybe Core.Natural)
describeInstancePatchStatesForPatchGroup_maxResults = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {maxResults} -> maxResults) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {maxResults = a} :: DescribeInstancePatchStatesForPatchGroup)

-- | Each entry in the array is a structure containing:
--
-- Key (string between 1 and 200 characters)
--
-- Values (array containing a single string)
--
-- Type (string \"Equal\", \"NotEqual\", \"LessThan\", \"GreaterThan\")
describeInstancePatchStatesForPatchGroup_filters :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Core.Maybe [InstancePatchStateFilter])
describeInstancePatchStatesForPatchGroup_filters = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {filters} -> filters) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {filters = a} :: DescribeInstancePatchStatesForPatchGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the patch group for which the patch state information should
-- be retrieved.
describeInstancePatchStatesForPatchGroup_patchGroup :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup Core.Text
describeInstancePatchStatesForPatchGroup_patchGroup = Lens.lens (\DescribeInstancePatchStatesForPatchGroup' {patchGroup} -> patchGroup) (\s@DescribeInstancePatchStatesForPatchGroup' {} a -> s {patchGroup = a} :: DescribeInstancePatchStatesForPatchGroup)

instance
  Core.AWSPager
    DescribeInstancePatchStatesForPatchGroup
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesForPatchGroupResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesForPatchGroupResponse_instancePatchStates
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeInstancePatchStatesForPatchGroup_nextToken
          Lens..~ rs
            Lens.^? describeInstancePatchStatesForPatchGroupResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeInstancePatchStatesForPatchGroup
  where
  type
    AWSResponse
      DescribeInstancePatchStatesForPatchGroup =
      DescribeInstancePatchStatesForPatchGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesForPatchGroupResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> (x Core..?> "InstancePatchStates")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeInstancePatchStatesForPatchGroup

instance
  Core.NFData
    DescribeInstancePatchStatesForPatchGroup

instance
  Core.ToHeaders
    DescribeInstancePatchStatesForPatchGroup
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeInstancePatchStatesForPatchGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeInstancePatchStatesForPatchGroup
  where
  toJSON DescribeInstancePatchStatesForPatchGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("PatchGroup" Core..= patchGroup)
          ]
      )

instance
  Core.ToPath
    DescribeInstancePatchStatesForPatchGroup
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeInstancePatchStatesForPatchGroup
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeInstancePatchStatesForPatchGroupResponse' smart constructor.
data DescribeInstancePatchStatesForPatchGroupResponse = DescribeInstancePatchStatesForPatchGroupResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The high-level patch state for the requested instances.
    instancePatchStates :: Core.Maybe (Core.NonEmpty InstancePatchState),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStatesForPatchGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchStatesForPatchGroupResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'instancePatchStates', 'describeInstancePatchStatesForPatchGroupResponse_instancePatchStates' - The high-level patch state for the requested instances.
--
-- 'httpStatus', 'describeInstancePatchStatesForPatchGroupResponse_httpStatus' - The response's http status code.
newDescribeInstancePatchStatesForPatchGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstancePatchStatesForPatchGroupResponse
newDescribeInstancePatchStatesForPatchGroupResponse
  pHttpStatus_ =
    DescribeInstancePatchStatesForPatchGroupResponse'
      { nextToken =
          Core.Nothing,
        instancePatchStates =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstancePatchStatesForPatchGroupResponse_nextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Core.Maybe Core.Text)
describeInstancePatchStatesForPatchGroupResponse_nextToken = Lens.lens (\DescribeInstancePatchStatesForPatchGroupResponse' {nextToken} -> nextToken) (\s@DescribeInstancePatchStatesForPatchGroupResponse' {} a -> s {nextToken = a} :: DescribeInstancePatchStatesForPatchGroupResponse)

-- | The high-level patch state for the requested instances.
describeInstancePatchStatesForPatchGroupResponse_instancePatchStates :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Core.Maybe (Core.NonEmpty InstancePatchState))
describeInstancePatchStatesForPatchGroupResponse_instancePatchStates = Lens.lens (\DescribeInstancePatchStatesForPatchGroupResponse' {instancePatchStates} -> instancePatchStates) (\s@DescribeInstancePatchStatesForPatchGroupResponse' {} a -> s {instancePatchStates = a} :: DescribeInstancePatchStatesForPatchGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstancePatchStatesForPatchGroupResponse_httpStatus :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse Core.Int
describeInstancePatchStatesForPatchGroupResponse_httpStatus = Lens.lens (\DescribeInstancePatchStatesForPatchGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchStatesForPatchGroupResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchStatesForPatchGroupResponse)

instance
  Core.NFData
    DescribeInstancePatchStatesForPatchGroupResponse
