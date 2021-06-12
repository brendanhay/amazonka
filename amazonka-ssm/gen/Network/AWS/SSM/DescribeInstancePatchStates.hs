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
-- Module      : Network.AWS.SSM.DescribeInstancePatchStates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state of one or more instances.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatchStates
  ( -- * Creating a Request
    DescribeInstancePatchStates (..),
    newDescribeInstancePatchStates,

    -- * Request Lenses
    describeInstancePatchStates_nextToken,
    describeInstancePatchStates_maxResults,
    describeInstancePatchStates_instanceIds,

    -- * Destructuring the Response
    DescribeInstancePatchStatesResponse (..),
    newDescribeInstancePatchStatesResponse,

    -- * Response Lenses
    describeInstancePatchStatesResponse_nextToken,
    describeInstancePatchStatesResponse_instancePatchStates,
    describeInstancePatchStatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInstancePatchStates' smart constructor.
data DescribeInstancePatchStates = DescribeInstancePatchStates'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of instances to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the instance whose patch state information should be
    -- retrieved.
    instanceIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchStates_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeInstancePatchStates_maxResults' - The maximum number of instances to return (per page).
--
-- 'instanceIds', 'describeInstancePatchStates_instanceIds' - The ID of the instance whose patch state information should be
-- retrieved.
newDescribeInstancePatchStates ::
  DescribeInstancePatchStates
newDescribeInstancePatchStates =
  DescribeInstancePatchStates'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      instanceIds = Core.mempty
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeInstancePatchStates_nextToken :: Lens.Lens' DescribeInstancePatchStates (Core.Maybe Core.Text)
describeInstancePatchStates_nextToken = Lens.lens (\DescribeInstancePatchStates' {nextToken} -> nextToken) (\s@DescribeInstancePatchStates' {} a -> s {nextToken = a} :: DescribeInstancePatchStates)

-- | The maximum number of instances to return (per page).
describeInstancePatchStates_maxResults :: Lens.Lens' DescribeInstancePatchStates (Core.Maybe Core.Natural)
describeInstancePatchStates_maxResults = Lens.lens (\DescribeInstancePatchStates' {maxResults} -> maxResults) (\s@DescribeInstancePatchStates' {} a -> s {maxResults = a} :: DescribeInstancePatchStates)

-- | The ID of the instance whose patch state information should be
-- retrieved.
describeInstancePatchStates_instanceIds :: Lens.Lens' DescribeInstancePatchStates [Core.Text]
describeInstancePatchStates_instanceIds = Lens.lens (\DescribeInstancePatchStates' {instanceIds} -> instanceIds) (\s@DescribeInstancePatchStates' {} a -> s {instanceIds = a} :: DescribeInstancePatchStates) Core.. Lens._Coerce

instance Core.AWSPager DescribeInstancePatchStates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstancePatchStatesResponse_instancePatchStates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeInstancePatchStates_nextToken
          Lens..~ rs
          Lens.^? describeInstancePatchStatesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeInstancePatchStates where
  type
    AWSResponse DescribeInstancePatchStates =
      DescribeInstancePatchStatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "InstancePatchStates"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstancePatchStates

instance Core.NFData DescribeInstancePatchStates

instance Core.ToHeaders DescribeInstancePatchStates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeInstancePatchStates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeInstancePatchStates where
  toJSON DescribeInstancePatchStates' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("InstanceIds" Core..= instanceIds)
          ]
      )

instance Core.ToPath DescribeInstancePatchStates where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInstancePatchStates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeInstancePatchStatesResponse' smart constructor.
data DescribeInstancePatchStatesResponse = DescribeInstancePatchStatesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The high-level patch state for the requested instances.
    instancePatchStates :: Core.Maybe [InstancePatchState],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancePatchStatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancePatchStatesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'instancePatchStates', 'describeInstancePatchStatesResponse_instancePatchStates' - The high-level patch state for the requested instances.
--
-- 'httpStatus', 'describeInstancePatchStatesResponse_httpStatus' - The response's http status code.
newDescribeInstancePatchStatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstancePatchStatesResponse
newDescribeInstancePatchStatesResponse pHttpStatus_ =
  DescribeInstancePatchStatesResponse'
    { nextToken =
        Core.Nothing,
      instancePatchStates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeInstancePatchStatesResponse_nextToken :: Lens.Lens' DescribeInstancePatchStatesResponse (Core.Maybe Core.Text)
describeInstancePatchStatesResponse_nextToken = Lens.lens (\DescribeInstancePatchStatesResponse' {nextToken} -> nextToken) (\s@DescribeInstancePatchStatesResponse' {} a -> s {nextToken = a} :: DescribeInstancePatchStatesResponse)

-- | The high-level patch state for the requested instances.
describeInstancePatchStatesResponse_instancePatchStates :: Lens.Lens' DescribeInstancePatchStatesResponse (Core.Maybe [InstancePatchState])
describeInstancePatchStatesResponse_instancePatchStates = Lens.lens (\DescribeInstancePatchStatesResponse' {instancePatchStates} -> instancePatchStates) (\s@DescribeInstancePatchStatesResponse' {} a -> s {instancePatchStates = a} :: DescribeInstancePatchStatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstancePatchStatesResponse_httpStatus :: Lens.Lens' DescribeInstancePatchStatesResponse Core.Int
describeInstancePatchStatesResponse_httpStatus = Lens.lens (\DescribeInstancePatchStatesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancePatchStatesResponse' {} a -> s {httpStatus = a} :: DescribeInstancePatchStatesResponse)

instance
  Core.NFData
    DescribeInstancePatchStatesResponse
