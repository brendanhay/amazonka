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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the maintenance window targets or tasks that
-- an instance is associated with.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
  ( -- * Creating a Request
    DescribeMaintenanceWindowsForTarget (..),
    newDescribeMaintenanceWindowsForTarget,

    -- * Request Lenses
    describeMaintenanceWindowsForTarget_nextToken,
    describeMaintenanceWindowsForTarget_maxResults,
    describeMaintenanceWindowsForTarget_targets,
    describeMaintenanceWindowsForTarget_resourceType,

    -- * Destructuring the Response
    DescribeMaintenanceWindowsForTargetResponse (..),
    newDescribeMaintenanceWindowsForTargetResponse,

    -- * Response Lenses
    describeMaintenanceWindowsForTargetResponse_nextToken,
    describeMaintenanceWindowsForTargetResponse_windowIdentities,
    describeMaintenanceWindowsForTargetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowsForTarget' smart constructor.
data DescribeMaintenanceWindowsForTarget = DescribeMaintenanceWindowsForTarget'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The instance ID or key\/value pair to retrieve information about.
    targets :: [Target],
    -- | The type of resource you want to retrieve information about. For
    -- example, \"INSTANCE\".
    resourceType :: MaintenanceWindowResourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowsForTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowsForTarget_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeMaintenanceWindowsForTarget_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'targets', 'describeMaintenanceWindowsForTarget_targets' - The instance ID or key\/value pair to retrieve information about.
--
-- 'resourceType', 'describeMaintenanceWindowsForTarget_resourceType' - The type of resource you want to retrieve information about. For
-- example, \"INSTANCE\".
newDescribeMaintenanceWindowsForTarget ::
  -- | 'resourceType'
  MaintenanceWindowResourceType ->
  DescribeMaintenanceWindowsForTarget
newDescribeMaintenanceWindowsForTarget pResourceType_ =
  DescribeMaintenanceWindowsForTarget'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      targets = Core.mempty,
      resourceType = pResourceType_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowsForTarget_nextToken :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Core.Maybe Core.Text)
describeMaintenanceWindowsForTarget_nextToken = Lens.lens (\DescribeMaintenanceWindowsForTarget' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowsForTarget' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowsForTarget)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowsForTarget_maxResults :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Core.Maybe Core.Natural)
describeMaintenanceWindowsForTarget_maxResults = Lens.lens (\DescribeMaintenanceWindowsForTarget' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowsForTarget' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowsForTarget)

-- | The instance ID or key\/value pair to retrieve information about.
describeMaintenanceWindowsForTarget_targets :: Lens.Lens' DescribeMaintenanceWindowsForTarget [Target]
describeMaintenanceWindowsForTarget_targets = Lens.lens (\DescribeMaintenanceWindowsForTarget' {targets} -> targets) (\s@DescribeMaintenanceWindowsForTarget' {} a -> s {targets = a} :: DescribeMaintenanceWindowsForTarget) Core.. Lens._Coerce

-- | The type of resource you want to retrieve information about. For
-- example, \"INSTANCE\".
describeMaintenanceWindowsForTarget_resourceType :: Lens.Lens' DescribeMaintenanceWindowsForTarget MaintenanceWindowResourceType
describeMaintenanceWindowsForTarget_resourceType = Lens.lens (\DescribeMaintenanceWindowsForTarget' {resourceType} -> resourceType) (\s@DescribeMaintenanceWindowsForTarget' {} a -> s {resourceType = a} :: DescribeMaintenanceWindowsForTarget)

instance
  Core.AWSPager
    DescribeMaintenanceWindowsForTarget
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowsForTargetResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowsForTargetResponse_windowIdentities
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMaintenanceWindowsForTarget_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowsForTargetResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowsForTarget
  where
  type
    AWSResponse DescribeMaintenanceWindowsForTarget =
      DescribeMaintenanceWindowsForTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsForTargetResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "WindowIdentities" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeMaintenanceWindowsForTarget

instance
  Core.NFData
    DescribeMaintenanceWindowsForTarget

instance
  Core.ToHeaders
    DescribeMaintenanceWindowsForTarget
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowsForTarget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeMaintenanceWindowsForTarget
  where
  toJSON DescribeMaintenanceWindowsForTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("Targets" Core..= targets),
            Core.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance
  Core.ToPath
    DescribeMaintenanceWindowsForTarget
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowsForTarget
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMaintenanceWindowsForTargetResponse' smart constructor.
data DescribeMaintenanceWindowsForTargetResponse = DescribeMaintenanceWindowsForTargetResponse'
  { -- | The token for the next set of items to return. (You use this token in
    -- the next call.)
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the maintenance window targets and tasks an instance
    -- is associated with.
    windowIdentities :: Core.Maybe [MaintenanceWindowIdentityForTarget],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowsForTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowsForTargetResponse_nextToken' - The token for the next set of items to return. (You use this token in
-- the next call.)
--
-- 'windowIdentities', 'describeMaintenanceWindowsForTargetResponse_windowIdentities' - Information about the maintenance window targets and tasks an instance
-- is associated with.
--
-- 'httpStatus', 'describeMaintenanceWindowsForTargetResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowsForTargetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMaintenanceWindowsForTargetResponse
newDescribeMaintenanceWindowsForTargetResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowsForTargetResponse'
      { nextToken =
          Core.Nothing,
        windowIdentities =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of items to return. (You use this token in
-- the next call.)
describeMaintenanceWindowsForTargetResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Core.Maybe Core.Text)
describeMaintenanceWindowsForTargetResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowsForTargetResponse)

-- | Information about the maintenance window targets and tasks an instance
-- is associated with.
describeMaintenanceWindowsForTargetResponse_windowIdentities :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Core.Maybe [MaintenanceWindowIdentityForTarget])
describeMaintenanceWindowsForTargetResponse_windowIdentities = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {windowIdentities} -> windowIdentities) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsForTargetResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowsForTargetResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse Core.Int
describeMaintenanceWindowsForTargetResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowsForTargetResponse)

instance
  Core.NFData
    DescribeMaintenanceWindowsForTargetResponse
