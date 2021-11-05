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
-- Module      : Amazonka.SSM.DescribeMaintenanceWindowsForTarget
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
module Amazonka.SSM.DescribeMaintenanceWindowsForTarget
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
    describeMaintenanceWindowsForTargetResponse_windowIdentities,
    describeMaintenanceWindowsForTargetResponse_nextToken,
    describeMaintenanceWindowsForTargetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowsForTarget' smart constructor.
data DescribeMaintenanceWindowsForTarget = DescribeMaintenanceWindowsForTarget'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The instance ID or key-value pair to retrieve information about.
    targets :: [Target],
    -- | The type of resource you want to retrieve information about. For
    -- example, @INSTANCE@.
    resourceType :: MaintenanceWindowResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'targets', 'describeMaintenanceWindowsForTarget_targets' - The instance ID or key-value pair to retrieve information about.
--
-- 'resourceType', 'describeMaintenanceWindowsForTarget_resourceType' - The type of resource you want to retrieve information about. For
-- example, @INSTANCE@.
newDescribeMaintenanceWindowsForTarget ::
  -- | 'resourceType'
  MaintenanceWindowResourceType ->
  DescribeMaintenanceWindowsForTarget
newDescribeMaintenanceWindowsForTarget pResourceType_ =
  DescribeMaintenanceWindowsForTarget'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      targets = Prelude.mempty,
      resourceType = pResourceType_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowsForTarget_nextToken :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowsForTarget_nextToken = Lens.lens (\DescribeMaintenanceWindowsForTarget' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowsForTarget' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowsForTarget)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowsForTarget_maxResults :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindowsForTarget_maxResults = Lens.lens (\DescribeMaintenanceWindowsForTarget' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowsForTarget' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowsForTarget)

-- | The instance ID or key-value pair to retrieve information about.
describeMaintenanceWindowsForTarget_targets :: Lens.Lens' DescribeMaintenanceWindowsForTarget [Target]
describeMaintenanceWindowsForTarget_targets = Lens.lens (\DescribeMaintenanceWindowsForTarget' {targets} -> targets) (\s@DescribeMaintenanceWindowsForTarget' {} a -> s {targets = a} :: DescribeMaintenanceWindowsForTarget) Prelude.. Lens.coerced

-- | The type of resource you want to retrieve information about. For
-- example, @INSTANCE@.
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
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowsForTargetResponse_windowIdentities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMaintenanceWindowsForTarget_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowsForTargetResponse_nextToken
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..?> "WindowIdentities"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceWindowsForTarget

instance
  Prelude.NFData
    DescribeMaintenanceWindowsForTarget

instance
  Core.ToHeaders
    DescribeMaintenanceWindowsForTarget
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowsForTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeMaintenanceWindowsForTarget
  where
  toJSON DescribeMaintenanceWindowsForTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Targets" Core..= targets),
            Prelude.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance
  Core.ToPath
    DescribeMaintenanceWindowsForTarget
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowsForTarget
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowsForTargetResponse' smart constructor.
data DescribeMaintenanceWindowsForTargetResponse = DescribeMaintenanceWindowsForTargetResponse'
  { -- | Information about the maintenance window targets and tasks an instance
    -- is associated with.
    windowIdentities :: Prelude.Maybe [MaintenanceWindowIdentityForTarget],
    -- | The token for the next set of items to return. (You use this token in
    -- the next call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowsForTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowIdentities', 'describeMaintenanceWindowsForTargetResponse_windowIdentities' - Information about the maintenance window targets and tasks an instance
-- is associated with.
--
-- 'nextToken', 'describeMaintenanceWindowsForTargetResponse_nextToken' - The token for the next set of items to return. (You use this token in
-- the next call.)
--
-- 'httpStatus', 'describeMaintenanceWindowsForTargetResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowsForTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMaintenanceWindowsForTargetResponse
newDescribeMaintenanceWindowsForTargetResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowsForTargetResponse'
      { windowIdentities =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the maintenance window targets and tasks an instance
-- is associated with.
describeMaintenanceWindowsForTargetResponse_windowIdentities :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Prelude.Maybe [MaintenanceWindowIdentityForTarget])
describeMaintenanceWindowsForTargetResponse_windowIdentities = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {windowIdentities} -> windowIdentities) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsForTargetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. (You use this token in
-- the next call.)
describeMaintenanceWindowsForTargetResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowsForTargetResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowsForTargetResponse)

-- | The response's http status code.
describeMaintenanceWindowsForTargetResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse Prelude.Int
describeMaintenanceWindowsForTargetResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowsForTargetResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowsForTargetResponse
