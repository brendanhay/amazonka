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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the maintenance window targets or tasks that
-- a managed node is associated with.
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
    describeMaintenanceWindowsForTargetResponse_nextToken,
    describeMaintenanceWindowsForTargetResponse_windowIdentities,
    describeMaintenanceWindowsForTargetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | The managed node ID or key-value pair to retrieve information about.
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
-- 'targets', 'describeMaintenanceWindowsForTarget_targets' - The managed node ID or key-value pair to retrieve information about.
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

-- | The managed node ID or key-value pair to retrieve information about.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsForTargetResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x Data..?> "WindowIdentities"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceWindowsForTarget
  where
  hashWithSalt
    _salt
    DescribeMaintenanceWindowsForTarget' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` targets
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    DescribeMaintenanceWindowsForTarget
  where
  rnf DescribeMaintenanceWindowsForTarget' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf resourceType

instance
  Data.ToHeaders
    DescribeMaintenanceWindowsForTarget
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeMaintenanceWindowsForTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeMaintenanceWindowsForTarget
  where
  toJSON DescribeMaintenanceWindowsForTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Targets" Data..= targets),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance
  Data.ToPath
    DescribeMaintenanceWindowsForTarget
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeMaintenanceWindowsForTarget
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowsForTargetResponse' smart constructor.
data DescribeMaintenanceWindowsForTargetResponse = DescribeMaintenanceWindowsForTargetResponse'
  { -- | The token for the next set of items to return. (You use this token in
    -- the next call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the maintenance window targets and tasks a managed
    -- node is associated with.
    windowIdentities :: Prelude.Maybe [MaintenanceWindowIdentityForTarget],
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
-- 'nextToken', 'describeMaintenanceWindowsForTargetResponse_nextToken' - The token for the next set of items to return. (You use this token in
-- the next call.)
--
-- 'windowIdentities', 'describeMaintenanceWindowsForTargetResponse_windowIdentities' - Information about the maintenance window targets and tasks a managed
-- node is associated with.
--
-- 'httpStatus', 'describeMaintenanceWindowsForTargetResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowsForTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMaintenanceWindowsForTargetResponse
newDescribeMaintenanceWindowsForTargetResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowsForTargetResponse'
      { nextToken =
          Prelude.Nothing,
        windowIdentities =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of items to return. (You use this token in
-- the next call.)
describeMaintenanceWindowsForTargetResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowsForTargetResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowsForTargetResponse)

-- | Information about the maintenance window targets and tasks a managed
-- node is associated with.
describeMaintenanceWindowsForTargetResponse_windowIdentities :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Prelude.Maybe [MaintenanceWindowIdentityForTarget])
describeMaintenanceWindowsForTargetResponse_windowIdentities = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {windowIdentities} -> windowIdentities) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsForTargetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMaintenanceWindowsForTargetResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse Prelude.Int
describeMaintenanceWindowsForTargetResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowsForTargetResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowsForTargetResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowsForTargetResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowsForTargetResponse
  where
  rnf DescribeMaintenanceWindowsForTargetResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf windowIdentities
      `Prelude.seq` Prelude.rnf httpStatus
