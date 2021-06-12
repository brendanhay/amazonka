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
-- Module      : Network.AWS.Mobile.DescribeProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a project in AWS Mobile Hub.
module Network.AWS.Mobile.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_syncFromResources,
    describeProject_projectId,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_details,
    describeProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request details about a project.
--
-- /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | If set to true, causes AWS Mobile Hub to synchronize information from
    -- other services, e.g., update state of AWS CloudFormation stacks in the
    -- AWS Mobile Hub project.
    syncFromResources :: Core.Maybe Core.Bool,
    -- | Unique project identifier.
    projectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncFromResources', 'describeProject_syncFromResources' - If set to true, causes AWS Mobile Hub to synchronize information from
-- other services, e.g., update state of AWS CloudFormation stacks in the
-- AWS Mobile Hub project.
--
-- 'projectId', 'describeProject_projectId' - Unique project identifier.
newDescribeProject ::
  -- | 'projectId'
  Core.Text ->
  DescribeProject
newDescribeProject pProjectId_ =
  DescribeProject'
    { syncFromResources = Core.Nothing,
      projectId = pProjectId_
    }

-- | If set to true, causes AWS Mobile Hub to synchronize information from
-- other services, e.g., update state of AWS CloudFormation stacks in the
-- AWS Mobile Hub project.
describeProject_syncFromResources :: Lens.Lens' DescribeProject (Core.Maybe Core.Bool)
describeProject_syncFromResources = Lens.lens (\DescribeProject' {syncFromResources} -> syncFromResources) (\s@DescribeProject' {} a -> s {syncFromResources = a} :: DescribeProject)

-- | Unique project identifier.
describeProject_projectId :: Lens.Lens' DescribeProject Core.Text
describeProject_projectId = Lens.lens (\DescribeProject' {projectId} -> projectId) (\s@DescribeProject' {} a -> s {projectId = a} :: DescribeProject)

instance Core.AWSRequest DescribeProject where
  type
    AWSResponse DescribeProject =
      DescribeProjectResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Core.<$> (x Core..?> "details")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProject

instance Core.NFData DescribeProject

instance Core.ToHeaders DescribeProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeProject where
  toPath = Core.const "/project"

instance Core.ToQuery DescribeProject where
  toQuery DescribeProject' {..} =
    Core.mconcat
      [ "syncFromResources" Core.=: syncFromResources,
        "projectId" Core.=: projectId
      ]

-- | Result structure used for requests of project details.
--
-- /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { details :: Core.Maybe ProjectDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'describeProjectResponse_details' - Undocumented member.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProjectResponse
newDescribeProjectResponse pHttpStatus_ =
  DescribeProjectResponse'
    { details = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeProjectResponse_details :: Lens.Lens' DescribeProjectResponse (Core.Maybe ProjectDetails)
describeProjectResponse_details = Lens.lens (\DescribeProjectResponse' {details} -> details) (\s@DescribeProjectResponse' {} a -> s {details = a} :: DescribeProjectResponse)

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Core.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

instance Core.NFData DescribeProjectResponse
