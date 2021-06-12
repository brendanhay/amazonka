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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment groups for an application registered with the IAM
-- user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentGroups
  ( -- * Creating a Request
    ListDeploymentGroups (..),
    newListDeploymentGroups,

    -- * Request Lenses
    listDeploymentGroups_nextToken,
    listDeploymentGroups_applicationName,

    -- * Destructuring the Response
    ListDeploymentGroupsResponse (..),
    newListDeploymentGroupsResponse,

    -- * Response Lenses
    listDeploymentGroupsResponse_nextToken,
    listDeploymentGroupsResponse_deploymentGroups,
    listDeploymentGroupsResponse_applicationName,
    listDeploymentGroupsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'newListDeploymentGroups' smart constructor.
data ListDeploymentGroups = ListDeploymentGroups'
  { -- | An identifier returned from the previous list deployment groups call. It
    -- can be used to return the next set of deployment groups in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeploymentGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentGroups_nextToken' - An identifier returned from the previous list deployment groups call. It
-- can be used to return the next set of deployment groups in the list.
--
-- 'applicationName', 'listDeploymentGroups_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
newListDeploymentGroups ::
  -- | 'applicationName'
  Core.Text ->
  ListDeploymentGroups
newListDeploymentGroups pApplicationName_ =
  ListDeploymentGroups'
    { nextToken = Core.Nothing,
      applicationName = pApplicationName_
    }

-- | An identifier returned from the previous list deployment groups call. It
-- can be used to return the next set of deployment groups in the list.
listDeploymentGroups_nextToken :: Lens.Lens' ListDeploymentGroups (Core.Maybe Core.Text)
listDeploymentGroups_nextToken = Lens.lens (\ListDeploymentGroups' {nextToken} -> nextToken) (\s@ListDeploymentGroups' {} a -> s {nextToken = a} :: ListDeploymentGroups)

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
listDeploymentGroups_applicationName :: Lens.Lens' ListDeploymentGroups Core.Text
listDeploymentGroups_applicationName = Lens.lens (\ListDeploymentGroups' {applicationName} -> applicationName) (\s@ListDeploymentGroups' {} a -> s {applicationName = a} :: ListDeploymentGroups)

instance Core.AWSPager ListDeploymentGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentGroupsResponse_deploymentGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeploymentGroups_nextToken
          Lens..~ rs
          Lens.^? listDeploymentGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDeploymentGroups where
  type
    AWSResponse ListDeploymentGroups =
      ListDeploymentGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentGroupsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "deploymentGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "applicationName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDeploymentGroups

instance Core.NFData ListDeploymentGroups

instance Core.ToHeaders ListDeploymentGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ListDeploymentGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDeploymentGroups where
  toJSON ListDeploymentGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just
              ("applicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath ListDeploymentGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListDeploymentGroups where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'newListDeploymentGroupsResponse' smart constructor.
data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list deployment groups call to
    -- return the next set of deployment groups in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of deployment group names.
    deploymentGroups :: Core.Maybe [Core.Text],
    -- | The application name.
    applicationName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeploymentGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentGroupsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment groups call to
-- return the next set of deployment groups in the list.
--
-- 'deploymentGroups', 'listDeploymentGroupsResponse_deploymentGroups' - A list of deployment group names.
--
-- 'applicationName', 'listDeploymentGroupsResponse_applicationName' - The application name.
--
-- 'httpStatus', 'listDeploymentGroupsResponse_httpStatus' - The response's http status code.
newListDeploymentGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeploymentGroupsResponse
newListDeploymentGroupsResponse pHttpStatus_ =
  ListDeploymentGroupsResponse'
    { nextToken =
        Core.Nothing,
      deploymentGroups = Core.Nothing,
      applicationName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment groups call to
-- return the next set of deployment groups in the list.
listDeploymentGroupsResponse_nextToken :: Lens.Lens' ListDeploymentGroupsResponse (Core.Maybe Core.Text)
listDeploymentGroupsResponse_nextToken = Lens.lens (\ListDeploymentGroupsResponse' {nextToken} -> nextToken) (\s@ListDeploymentGroupsResponse' {} a -> s {nextToken = a} :: ListDeploymentGroupsResponse)

-- | A list of deployment group names.
listDeploymentGroupsResponse_deploymentGroups :: Lens.Lens' ListDeploymentGroupsResponse (Core.Maybe [Core.Text])
listDeploymentGroupsResponse_deploymentGroups = Lens.lens (\ListDeploymentGroupsResponse' {deploymentGroups} -> deploymentGroups) (\s@ListDeploymentGroupsResponse' {} a -> s {deploymentGroups = a} :: ListDeploymentGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The application name.
listDeploymentGroupsResponse_applicationName :: Lens.Lens' ListDeploymentGroupsResponse (Core.Maybe Core.Text)
listDeploymentGroupsResponse_applicationName = Lens.lens (\ListDeploymentGroupsResponse' {applicationName} -> applicationName) (\s@ListDeploymentGroupsResponse' {} a -> s {applicationName = a} :: ListDeploymentGroupsResponse)

-- | The response's http status code.
listDeploymentGroupsResponse_httpStatus :: Lens.Lens' ListDeploymentGroupsResponse Core.Int
listDeploymentGroupsResponse_httpStatus = Lens.lens (\ListDeploymentGroupsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentGroupsResponse' {} a -> s {httpStatus = a} :: ListDeploymentGroupsResponse)

instance Core.NFData ListDeploymentGroupsResponse
