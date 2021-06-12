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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment configurations with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentConfigs
  ( -- * Creating a Request
    ListDeploymentConfigs (..),
    newListDeploymentConfigs,

    -- * Request Lenses
    listDeploymentConfigs_nextToken,

    -- * Destructuring the Response
    ListDeploymentConfigsResponse (..),
    newListDeploymentConfigsResponse,

    -- * Response Lenses
    listDeploymentConfigsResponse_nextToken,
    listDeploymentConfigsResponse_deploymentConfigsList,
    listDeploymentConfigsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'newListDeploymentConfigs' smart constructor.
data ListDeploymentConfigs = ListDeploymentConfigs'
  { -- | An identifier returned from the previous @ListDeploymentConfigs@ call.
    -- It can be used to return the next set of deployment configurations in
    -- the list.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeploymentConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentConfigs_nextToken' - An identifier returned from the previous @ListDeploymentConfigs@ call.
-- It can be used to return the next set of deployment configurations in
-- the list.
newListDeploymentConfigs ::
  ListDeploymentConfigs
newListDeploymentConfigs =
  ListDeploymentConfigs' {nextToken = Core.Nothing}

-- | An identifier returned from the previous @ListDeploymentConfigs@ call.
-- It can be used to return the next set of deployment configurations in
-- the list.
listDeploymentConfigs_nextToken :: Lens.Lens' ListDeploymentConfigs (Core.Maybe Core.Text)
listDeploymentConfigs_nextToken = Lens.lens (\ListDeploymentConfigs' {nextToken} -> nextToken) (\s@ListDeploymentConfigs' {} a -> s {nextToken = a} :: ListDeploymentConfigs)

instance Core.AWSPager ListDeploymentConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentConfigsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentConfigsResponse_deploymentConfigsList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeploymentConfigs_nextToken
          Lens..~ rs
          Lens.^? listDeploymentConfigsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDeploymentConfigs where
  type
    AWSResponse ListDeploymentConfigs =
      ListDeploymentConfigsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentConfigsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "deploymentConfigsList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDeploymentConfigs

instance Core.NFData ListDeploymentConfigs

instance Core.ToHeaders ListDeploymentConfigs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ListDeploymentConfigs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDeploymentConfigs where
  toJSON ListDeploymentConfigs' {..} =
    Core.object
      ( Core.catMaybes
          [("nextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListDeploymentConfigs where
  toPath = Core.const "/"

instance Core.ToQuery ListDeploymentConfigs where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'newListDeploymentConfigsResponse' smart constructor.
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list deployment configurations
    -- call to return the next set of deployment configurations in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of deployment configurations, including built-in configurations
    -- such as @CodeDeployDefault.OneAtATime@.
    deploymentConfigsList :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeploymentConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentConfigsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment configurations
-- call to return the next set of deployment configurations in the list.
--
-- 'deploymentConfigsList', 'listDeploymentConfigsResponse_deploymentConfigsList' - A list of deployment configurations, including built-in configurations
-- such as @CodeDeployDefault.OneAtATime@.
--
-- 'httpStatus', 'listDeploymentConfigsResponse_httpStatus' - The response's http status code.
newListDeploymentConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeploymentConfigsResponse
newListDeploymentConfigsResponse pHttpStatus_ =
  ListDeploymentConfigsResponse'
    { nextToken =
        Core.Nothing,
      deploymentConfigsList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment configurations
-- call to return the next set of deployment configurations in the list.
listDeploymentConfigsResponse_nextToken :: Lens.Lens' ListDeploymentConfigsResponse (Core.Maybe Core.Text)
listDeploymentConfigsResponse_nextToken = Lens.lens (\ListDeploymentConfigsResponse' {nextToken} -> nextToken) (\s@ListDeploymentConfigsResponse' {} a -> s {nextToken = a} :: ListDeploymentConfigsResponse)

-- | A list of deployment configurations, including built-in configurations
-- such as @CodeDeployDefault.OneAtATime@.
listDeploymentConfigsResponse_deploymentConfigsList :: Lens.Lens' ListDeploymentConfigsResponse (Core.Maybe [Core.Text])
listDeploymentConfigsResponse_deploymentConfigsList = Lens.lens (\ListDeploymentConfigsResponse' {deploymentConfigsList} -> deploymentConfigsList) (\s@ListDeploymentConfigsResponse' {} a -> s {deploymentConfigsList = a} :: ListDeploymentConfigsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDeploymentConfigsResponse_httpStatus :: Lens.Lens' ListDeploymentConfigsResponse Core.Int
listDeploymentConfigsResponse_httpStatus = Lens.lens (\ListDeploymentConfigsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentConfigsResponse' {} a -> s {httpStatus = a} :: ListDeploymentConfigsResponse)

instance Core.NFData ListDeploymentConfigsResponse
