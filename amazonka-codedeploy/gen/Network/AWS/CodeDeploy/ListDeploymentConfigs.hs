{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'newListDeploymentConfigs' smart constructor.
data ListDeploymentConfigs = ListDeploymentConfigs'
  { -- | An identifier returned from the previous @ListDeploymentConfigs@ call.
    -- It can be used to return the next set of deployment configurations in
    -- the list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ListDeploymentConfigs' {nextToken = Prelude.Nothing}

-- | An identifier returned from the previous @ListDeploymentConfigs@ call.
-- It can be used to return the next set of deployment configurations in
-- the list.
listDeploymentConfigs_nextToken :: Lens.Lens' ListDeploymentConfigs (Prelude.Maybe Prelude.Text)
listDeploymentConfigs_nextToken = Lens.lens (\ListDeploymentConfigs' {nextToken} -> nextToken) (\s@ListDeploymentConfigs' {} a -> s {nextToken = a} :: ListDeploymentConfigs)

instance Pager.AWSPager ListDeploymentConfigs where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDeploymentConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDeploymentConfigsResponse_deploymentConfigsList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDeploymentConfigs_nextToken
          Lens..~ rs
          Lens.^? listDeploymentConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListDeploymentConfigs where
  type
    Rs ListDeploymentConfigs =
      ListDeploymentConfigsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentConfigsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "deploymentConfigsList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeploymentConfigs

instance Prelude.NFData ListDeploymentConfigs

instance Prelude.ToHeaders ListDeploymentConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.ListDeploymentConfigs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDeploymentConfigs where
  toJSON ListDeploymentConfigs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("nextToken" Prelude..=) Prelude.<$> nextToken]
      )

instance Prelude.ToPath ListDeploymentConfigs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDeploymentConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'newListDeploymentConfigsResponse' smart constructor.
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list deployment configurations
    -- call to return the next set of deployment configurations in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of deployment configurations, including built-in configurations
    -- such as @CodeDeployDefault.OneAtATime@.
    deploymentConfigsList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListDeploymentConfigsResponse
newListDeploymentConfigsResponse pHttpStatus_ =
  ListDeploymentConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      deploymentConfigsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment configurations
-- call to return the next set of deployment configurations in the list.
listDeploymentConfigsResponse_nextToken :: Lens.Lens' ListDeploymentConfigsResponse (Prelude.Maybe Prelude.Text)
listDeploymentConfigsResponse_nextToken = Lens.lens (\ListDeploymentConfigsResponse' {nextToken} -> nextToken) (\s@ListDeploymentConfigsResponse' {} a -> s {nextToken = a} :: ListDeploymentConfigsResponse)

-- | A list of deployment configurations, including built-in configurations
-- such as @CodeDeployDefault.OneAtATime@.
listDeploymentConfigsResponse_deploymentConfigsList :: Lens.Lens' ListDeploymentConfigsResponse (Prelude.Maybe [Prelude.Text])
listDeploymentConfigsResponse_deploymentConfigsList = Lens.lens (\ListDeploymentConfigsResponse' {deploymentConfigsList} -> deploymentConfigsList) (\s@ListDeploymentConfigsResponse' {} a -> s {deploymentConfigsList = a} :: ListDeploymentConfigsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listDeploymentConfigsResponse_httpStatus :: Lens.Lens' ListDeploymentConfigsResponse Prelude.Int
listDeploymentConfigsResponse_httpStatus = Lens.lens (\ListDeploymentConfigsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentConfigsResponse' {} a -> s {httpStatus = a} :: ListDeploymentConfigsResponse)

instance Prelude.NFData ListDeploymentConfigsResponse
