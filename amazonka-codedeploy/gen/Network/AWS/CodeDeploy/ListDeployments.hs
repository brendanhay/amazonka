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
-- Module      : Network.AWS.CodeDeploy.ListDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployments in a deployment group for an application
-- registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeployments
  ( -- * Creating a Request
    ListDeployments (..),
    newListDeployments,

    -- * Request Lenses
    listDeployments_nextToken,
    listDeployments_deploymentGroupName,
    listDeployments_createTimeRange,
    listDeployments_includeOnlyStatuses,
    listDeployments_externalId,
    listDeployments_applicationName,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeployments@ operation.
--
-- /See:/ 'newListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { -- | An identifier returned from the previous list deployments call. It can
    -- be used to return the next set of deployments in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of a deployment group for the specified application.
    --
    -- If @deploymentGroupName@ is specified, then @applicationName@ must be
    -- specified. If it is not specified, then @applicationName@ must not be
    -- specified.
    deploymentGroupName :: Prelude.Maybe Prelude.Text,
    -- | A time range (start and end) for returning a subset of the list of
    -- deployments.
    createTimeRange :: Prelude.Maybe TimeRange,
    -- | A subset of deployments to list by status:
    --
    -- -   @Created@: Include created deployments in the resulting list.
    --
    -- -   @Queued@: Include queued deployments in the resulting list.
    --
    -- -   @In Progress@: Include in-progress deployments in the resulting
    --     list.
    --
    -- -   @Succeeded@: Include successful deployments in the resulting list.
    --
    -- -   @Failed@: Include failed deployments in the resulting list.
    --
    -- -   @Stopped@: Include stopped deployments in the resulting list.
    includeOnlyStatuses :: Prelude.Maybe [DeploymentStatus],
    -- | The unique ID of an external resource for returning deployments linked
    -- to the external resource.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    --
    -- If @applicationName@ is specified, then @deploymentGroupName@ must be
    -- specified. If it is not specified, then @deploymentGroupName@ must not
    -- be specified.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeployments_nextToken' - An identifier returned from the previous list deployments call. It can
-- be used to return the next set of deployments in the list.
--
-- 'deploymentGroupName', 'listDeployments_deploymentGroupName' - The name of a deployment group for the specified application.
--
-- If @deploymentGroupName@ is specified, then @applicationName@ must be
-- specified. If it is not specified, then @applicationName@ must not be
-- specified.
--
-- 'createTimeRange', 'listDeployments_createTimeRange' - A time range (start and end) for returning a subset of the list of
-- deployments.
--
-- 'includeOnlyStatuses', 'listDeployments_includeOnlyStatuses' - A subset of deployments to list by status:
--
-- -   @Created@: Include created deployments in the resulting list.
--
-- -   @Queued@: Include queued deployments in the resulting list.
--
-- -   @In Progress@: Include in-progress deployments in the resulting
--     list.
--
-- -   @Succeeded@: Include successful deployments in the resulting list.
--
-- -   @Failed@: Include failed deployments in the resulting list.
--
-- -   @Stopped@: Include stopped deployments in the resulting list.
--
-- 'externalId', 'listDeployments_externalId' - The unique ID of an external resource for returning deployments linked
-- to the external resource.
--
-- 'applicationName', 'listDeployments_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
--
-- If @applicationName@ is specified, then @deploymentGroupName@ must be
-- specified. If it is not specified, then @deploymentGroupName@ must not
-- be specified.
newListDeployments ::
  ListDeployments
newListDeployments =
  ListDeployments'
    { nextToken = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing,
      createTimeRange = Prelude.Nothing,
      includeOnlyStatuses = Prelude.Nothing,
      externalId = Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | An identifier returned from the previous list deployments call. It can
-- be used to return the next set of deployments in the list.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

-- | The name of a deployment group for the specified application.
--
-- If @deploymentGroupName@ is specified, then @applicationName@ must be
-- specified. If it is not specified, then @applicationName@ must not be
-- specified.
listDeployments_deploymentGroupName :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_deploymentGroupName = Lens.lens (\ListDeployments' {deploymentGroupName} -> deploymentGroupName) (\s@ListDeployments' {} a -> s {deploymentGroupName = a} :: ListDeployments)

-- | A time range (start and end) for returning a subset of the list of
-- deployments.
listDeployments_createTimeRange :: Lens.Lens' ListDeployments (Prelude.Maybe TimeRange)
listDeployments_createTimeRange = Lens.lens (\ListDeployments' {createTimeRange} -> createTimeRange) (\s@ListDeployments' {} a -> s {createTimeRange = a} :: ListDeployments)

-- | A subset of deployments to list by status:
--
-- -   @Created@: Include created deployments in the resulting list.
--
-- -   @Queued@: Include queued deployments in the resulting list.
--
-- -   @In Progress@: Include in-progress deployments in the resulting
--     list.
--
-- -   @Succeeded@: Include successful deployments in the resulting list.
--
-- -   @Failed@: Include failed deployments in the resulting list.
--
-- -   @Stopped@: Include stopped deployments in the resulting list.
listDeployments_includeOnlyStatuses :: Lens.Lens' ListDeployments (Prelude.Maybe [DeploymentStatus])
listDeployments_includeOnlyStatuses = Lens.lens (\ListDeployments' {includeOnlyStatuses} -> includeOnlyStatuses) (\s@ListDeployments' {} a -> s {includeOnlyStatuses = a} :: ListDeployments) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique ID of an external resource for returning deployments linked
-- to the external resource.
listDeployments_externalId :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_externalId = Lens.lens (\ListDeployments' {externalId} -> externalId) (\s@ListDeployments' {} a -> s {externalId = a} :: ListDeployments)

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
--
-- If @applicationName@ is specified, then @deploymentGroupName@ must be
-- specified. If it is not specified, then @deploymentGroupName@ must not
-- be specified.
listDeployments_applicationName :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_applicationName = Lens.lens (\ListDeployments' {applicationName} -> applicationName) (\s@ListDeployments' {} a -> s {applicationName = a} :: ListDeployments)

instance Pager.AWSPager ListDeployments where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDeploymentsResponse_deployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDeployments_nextToken
          Lens..~ rs
          Lens.^? listDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListDeployments where
  type Rs ListDeployments = ListDeploymentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "deployments"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeployments

instance Prelude.NFData ListDeployments

instance Prelude.ToHeaders ListDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.ListDeployments" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDeployments where
  toJSON ListDeployments' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("deploymentGroupName" Prelude..=)
              Prelude.<$> deploymentGroupName,
            ("createTimeRange" Prelude..=)
              Prelude.<$> createTimeRange,
            ("includeOnlyStatuses" Prelude..=)
              Prelude.<$> includeOnlyStatuses,
            ("externalId" Prelude..=) Prelude.<$> externalId,
            ("applicationName" Prelude..=)
              Prelude.<$> applicationName
          ]
      )

instance Prelude.ToPath ListDeployments where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDeployments where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListDeployments@ operation.
--
-- /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list deployments call to return
    -- the next set of deployments in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of deployment IDs.
    deployments :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployments call to return
-- the next set of deployments in the list.
--
-- 'deployments', 'listDeploymentsResponse_deployments' - A list of deployment IDs.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { nextToken =
        Prelude.Nothing,
      deployments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployments call to return
-- the next set of deployments in the list.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe Prelude.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | A list of deployment IDs.
listDeploymentsResponse_deployments :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe [Prelude.Text])
listDeploymentsResponse_deployments = Lens.lens (\ListDeploymentsResponse' {deployments} -> deployments) (\s@ListDeploymentsResponse' {} a -> s {deployments = a} :: ListDeploymentsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Prelude.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

instance Prelude.NFData ListDeploymentsResponse
