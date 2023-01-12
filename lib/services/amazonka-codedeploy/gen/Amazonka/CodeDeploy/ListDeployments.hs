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
-- Module      : Amazonka.CodeDeploy.ListDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployments in a deployment group for an application
-- registered with the IAM user or Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.CodeDeploy.ListDeployments
  ( -- * Creating a Request
    ListDeployments (..),
    newListDeployments,

    -- * Request Lenses
    listDeployments_applicationName,
    listDeployments_createTimeRange,
    listDeployments_deploymentGroupName,
    listDeployments_externalId,
    listDeployments_includeOnlyStatuses,
    listDeployments_nextToken,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ListDeployments@ operation.
--
-- /See:/ 'newListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { -- | The name of an CodeDeploy application associated with the IAM user or
    -- Amazon Web Services account.
    --
    -- If @applicationName@ is specified, then @deploymentGroupName@ must be
    -- specified. If it is not specified, then @deploymentGroupName@ must not
    -- be specified.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | A time range (start and end) for returning a subset of the list of
    -- deployments.
    createTimeRange :: Prelude.Maybe TimeRange,
    -- | The name of a deployment group for the specified application.
    --
    -- If @deploymentGroupName@ is specified, then @applicationName@ must be
    -- specified. If it is not specified, then @applicationName@ must not be
    -- specified.
    deploymentGroupName :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of an external resource for returning deployments linked
    -- to the external resource.
    externalId :: Prelude.Maybe Prelude.Text,
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
    -- | An identifier returned from the previous list deployments call. It can
    -- be used to return the next set of deployments in the list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'listDeployments_applicationName' - The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
--
-- If @applicationName@ is specified, then @deploymentGroupName@ must be
-- specified. If it is not specified, then @deploymentGroupName@ must not
-- be specified.
--
-- 'createTimeRange', 'listDeployments_createTimeRange' - A time range (start and end) for returning a subset of the list of
-- deployments.
--
-- 'deploymentGroupName', 'listDeployments_deploymentGroupName' - The name of a deployment group for the specified application.
--
-- If @deploymentGroupName@ is specified, then @applicationName@ must be
-- specified. If it is not specified, then @applicationName@ must not be
-- specified.
--
-- 'externalId', 'listDeployments_externalId' - The unique ID of an external resource for returning deployments linked
-- to the external resource.
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
-- 'nextToken', 'listDeployments_nextToken' - An identifier returned from the previous list deployments call. It can
-- be used to return the next set of deployments in the list.
newListDeployments ::
  ListDeployments
newListDeployments =
  ListDeployments'
    { applicationName = Prelude.Nothing,
      createTimeRange = Prelude.Nothing,
      deploymentGroupName = Prelude.Nothing,
      externalId = Prelude.Nothing,
      includeOnlyStatuses = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
--
-- If @applicationName@ is specified, then @deploymentGroupName@ must be
-- specified. If it is not specified, then @deploymentGroupName@ must not
-- be specified.
listDeployments_applicationName :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_applicationName = Lens.lens (\ListDeployments' {applicationName} -> applicationName) (\s@ListDeployments' {} a -> s {applicationName = a} :: ListDeployments)

-- | A time range (start and end) for returning a subset of the list of
-- deployments.
listDeployments_createTimeRange :: Lens.Lens' ListDeployments (Prelude.Maybe TimeRange)
listDeployments_createTimeRange = Lens.lens (\ListDeployments' {createTimeRange} -> createTimeRange) (\s@ListDeployments' {} a -> s {createTimeRange = a} :: ListDeployments)

-- | The name of a deployment group for the specified application.
--
-- If @deploymentGroupName@ is specified, then @applicationName@ must be
-- specified. If it is not specified, then @applicationName@ must not be
-- specified.
listDeployments_deploymentGroupName :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_deploymentGroupName = Lens.lens (\ListDeployments' {deploymentGroupName} -> deploymentGroupName) (\s@ListDeployments' {} a -> s {deploymentGroupName = a} :: ListDeployments)

-- | The unique ID of an external resource for returning deployments linked
-- to the external resource.
listDeployments_externalId :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_externalId = Lens.lens (\ListDeployments' {externalId} -> externalId) (\s@ListDeployments' {} a -> s {externalId = a} :: ListDeployments)

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
listDeployments_includeOnlyStatuses = Lens.lens (\ListDeployments' {includeOnlyStatuses} -> includeOnlyStatuses) (\s@ListDeployments' {} a -> s {includeOnlyStatuses = a} :: ListDeployments) Prelude.. Lens.mapping Lens.coerced

-- | An identifier returned from the previous list deployments call. It can
-- be used to return the next set of deployments in the list.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

instance Core.AWSPager ListDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_deployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeployments_nextToken
          Lens..~ rs
          Lens.^? listDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeployments where
  type
    AWSResponse ListDeployments =
      ListDeploymentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Prelude.<$> (x Data..?> "deployments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeployments where
  hashWithSalt _salt ListDeployments' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` createTimeRange
      `Prelude.hashWithSalt` deploymentGroupName
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` includeOnlyStatuses
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDeployments where
  rnf ListDeployments' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf createTimeRange
      `Prelude.seq` Prelude.rnf deploymentGroupName
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf includeOnlyStatuses
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.ListDeployments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDeployments where
  toJSON ListDeployments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationName" Data..=)
              Prelude.<$> applicationName,
            ("createTimeRange" Data..=)
              Prelude.<$> createTimeRange,
            ("deploymentGroupName" Data..=)
              Prelude.<$> deploymentGroupName,
            ("externalId" Data..=) Prelude.<$> externalId,
            ("includeOnlyStatuses" Data..=)
              Prelude.<$> includeOnlyStatuses,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDeployments where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDeployments where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListDeployments@ operation.
--
-- /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | A list of deployment IDs.
    deployments :: Prelude.Maybe [Prelude.Text],
    -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list deployments call to return
    -- the next set of deployments in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployments', 'listDeploymentsResponse_deployments' - A list of deployment IDs.
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployments call to return
-- the next set of deployments in the list.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { deployments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of deployment IDs.
listDeploymentsResponse_deployments :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe [Prelude.Text])
listDeploymentsResponse_deployments = Lens.lens (\ListDeploymentsResponse' {deployments} -> deployments) (\s@ListDeploymentsResponse' {} a -> s {deployments = a} :: ListDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployments call to return
-- the next set of deployments in the list.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe Prelude.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Prelude.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

instance Prelude.NFData ListDeploymentsResponse where
  rnf ListDeploymentsResponse' {..} =
    Prelude.rnf deployments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
