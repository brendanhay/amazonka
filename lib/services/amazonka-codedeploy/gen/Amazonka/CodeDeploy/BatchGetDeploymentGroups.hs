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
-- Module      : Amazonka.CodeDeploy.BatchGetDeploymentGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployment groups.
module Amazonka.CodeDeploy.BatchGetDeploymentGroups
  ( -- * Creating a Request
    BatchGetDeploymentGroups (..),
    newBatchGetDeploymentGroups,

    -- * Request Lenses
    batchGetDeploymentGroups_applicationName,
    batchGetDeploymentGroups_deploymentGroupNames,

    -- * Destructuring the Response
    BatchGetDeploymentGroupsResponse (..),
    newBatchGetDeploymentGroupsResponse,

    -- * Response Lenses
    batchGetDeploymentGroupsResponse_deploymentGroupsInfo,
    batchGetDeploymentGroupsResponse_errorMessage,
    batchGetDeploymentGroupsResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'newBatchGetDeploymentGroups' smart constructor.
data BatchGetDeploymentGroups = BatchGetDeploymentGroups'
  { -- | The name of an CodeDeploy application associated with the applicable IAM
    -- or Amazon Web Services account.
    applicationName :: Prelude.Text,
    -- | The names of the deployment groups.
    deploymentGroupNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDeploymentGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'batchGetDeploymentGroups_applicationName' - The name of an CodeDeploy application associated with the applicable IAM
-- or Amazon Web Services account.
--
-- 'deploymentGroupNames', 'batchGetDeploymentGroups_deploymentGroupNames' - The names of the deployment groups.
newBatchGetDeploymentGroups ::
  -- | 'applicationName'
  Prelude.Text ->
  BatchGetDeploymentGroups
newBatchGetDeploymentGroups pApplicationName_ =
  BatchGetDeploymentGroups'
    { applicationName =
        pApplicationName_,
      deploymentGroupNames = Prelude.mempty
    }

-- | The name of an CodeDeploy application associated with the applicable IAM
-- or Amazon Web Services account.
batchGetDeploymentGroups_applicationName :: Lens.Lens' BatchGetDeploymentGroups Prelude.Text
batchGetDeploymentGroups_applicationName = Lens.lens (\BatchGetDeploymentGroups' {applicationName} -> applicationName) (\s@BatchGetDeploymentGroups' {} a -> s {applicationName = a} :: BatchGetDeploymentGroups)

-- | The names of the deployment groups.
batchGetDeploymentGroups_deploymentGroupNames :: Lens.Lens' BatchGetDeploymentGroups [Prelude.Text]
batchGetDeploymentGroups_deploymentGroupNames = Lens.lens (\BatchGetDeploymentGroups' {deploymentGroupNames} -> deploymentGroupNames) (\s@BatchGetDeploymentGroups' {} a -> s {deploymentGroupNames = a} :: BatchGetDeploymentGroups) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetDeploymentGroups where
  type
    AWSResponse BatchGetDeploymentGroups =
      BatchGetDeploymentGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDeploymentGroupsResponse'
            Prelude.<$> ( x Data..?> "deploymentGroupsInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "errorMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetDeploymentGroups where
  hashWithSalt _salt BatchGetDeploymentGroups' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` deploymentGroupNames

instance Prelude.NFData BatchGetDeploymentGroups where
  rnf BatchGetDeploymentGroups' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf deploymentGroupNames

instance Data.ToHeaders BatchGetDeploymentGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.BatchGetDeploymentGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetDeploymentGroups where
  toJSON BatchGetDeploymentGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationName" Data..= applicationName),
            Prelude.Just
              ( "deploymentGroupNames"
                  Data..= deploymentGroupNames
              )
          ]
      )

instance Data.ToPath BatchGetDeploymentGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetDeploymentGroups where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'newBatchGetDeploymentGroupsResponse' smart constructor.
data BatchGetDeploymentGroupsResponse = BatchGetDeploymentGroupsResponse'
  { -- | Information about the deployment groups.
    deploymentGroupsInfo :: Prelude.Maybe [DeploymentGroupInfo],
    -- | Information about errors that might have occurred during the API call.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDeploymentGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentGroupsInfo', 'batchGetDeploymentGroupsResponse_deploymentGroupsInfo' - Information about the deployment groups.
--
-- 'errorMessage', 'batchGetDeploymentGroupsResponse_errorMessage' - Information about errors that might have occurred during the API call.
--
-- 'httpStatus', 'batchGetDeploymentGroupsResponse_httpStatus' - The response's http status code.
newBatchGetDeploymentGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetDeploymentGroupsResponse
newBatchGetDeploymentGroupsResponse pHttpStatus_ =
  BatchGetDeploymentGroupsResponse'
    { deploymentGroupsInfo =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deployment groups.
batchGetDeploymentGroupsResponse_deploymentGroupsInfo :: Lens.Lens' BatchGetDeploymentGroupsResponse (Prelude.Maybe [DeploymentGroupInfo])
batchGetDeploymentGroupsResponse_deploymentGroupsInfo = Lens.lens (\BatchGetDeploymentGroupsResponse' {deploymentGroupsInfo} -> deploymentGroupsInfo) (\s@BatchGetDeploymentGroupsResponse' {} a -> s {deploymentGroupsInfo = a} :: BatchGetDeploymentGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about errors that might have occurred during the API call.
batchGetDeploymentGroupsResponse_errorMessage :: Lens.Lens' BatchGetDeploymentGroupsResponse (Prelude.Maybe Prelude.Text)
batchGetDeploymentGroupsResponse_errorMessage = Lens.lens (\BatchGetDeploymentGroupsResponse' {errorMessage} -> errorMessage) (\s@BatchGetDeploymentGroupsResponse' {} a -> s {errorMessage = a} :: BatchGetDeploymentGroupsResponse)

-- | The response's http status code.
batchGetDeploymentGroupsResponse_httpStatus :: Lens.Lens' BatchGetDeploymentGroupsResponse Prelude.Int
batchGetDeploymentGroupsResponse_httpStatus = Lens.lens (\BatchGetDeploymentGroupsResponse' {httpStatus} -> httpStatus) (\s@BatchGetDeploymentGroupsResponse' {} a -> s {httpStatus = a} :: BatchGetDeploymentGroupsResponse)

instance
  Prelude.NFData
    BatchGetDeploymentGroupsResponse
  where
  rnf BatchGetDeploymentGroupsResponse' {..} =
    Prelude.rnf deploymentGroupsInfo
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf httpStatus
