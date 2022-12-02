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
-- Module      : Amazonka.CodeDeploy.BatchGetDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployments. The maximum number of
-- deployments that can be returned is 25.
module Amazonka.CodeDeploy.BatchGetDeployments
  ( -- * Creating a Request
    BatchGetDeployments (..),
    newBatchGetDeployments,

    -- * Request Lenses
    batchGetDeployments_deploymentIds,

    -- * Destructuring the Response
    BatchGetDeploymentsResponse (..),
    newBatchGetDeploymentsResponse,

    -- * Response Lenses
    batchGetDeploymentsResponse_deploymentsInfo,
    batchGetDeploymentsResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @BatchGetDeployments@ operation.
--
-- /See:/ 'newBatchGetDeployments' smart constructor.
data BatchGetDeployments = BatchGetDeployments'
  { -- | A list of deployment IDs, separated by spaces. The maximum number of
    -- deployment IDs you can specify is 25.
    deploymentIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentIds', 'batchGetDeployments_deploymentIds' - A list of deployment IDs, separated by spaces. The maximum number of
-- deployment IDs you can specify is 25.
newBatchGetDeployments ::
  BatchGetDeployments
newBatchGetDeployments =
  BatchGetDeployments'
    { deploymentIds =
        Prelude.mempty
    }

-- | A list of deployment IDs, separated by spaces. The maximum number of
-- deployment IDs you can specify is 25.
batchGetDeployments_deploymentIds :: Lens.Lens' BatchGetDeployments [Prelude.Text]
batchGetDeployments_deploymentIds = Lens.lens (\BatchGetDeployments' {deploymentIds} -> deploymentIds) (\s@BatchGetDeployments' {} a -> s {deploymentIds = a} :: BatchGetDeployments) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetDeployments where
  type
    AWSResponse BatchGetDeployments =
      BatchGetDeploymentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDeploymentsResponse'
            Prelude.<$> ( x Data..?> "deploymentsInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetDeployments where
  hashWithSalt _salt BatchGetDeployments' {..} =
    _salt `Prelude.hashWithSalt` deploymentIds

instance Prelude.NFData BatchGetDeployments where
  rnf BatchGetDeployments' {..} =
    Prelude.rnf deploymentIds

instance Data.ToHeaders BatchGetDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.BatchGetDeployments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetDeployments where
  toJSON BatchGetDeployments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("deploymentIds" Data..= deploymentIds)
          ]
      )

instance Data.ToPath BatchGetDeployments where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetDeployments where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @BatchGetDeployments@ operation.
--
-- /See:/ 'newBatchGetDeploymentsResponse' smart constructor.
data BatchGetDeploymentsResponse = BatchGetDeploymentsResponse'
  { -- | Information about the deployments.
    deploymentsInfo :: Prelude.Maybe [DeploymentInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentsInfo', 'batchGetDeploymentsResponse_deploymentsInfo' - Information about the deployments.
--
-- 'httpStatus', 'batchGetDeploymentsResponse_httpStatus' - The response's http status code.
newBatchGetDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetDeploymentsResponse
newBatchGetDeploymentsResponse pHttpStatus_ =
  BatchGetDeploymentsResponse'
    { deploymentsInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deployments.
batchGetDeploymentsResponse_deploymentsInfo :: Lens.Lens' BatchGetDeploymentsResponse (Prelude.Maybe [DeploymentInfo])
batchGetDeploymentsResponse_deploymentsInfo = Lens.lens (\BatchGetDeploymentsResponse' {deploymentsInfo} -> deploymentsInfo) (\s@BatchGetDeploymentsResponse' {} a -> s {deploymentsInfo = a} :: BatchGetDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetDeploymentsResponse_httpStatus :: Lens.Lens' BatchGetDeploymentsResponse Prelude.Int
batchGetDeploymentsResponse_httpStatus = Lens.lens (\BatchGetDeploymentsResponse' {httpStatus} -> httpStatus) (\s@BatchGetDeploymentsResponse' {} a -> s {httpStatus = a} :: BatchGetDeploymentsResponse)

instance Prelude.NFData BatchGetDeploymentsResponse where
  rnf BatchGetDeploymentsResponse' {..} =
    Prelude.rnf deploymentsInfo
      `Prelude.seq` Prelude.rnf httpStatus
