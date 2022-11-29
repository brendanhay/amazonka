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
-- Module      : Amazonka.APIGateway.UpdateDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a Deployment resource.
module Amazonka.APIGateway.UpdateDeployment
  ( -- * Creating a Request
    UpdateDeployment (..),
    newUpdateDeployment,

    -- * Request Lenses
    updateDeployment_patchOperations,
    updateDeployment_restApiId,
    updateDeployment_deploymentId,

    -- * Destructuring the Response
    Deployment (..),
    newDeployment,

    -- * Response Lenses
    deployment_description,
    deployment_id,
    deployment_createdDate,
    deployment_apiSummary,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to change information about a Deployment resource.
--
-- /See:/ 'newUpdateDeployment' smart constructor.
data UpdateDeployment = UpdateDeployment'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The replacement identifier for the Deployment resource to change
    -- information about.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateDeployment_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateDeployment_restApiId' - The string identifier of the associated RestApi.
--
-- 'deploymentId', 'updateDeployment_deploymentId' - The replacement identifier for the Deployment resource to change
-- information about.
newUpdateDeployment ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'deploymentId'
  Prelude.Text ->
  UpdateDeployment
newUpdateDeployment pRestApiId_ pDeploymentId_ =
  UpdateDeployment'
    { patchOperations =
        Prelude.Nothing,
      restApiId = pRestApiId_,
      deploymentId = pDeploymentId_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateDeployment_patchOperations :: Lens.Lens' UpdateDeployment (Prelude.Maybe [PatchOperation])
updateDeployment_patchOperations = Lens.lens (\UpdateDeployment' {patchOperations} -> patchOperations) (\s@UpdateDeployment' {} a -> s {patchOperations = a} :: UpdateDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateDeployment_restApiId :: Lens.Lens' UpdateDeployment Prelude.Text
updateDeployment_restApiId = Lens.lens (\UpdateDeployment' {restApiId} -> restApiId) (\s@UpdateDeployment' {} a -> s {restApiId = a} :: UpdateDeployment)

-- | The replacement identifier for the Deployment resource to change
-- information about.
updateDeployment_deploymentId :: Lens.Lens' UpdateDeployment Prelude.Text
updateDeployment_deploymentId = Lens.lens (\UpdateDeployment' {deploymentId} -> deploymentId) (\s@UpdateDeployment' {} a -> s {deploymentId = a} :: UpdateDeployment)

instance Core.AWSRequest UpdateDeployment where
  type AWSResponse UpdateDeployment = Deployment
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateDeployment where
  hashWithSalt _salt UpdateDeployment' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData UpdateDeployment where
  rnf UpdateDeployment' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf deploymentId

instance Core.ToHeaders UpdateDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateDeployment where
  toJSON UpdateDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateDeployment where
  toPath UpdateDeployment' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/deployments/",
        Core.toBS deploymentId
      ]

instance Core.ToQuery UpdateDeployment where
  toQuery = Prelude.const Prelude.mempty
