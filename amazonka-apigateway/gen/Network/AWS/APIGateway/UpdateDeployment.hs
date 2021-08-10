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
-- Module      : Network.AWS.APIGateway.UpdateDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a Deployment resource.
module Network.AWS.APIGateway.UpdateDeployment
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
    deployment_createdDate,
    deployment_id,
    deployment_apiSummary,
    deployment_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to change information about a Deployment resource.
--
-- /See:/ 'newUpdateDeployment' smart constructor.
data UpdateDeployment = UpdateDeployment'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
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
-- 'patchOperations', 'updateDeployment_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateDeployment_restApiId' - [Required] The string identifier of the associated RestApi.
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

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateDeployment_patchOperations :: Lens.Lens' UpdateDeployment (Prelude.Maybe [PatchOperation])
updateDeployment_patchOperations = Lens.lens (\UpdateDeployment' {patchOperations} -> patchOperations) (\s@UpdateDeployment' {} a -> s {patchOperations = a} :: UpdateDeployment) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateDeployment_restApiId :: Lens.Lens' UpdateDeployment Prelude.Text
updateDeployment_restApiId = Lens.lens (\UpdateDeployment' {restApiId} -> restApiId) (\s@UpdateDeployment' {} a -> s {restApiId = a} :: UpdateDeployment)

-- | The replacement identifier for the Deployment resource to change
-- information about.
updateDeployment_deploymentId :: Lens.Lens' UpdateDeployment Prelude.Text
updateDeployment_deploymentId = Lens.lens (\UpdateDeployment' {deploymentId} -> deploymentId) (\s@UpdateDeployment' {} a -> s {deploymentId = a} :: UpdateDeployment)

instance Core.AWSRequest UpdateDeployment where
  type AWSResponse UpdateDeployment = Deployment
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateDeployment

instance Prelude.NFData UpdateDeployment

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
