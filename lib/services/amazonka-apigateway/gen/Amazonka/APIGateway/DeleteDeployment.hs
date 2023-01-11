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
-- Module      : Amazonka.APIGateway.DeleteDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Deployment resource. Deleting a deployment will only succeed
-- if there are no Stage resources associated with it.
module Amazonka.APIGateway.DeleteDeployment
  ( -- * Creating a Request
    DeleteDeployment (..),
    newDeleteDeployment,

    -- * Request Lenses
    deleteDeployment_restApiId,
    deleteDeployment_deploymentId,

    -- * Destructuring the Response
    DeleteDeploymentResponse (..),
    newDeleteDeploymentResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to delete a Deployment resource.
--
-- /See:/ 'newDeleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the Deployment resource to delete.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteDeployment_restApiId' - The string identifier of the associated RestApi.
--
-- 'deploymentId', 'deleteDeployment_deploymentId' - The identifier of the Deployment resource to delete.
newDeleteDeployment ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'deploymentId'
  Prelude.Text ->
  DeleteDeployment
newDeleteDeployment pRestApiId_ pDeploymentId_ =
  DeleteDeployment'
    { restApiId = pRestApiId_,
      deploymentId = pDeploymentId_
    }

-- | The string identifier of the associated RestApi.
deleteDeployment_restApiId :: Lens.Lens' DeleteDeployment Prelude.Text
deleteDeployment_restApiId = Lens.lens (\DeleteDeployment' {restApiId} -> restApiId) (\s@DeleteDeployment' {} a -> s {restApiId = a} :: DeleteDeployment)

-- | The identifier of the Deployment resource to delete.
deleteDeployment_deploymentId :: Lens.Lens' DeleteDeployment Prelude.Text
deleteDeployment_deploymentId = Lens.lens (\DeleteDeployment' {deploymentId} -> deploymentId) (\s@DeleteDeployment' {} a -> s {deploymentId = a} :: DeleteDeployment)

instance Core.AWSRequest DeleteDeployment where
  type
    AWSResponse DeleteDeployment =
      DeleteDeploymentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteDeploymentResponse'

instance Prelude.Hashable DeleteDeployment where
  hashWithSalt _salt DeleteDeployment' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData DeleteDeployment where
  rnf DeleteDeployment' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf deploymentId

instance Data.ToHeaders DeleteDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteDeployment where
  toPath DeleteDeployment' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/deployments/",
        Data.toBS deploymentId
      ]

instance Data.ToQuery DeleteDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeploymentResponse' smart constructor.
data DeleteDeploymentResponse = DeleteDeploymentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDeploymentResponse ::
  DeleteDeploymentResponse
newDeleteDeploymentResponse =
  DeleteDeploymentResponse'

instance Prelude.NFData DeleteDeploymentResponse where
  rnf _ = ()
