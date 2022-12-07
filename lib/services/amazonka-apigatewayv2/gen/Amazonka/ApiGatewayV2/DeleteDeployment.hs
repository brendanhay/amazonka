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
-- Module      : Amazonka.ApiGatewayV2.DeleteDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Deployment.
module Amazonka.ApiGatewayV2.DeleteDeployment
  ( -- * Creating a Request
    DeleteDeployment (..),
    newDeleteDeployment,

    -- * Request Lenses
    deleteDeployment_apiId,
    deleteDeployment_deploymentId,

    -- * Destructuring the Response
    DeleteDeploymentResponse (..),
    newDeleteDeploymentResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The deployment ID.
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
-- 'apiId', 'deleteDeployment_apiId' - The API identifier.
--
-- 'deploymentId', 'deleteDeployment_deploymentId' - The deployment ID.
newDeleteDeployment ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'deploymentId'
  Prelude.Text ->
  DeleteDeployment
newDeleteDeployment pApiId_ pDeploymentId_ =
  DeleteDeployment'
    { apiId = pApiId_,
      deploymentId = pDeploymentId_
    }

-- | The API identifier.
deleteDeployment_apiId :: Lens.Lens' DeleteDeployment Prelude.Text
deleteDeployment_apiId = Lens.lens (\DeleteDeployment' {apiId} -> apiId) (\s@DeleteDeployment' {} a -> s {apiId = a} :: DeleteDeployment)

-- | The deployment ID.
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
    _salt `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData DeleteDeployment where
  rnf DeleteDeployment' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf deploymentId

instance Data.ToHeaders DeleteDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDeployment where
  toPath DeleteDeployment' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
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
