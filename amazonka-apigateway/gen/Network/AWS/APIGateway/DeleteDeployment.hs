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
-- Module      : Network.AWS.APIGateway.DeleteDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Deployment resource. Deleting a deployment will only succeed
-- if there are no Stage resources associated with it.
module Network.AWS.APIGateway.DeleteDeployment
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to delete a Deployment resource.
--
-- /See:/ 'newDeleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of the Deployment resource to delete.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteDeployment_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'deploymentId', 'deleteDeployment_deploymentId' - [Required] The identifier of the Deployment resource to delete.
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

-- | [Required] The string identifier of the associated RestApi.
deleteDeployment_restApiId :: Lens.Lens' DeleteDeployment Prelude.Text
deleteDeployment_restApiId = Lens.lens (\DeleteDeployment' {restApiId} -> restApiId) (\s@DeleteDeployment' {} a -> s {restApiId = a} :: DeleteDeployment)

-- | [Required] The identifier of the Deployment resource to delete.
deleteDeployment_deploymentId :: Lens.Lens' DeleteDeployment Prelude.Text
deleteDeployment_deploymentId = Lens.lens (\DeleteDeployment' {deploymentId} -> deploymentId) (\s@DeleteDeployment' {} a -> s {deploymentId = a} :: DeleteDeployment)

instance Prelude.AWSRequest DeleteDeployment where
  type Rs DeleteDeployment = DeleteDeploymentResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteDeploymentResponse'

instance Prelude.Hashable DeleteDeployment

instance Prelude.NFData DeleteDeployment

instance Prelude.ToHeaders DeleteDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteDeployment where
  toPath DeleteDeployment' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/deployments/",
        Prelude.toBS deploymentId
      ]

instance Prelude.ToQuery DeleteDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeploymentResponse' smart constructor.
data DeleteDeploymentResponse = DeleteDeploymentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDeploymentResponse ::
  DeleteDeploymentResponse
newDeleteDeploymentResponse =
  DeleteDeploymentResponse'

instance Prelude.NFData DeleteDeploymentResponse
