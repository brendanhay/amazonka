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
-- Module      : Amazonka.GreengrassV2.DeleteDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment. To delete an active deployment, you must first
-- cancel it. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CancelDeployment.html CancelDeployment>.
--
-- Deleting a deployment doesn\'t affect core devices that run that
-- deployment, because core devices store the deployment\'s configuration
-- on the device. Additionally, core devices can roll back to a previous
-- deployment that has been deleted.
module Amazonka.GreengrassV2.DeleteDeployment
  ( -- * Creating a Request
    DeleteDeployment (..),
    newDeleteDeployment,

    -- * Request Lenses
    deleteDeployment_deploymentId,

    -- * Destructuring the Response
    DeleteDeploymentResponse (..),
    newDeleteDeploymentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { -- | The ID of the deployment.
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
-- 'deploymentId', 'deleteDeployment_deploymentId' - The ID of the deployment.
newDeleteDeployment ::
  -- | 'deploymentId'
  Prelude.Text ->
  DeleteDeployment
newDeleteDeployment pDeploymentId_ =
  DeleteDeployment' {deploymentId = pDeploymentId_}

-- | The ID of the deployment.
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
    _salt `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData DeleteDeployment where
  rnf DeleteDeployment' {..} = Prelude.rnf deploymentId

instance Data.ToHeaders DeleteDeployment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDeployment where
  toPath DeleteDeployment' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/deployments/",
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
