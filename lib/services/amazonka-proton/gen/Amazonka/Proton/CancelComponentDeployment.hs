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
-- Module      : Amazonka.Proton.CancelComponentDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel a component deployment (for a component that is in
-- the @IN_PROGRESS@ deployment status).
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
module Amazonka.Proton.CancelComponentDeployment
  ( -- * Creating a Request
    CancelComponentDeployment (..),
    newCancelComponentDeployment,

    -- * Request Lenses
    cancelComponentDeployment_componentName,

    -- * Destructuring the Response
    CancelComponentDeploymentResponse (..),
    newCancelComponentDeploymentResponse,

    -- * Response Lenses
    cancelComponentDeploymentResponse_httpStatus,
    cancelComponentDeploymentResponse_component,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelComponentDeployment' smart constructor.
data CancelComponentDeployment = CancelComponentDeployment'
  { -- | The name of the component with the deployment to cancel.
    componentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelComponentDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'cancelComponentDeployment_componentName' - The name of the component with the deployment to cancel.
newCancelComponentDeployment ::
  -- | 'componentName'
  Prelude.Text ->
  CancelComponentDeployment
newCancelComponentDeployment pComponentName_ =
  CancelComponentDeployment'
    { componentName =
        pComponentName_
    }

-- | The name of the component with the deployment to cancel.
cancelComponentDeployment_componentName :: Lens.Lens' CancelComponentDeployment Prelude.Text
cancelComponentDeployment_componentName = Lens.lens (\CancelComponentDeployment' {componentName} -> componentName) (\s@CancelComponentDeployment' {} a -> s {componentName = a} :: CancelComponentDeployment)

instance Core.AWSRequest CancelComponentDeployment where
  type
    AWSResponse CancelComponentDeployment =
      CancelComponentDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelComponentDeploymentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "component")
      )

instance Prelude.Hashable CancelComponentDeployment where
  hashWithSalt _salt CancelComponentDeployment' {..} =
    _salt `Prelude.hashWithSalt` componentName

instance Prelude.NFData CancelComponentDeployment where
  rnf CancelComponentDeployment' {..} =
    Prelude.rnf componentName

instance Data.ToHeaders CancelComponentDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CancelComponentDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelComponentDeployment where
  toJSON CancelComponentDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("componentName" Data..= componentName)
          ]
      )

instance Data.ToPath CancelComponentDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelComponentDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelComponentDeploymentResponse' smart constructor.
data CancelComponentDeploymentResponse = CancelComponentDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the component with the deployment that is being
    -- canceled.
    component :: Component
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelComponentDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelComponentDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'component', 'cancelComponentDeploymentResponse_component' - The detailed data of the component with the deployment that is being
-- canceled.
newCancelComponentDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'component'
  Component ->
  CancelComponentDeploymentResponse
newCancelComponentDeploymentResponse
  pHttpStatus_
  pComponent_ =
    CancelComponentDeploymentResponse'
      { httpStatus =
          pHttpStatus_,
        component = pComponent_
      }

-- | The response's http status code.
cancelComponentDeploymentResponse_httpStatus :: Lens.Lens' CancelComponentDeploymentResponse Prelude.Int
cancelComponentDeploymentResponse_httpStatus = Lens.lens (\CancelComponentDeploymentResponse' {httpStatus} -> httpStatus) (\s@CancelComponentDeploymentResponse' {} a -> s {httpStatus = a} :: CancelComponentDeploymentResponse)

-- | The detailed data of the component with the deployment that is being
-- canceled.
cancelComponentDeploymentResponse_component :: Lens.Lens' CancelComponentDeploymentResponse Component
cancelComponentDeploymentResponse_component = Lens.lens (\CancelComponentDeploymentResponse' {component} -> component) (\s@CancelComponentDeploymentResponse' {} a -> s {component = a} :: CancelComponentDeploymentResponse)

instance
  Prelude.NFData
    CancelComponentDeploymentResponse
  where
  rnf CancelComponentDeploymentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf component
