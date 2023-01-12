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
-- Module      : Amazonka.Proton.CancelEnvironmentDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel an environment deployment on an UpdateEnvironment
-- action, if the deployment is @IN_PROGRESS@. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-update.html Update an environment>
-- in the /Proton User guide/.
--
-- The following list includes potential cancellation scenarios.
--
-- -   If the cancellation attempt succeeds, the resulting deployment state
--     is @CANCELLED@.
--
-- -   If the cancellation attempt fails, the resulting deployment state is
--     @FAILED@.
--
-- -   If the current UpdateEnvironment action succeeds before the
--     cancellation attempt starts, the resulting deployment state is
--     @SUCCEEDED@ and the cancellation attempt has no effect.
module Amazonka.Proton.CancelEnvironmentDeployment
  ( -- * Creating a Request
    CancelEnvironmentDeployment (..),
    newCancelEnvironmentDeployment,

    -- * Request Lenses
    cancelEnvironmentDeployment_environmentName,

    -- * Destructuring the Response
    CancelEnvironmentDeploymentResponse (..),
    newCancelEnvironmentDeploymentResponse,

    -- * Response Lenses
    cancelEnvironmentDeploymentResponse_httpStatus,
    cancelEnvironmentDeploymentResponse_environment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelEnvironmentDeployment' smart constructor.
data CancelEnvironmentDeployment = CancelEnvironmentDeployment'
  { -- | The name of the environment with the deployment to cancel.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelEnvironmentDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'cancelEnvironmentDeployment_environmentName' - The name of the environment with the deployment to cancel.
newCancelEnvironmentDeployment ::
  -- | 'environmentName'
  Prelude.Text ->
  CancelEnvironmentDeployment
newCancelEnvironmentDeployment pEnvironmentName_ =
  CancelEnvironmentDeployment'
    { environmentName =
        pEnvironmentName_
    }

-- | The name of the environment with the deployment to cancel.
cancelEnvironmentDeployment_environmentName :: Lens.Lens' CancelEnvironmentDeployment Prelude.Text
cancelEnvironmentDeployment_environmentName = Lens.lens (\CancelEnvironmentDeployment' {environmentName} -> environmentName) (\s@CancelEnvironmentDeployment' {} a -> s {environmentName = a} :: CancelEnvironmentDeployment)

instance Core.AWSRequest CancelEnvironmentDeployment where
  type
    AWSResponse CancelEnvironmentDeployment =
      CancelEnvironmentDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelEnvironmentDeploymentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environment")
      )

instance Prelude.Hashable CancelEnvironmentDeployment where
  hashWithSalt _salt CancelEnvironmentDeployment' {..} =
    _salt `Prelude.hashWithSalt` environmentName

instance Prelude.NFData CancelEnvironmentDeployment where
  rnf CancelEnvironmentDeployment' {..} =
    Prelude.rnf environmentName

instance Data.ToHeaders CancelEnvironmentDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CancelEnvironmentDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelEnvironmentDeployment where
  toJSON CancelEnvironmentDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentName" Data..= environmentName)
          ]
      )

instance Data.ToPath CancelEnvironmentDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelEnvironmentDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelEnvironmentDeploymentResponse' smart constructor.
data CancelEnvironmentDeploymentResponse = CancelEnvironmentDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment summary data that\'s returned by Proton.
    environment :: Environment
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelEnvironmentDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelEnvironmentDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'environment', 'cancelEnvironmentDeploymentResponse_environment' - The environment summary data that\'s returned by Proton.
newCancelEnvironmentDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environment'
  Environment ->
  CancelEnvironmentDeploymentResponse
newCancelEnvironmentDeploymentResponse
  pHttpStatus_
  pEnvironment_ =
    CancelEnvironmentDeploymentResponse'
      { httpStatus =
          pHttpStatus_,
        environment = pEnvironment_
      }

-- | The response's http status code.
cancelEnvironmentDeploymentResponse_httpStatus :: Lens.Lens' CancelEnvironmentDeploymentResponse Prelude.Int
cancelEnvironmentDeploymentResponse_httpStatus = Lens.lens (\CancelEnvironmentDeploymentResponse' {httpStatus} -> httpStatus) (\s@CancelEnvironmentDeploymentResponse' {} a -> s {httpStatus = a} :: CancelEnvironmentDeploymentResponse)

-- | The environment summary data that\'s returned by Proton.
cancelEnvironmentDeploymentResponse_environment :: Lens.Lens' CancelEnvironmentDeploymentResponse Environment
cancelEnvironmentDeploymentResponse_environment = Lens.lens (\CancelEnvironmentDeploymentResponse' {environment} -> environment) (\s@CancelEnvironmentDeploymentResponse' {} a -> s {environment = a} :: CancelEnvironmentDeploymentResponse)

instance
  Prelude.NFData
    CancelEnvironmentDeploymentResponse
  where
  rnf CancelEnvironmentDeploymentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environment
