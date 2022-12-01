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
-- Module      : Amazonka.DevOpsGuru.UpdateServiceIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables integration with a service that can be integrated
-- with DevOps Guru. The one service that can be integrated with DevOps
-- Guru is Amazon Web Services Systems Manager, which can be used to create
-- an OpsItem for each generated insight.
module Amazonka.DevOpsGuru.UpdateServiceIntegration
  ( -- * Creating a Request
    UpdateServiceIntegration (..),
    newUpdateServiceIntegration,

    -- * Request Lenses
    updateServiceIntegration_serviceIntegration,

    -- * Destructuring the Response
    UpdateServiceIntegrationResponse (..),
    newUpdateServiceIntegrationResponse,

    -- * Response Lenses
    updateServiceIntegrationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceIntegration' smart constructor.
data UpdateServiceIntegration = UpdateServiceIntegration'
  { -- | An @IntegratedServiceConfig@ object used to specify the integrated
    -- service you want to update, and whether you want to update it to enabled
    -- or disabled.
    serviceIntegration :: UpdateServiceIntegrationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceIntegration', 'updateServiceIntegration_serviceIntegration' - An @IntegratedServiceConfig@ object used to specify the integrated
-- service you want to update, and whether you want to update it to enabled
-- or disabled.
newUpdateServiceIntegration ::
  -- | 'serviceIntegration'
  UpdateServiceIntegrationConfig ->
  UpdateServiceIntegration
newUpdateServiceIntegration pServiceIntegration_ =
  UpdateServiceIntegration'
    { serviceIntegration =
        pServiceIntegration_
    }

-- | An @IntegratedServiceConfig@ object used to specify the integrated
-- service you want to update, and whether you want to update it to enabled
-- or disabled.
updateServiceIntegration_serviceIntegration :: Lens.Lens' UpdateServiceIntegration UpdateServiceIntegrationConfig
updateServiceIntegration_serviceIntegration = Lens.lens (\UpdateServiceIntegration' {serviceIntegration} -> serviceIntegration) (\s@UpdateServiceIntegration' {} a -> s {serviceIntegration = a} :: UpdateServiceIntegration)

instance Core.AWSRequest UpdateServiceIntegration where
  type
    AWSResponse UpdateServiceIntegration =
      UpdateServiceIntegrationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateServiceIntegrationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceIntegration where
  hashWithSalt _salt UpdateServiceIntegration' {..} =
    _salt `Prelude.hashWithSalt` serviceIntegration

instance Prelude.NFData UpdateServiceIntegration where
  rnf UpdateServiceIntegration' {..} =
    Prelude.rnf serviceIntegration

instance Core.ToHeaders UpdateServiceIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateServiceIntegration where
  toJSON UpdateServiceIntegration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ServiceIntegration" Core..= serviceIntegration)
          ]
      )

instance Core.ToPath UpdateServiceIntegration where
  toPath = Prelude.const "/service-integrations"

instance Core.ToQuery UpdateServiceIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceIntegrationResponse' smart constructor.
data UpdateServiceIntegrationResponse = UpdateServiceIntegrationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServiceIntegrationResponse_httpStatus' - The response's http status code.
newUpdateServiceIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceIntegrationResponse
newUpdateServiceIntegrationResponse pHttpStatus_ =
  UpdateServiceIntegrationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateServiceIntegrationResponse_httpStatus :: Lens.Lens' UpdateServiceIntegrationResponse Prelude.Int
updateServiceIntegrationResponse_httpStatus = Lens.lens (\UpdateServiceIntegrationResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceIntegrationResponse' {} a -> s {httpStatus = a} :: UpdateServiceIntegrationResponse)

instance
  Prelude.NFData
    UpdateServiceIntegrationResponse
  where
  rnf UpdateServiceIntegrationResponse' {..} =
    Prelude.rnf httpStatus
