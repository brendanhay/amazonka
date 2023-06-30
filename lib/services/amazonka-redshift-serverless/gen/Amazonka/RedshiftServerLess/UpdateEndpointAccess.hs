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
-- Module      : Amazonka.RedshiftServerLess.UpdateEndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon Redshift Serverless managed endpoint.
module Amazonka.RedshiftServerLess.UpdateEndpointAccess
  ( -- * Creating a Request
    UpdateEndpointAccess (..),
    newUpdateEndpointAccess,

    -- * Request Lenses
    updateEndpointAccess_vpcSecurityGroupIds,
    updateEndpointAccess_endpointName,

    -- * Destructuring the Response
    UpdateEndpointAccessResponse (..),
    newUpdateEndpointAccessResponse,

    -- * Response Lenses
    updateEndpointAccessResponse_endpoint,
    updateEndpointAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEndpointAccess' smart constructor.
data UpdateEndpointAccess = UpdateEndpointAccess'
  { -- | The list of VPC security groups associated with the endpoint after the
    -- endpoint is modified.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the VPC endpoint to update.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcSecurityGroupIds', 'updateEndpointAccess_vpcSecurityGroupIds' - The list of VPC security groups associated with the endpoint after the
-- endpoint is modified.
--
-- 'endpointName', 'updateEndpointAccess_endpointName' - The name of the VPC endpoint to update.
newUpdateEndpointAccess ::
  -- | 'endpointName'
  Prelude.Text ->
  UpdateEndpointAccess
newUpdateEndpointAccess pEndpointName_ =
  UpdateEndpointAccess'
    { vpcSecurityGroupIds =
        Prelude.Nothing,
      endpointName = pEndpointName_
    }

-- | The list of VPC security groups associated with the endpoint after the
-- endpoint is modified.
updateEndpointAccess_vpcSecurityGroupIds :: Lens.Lens' UpdateEndpointAccess (Prelude.Maybe [Prelude.Text])
updateEndpointAccess_vpcSecurityGroupIds = Lens.lens (\UpdateEndpointAccess' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@UpdateEndpointAccess' {} a -> s {vpcSecurityGroupIds = a} :: UpdateEndpointAccess) Prelude.. Lens.mapping Lens.coerced

-- | The name of the VPC endpoint to update.
updateEndpointAccess_endpointName :: Lens.Lens' UpdateEndpointAccess Prelude.Text
updateEndpointAccess_endpointName = Lens.lens (\UpdateEndpointAccess' {endpointName} -> endpointName) (\s@UpdateEndpointAccess' {} a -> s {endpointName = a} :: UpdateEndpointAccess)

instance Core.AWSRequest UpdateEndpointAccess where
  type
    AWSResponse UpdateEndpointAccess =
      UpdateEndpointAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointAccessResponse'
            Prelude.<$> (x Data..?> "endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEndpointAccess where
  hashWithSalt _salt UpdateEndpointAccess' {..} =
    _salt
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` endpointName

instance Prelude.NFData UpdateEndpointAccess where
  rnf UpdateEndpointAccess' {..} =
    Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf endpointName

instance Data.ToHeaders UpdateEndpointAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.UpdateEndpointAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEndpointAccess where
  toJSON UpdateEndpointAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("vpcSecurityGroupIds" Data..=)
              Prelude.<$> vpcSecurityGroupIds,
            Prelude.Just ("endpointName" Data..= endpointName)
          ]
      )

instance Data.ToPath UpdateEndpointAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEndpointAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointAccessResponse' smart constructor.
data UpdateEndpointAccessResponse = UpdateEndpointAccessResponse'
  { -- | The updated VPC endpoint.
    endpoint :: Prelude.Maybe EndpointAccess,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'updateEndpointAccessResponse_endpoint' - The updated VPC endpoint.
--
-- 'httpStatus', 'updateEndpointAccessResponse_httpStatus' - The response's http status code.
newUpdateEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEndpointAccessResponse
newUpdateEndpointAccessResponse pHttpStatus_ =
  UpdateEndpointAccessResponse'
    { endpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated VPC endpoint.
updateEndpointAccessResponse_endpoint :: Lens.Lens' UpdateEndpointAccessResponse (Prelude.Maybe EndpointAccess)
updateEndpointAccessResponse_endpoint = Lens.lens (\UpdateEndpointAccessResponse' {endpoint} -> endpoint) (\s@UpdateEndpointAccessResponse' {} a -> s {endpoint = a} :: UpdateEndpointAccessResponse)

-- | The response's http status code.
updateEndpointAccessResponse_httpStatus :: Lens.Lens' UpdateEndpointAccessResponse Prelude.Int
updateEndpointAccessResponse_httpStatus = Lens.lens (\UpdateEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointAccessResponse' {} a -> s {httpStatus = a} :: UpdateEndpointAccessResponse)

instance Prelude.NFData UpdateEndpointAccessResponse where
  rnf UpdateEndpointAccessResponse' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf httpStatus
