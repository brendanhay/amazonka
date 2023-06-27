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
-- Module      : Amazonka.SageMaker.UpdateWorkforce
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to update your workforce. You can use this operation
-- to require that workers use specific IP addresses to work on tasks and
-- to update your OpenID Connect (OIDC) Identity Provider (IdP) workforce
-- configuration.
--
-- The worker portal is now supported in VPC and public internet.
--
-- Use @SourceIpConfig@ to restrict worker access to tasks to a specific
-- range of IP addresses. You specify allowed IP addresses by creating a
-- list of up to ten
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>.
-- By default, a workforce isn\'t restricted to specific IP addresses. If
-- you specify a range of IP addresses, workers who attempt to access tasks
-- using any IP address outside the specified range are denied and get a
-- @Not Found@ error message on the worker portal.
--
-- To restrict access to all the workers in public internet, add the
-- @SourceIpConfig@ CIDR value as \"10.0.0.0\/16\".
--
-- Amazon SageMaker does not support Source Ip restriction for worker
-- portals in VPC.
--
-- Use @OidcConfig@ to update the configuration of a workforce created
-- using your own OIDC IdP.
--
-- You can only update your OIDC IdP configuration when there are no work
-- teams associated with your workforce. You can delete work teams using
-- the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteWorkteam.html DeleteWorkteam>
-- operation.
--
-- After restricting access to a range of IP addresses or updating your
-- OIDC IdP configuration with this operation, you can view details about
-- your update workforce using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeWorkforce.html DescribeWorkforce>
-- operation.
--
-- This operation only applies to private workforces.
module Amazonka.SageMaker.UpdateWorkforce
  ( -- * Creating a Request
    UpdateWorkforce (..),
    newUpdateWorkforce,

    -- * Request Lenses
    updateWorkforce_oidcConfig,
    updateWorkforce_sourceIpConfig,
    updateWorkforce_workforceVpcConfig,
    updateWorkforce_workforceName,

    -- * Destructuring the Response
    UpdateWorkforceResponse (..),
    newUpdateWorkforceResponse,

    -- * Response Lenses
    updateWorkforceResponse_httpStatus,
    updateWorkforceResponse_workforce,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateWorkforce' smart constructor.
data UpdateWorkforce = UpdateWorkforce'
  { -- | Use this parameter to update your OIDC Identity Provider (IdP)
    -- configuration for a workforce made using your own IdP.
    oidcConfig :: Prelude.Maybe OidcConfig,
    -- | A list of one to ten worker IP address ranges
    -- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
    -- that can be used to access tasks assigned to this workforce.
    --
    -- Maximum: Ten CIDR values
    sourceIpConfig :: Prelude.Maybe SourceIpConfig,
    -- | Use this parameter to update your VPC configuration for a workforce.
    workforceVpcConfig :: Prelude.Maybe WorkforceVpcConfigRequest,
    -- | The name of the private workforce that you want to update. You can find
    -- your workforce name by using the
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ListWorkforces.html ListWorkforces>
    -- operation.
    workforceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oidcConfig', 'updateWorkforce_oidcConfig' - Use this parameter to update your OIDC Identity Provider (IdP)
-- configuration for a workforce made using your own IdP.
--
-- 'sourceIpConfig', 'updateWorkforce_sourceIpConfig' - A list of one to ten worker IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- that can be used to access tasks assigned to this workforce.
--
-- Maximum: Ten CIDR values
--
-- 'workforceVpcConfig', 'updateWorkforce_workforceVpcConfig' - Use this parameter to update your VPC configuration for a workforce.
--
-- 'workforceName', 'updateWorkforce_workforceName' - The name of the private workforce that you want to update. You can find
-- your workforce name by using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ListWorkforces.html ListWorkforces>
-- operation.
newUpdateWorkforce ::
  -- | 'workforceName'
  Prelude.Text ->
  UpdateWorkforce
newUpdateWorkforce pWorkforceName_ =
  UpdateWorkforce'
    { oidcConfig = Prelude.Nothing,
      sourceIpConfig = Prelude.Nothing,
      workforceVpcConfig = Prelude.Nothing,
      workforceName = pWorkforceName_
    }

-- | Use this parameter to update your OIDC Identity Provider (IdP)
-- configuration for a workforce made using your own IdP.
updateWorkforce_oidcConfig :: Lens.Lens' UpdateWorkforce (Prelude.Maybe OidcConfig)
updateWorkforce_oidcConfig = Lens.lens (\UpdateWorkforce' {oidcConfig} -> oidcConfig) (\s@UpdateWorkforce' {} a -> s {oidcConfig = a} :: UpdateWorkforce)

-- | A list of one to ten worker IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- that can be used to access tasks assigned to this workforce.
--
-- Maximum: Ten CIDR values
updateWorkforce_sourceIpConfig :: Lens.Lens' UpdateWorkforce (Prelude.Maybe SourceIpConfig)
updateWorkforce_sourceIpConfig = Lens.lens (\UpdateWorkforce' {sourceIpConfig} -> sourceIpConfig) (\s@UpdateWorkforce' {} a -> s {sourceIpConfig = a} :: UpdateWorkforce)

-- | Use this parameter to update your VPC configuration for a workforce.
updateWorkforce_workforceVpcConfig :: Lens.Lens' UpdateWorkforce (Prelude.Maybe WorkforceVpcConfigRequest)
updateWorkforce_workforceVpcConfig = Lens.lens (\UpdateWorkforce' {workforceVpcConfig} -> workforceVpcConfig) (\s@UpdateWorkforce' {} a -> s {workforceVpcConfig = a} :: UpdateWorkforce)

-- | The name of the private workforce that you want to update. You can find
-- your workforce name by using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ListWorkforces.html ListWorkforces>
-- operation.
updateWorkforce_workforceName :: Lens.Lens' UpdateWorkforce Prelude.Text
updateWorkforce_workforceName = Lens.lens (\UpdateWorkforce' {workforceName} -> workforceName) (\s@UpdateWorkforce' {} a -> s {workforceName = a} :: UpdateWorkforce)

instance Core.AWSRequest UpdateWorkforce where
  type
    AWSResponse UpdateWorkforce =
      UpdateWorkforceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkforceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Workforce")
      )

instance Prelude.Hashable UpdateWorkforce where
  hashWithSalt _salt UpdateWorkforce' {..} =
    _salt
      `Prelude.hashWithSalt` oidcConfig
      `Prelude.hashWithSalt` sourceIpConfig
      `Prelude.hashWithSalt` workforceVpcConfig
      `Prelude.hashWithSalt` workforceName

instance Prelude.NFData UpdateWorkforce where
  rnf UpdateWorkforce' {..} =
    Prelude.rnf oidcConfig
      `Prelude.seq` Prelude.rnf sourceIpConfig
      `Prelude.seq` Prelude.rnf workforceVpcConfig
      `Prelude.seq` Prelude.rnf workforceName

instance Data.ToHeaders UpdateWorkforce where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateWorkforce" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkforce where
  toJSON UpdateWorkforce' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OidcConfig" Data..=) Prelude.<$> oidcConfig,
            ("SourceIpConfig" Data..=)
              Prelude.<$> sourceIpConfig,
            ("WorkforceVpcConfig" Data..=)
              Prelude.<$> workforceVpcConfig,
            Prelude.Just
              ("WorkforceName" Data..= workforceName)
          ]
      )

instance Data.ToPath UpdateWorkforce where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWorkforce where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkforceResponse' smart constructor.
data UpdateWorkforceResponse = UpdateWorkforceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A single private workforce. You can create one private work force in
    -- each Amazon Web Services Region. By default, any workforce-related API
    -- operation used in a specific region will apply to the workforce created
    -- in that region. To learn how to create a private workforce, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
    workforce :: Workforce
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkforceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkforceResponse_httpStatus' - The response's http status code.
--
-- 'workforce', 'updateWorkforceResponse_workforce' - A single private workforce. You can create one private work force in
-- each Amazon Web Services Region. By default, any workforce-related API
-- operation used in a specific region will apply to the workforce created
-- in that region. To learn how to create a private workforce, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
newUpdateWorkforceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workforce'
  Workforce ->
  UpdateWorkforceResponse
newUpdateWorkforceResponse pHttpStatus_ pWorkforce_ =
  UpdateWorkforceResponse'
    { httpStatus = pHttpStatus_,
      workforce = pWorkforce_
    }

-- | The response's http status code.
updateWorkforceResponse_httpStatus :: Lens.Lens' UpdateWorkforceResponse Prelude.Int
updateWorkforceResponse_httpStatus = Lens.lens (\UpdateWorkforceResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkforceResponse' {} a -> s {httpStatus = a} :: UpdateWorkforceResponse)

-- | A single private workforce. You can create one private work force in
-- each Amazon Web Services Region. By default, any workforce-related API
-- operation used in a specific region will apply to the workforce created
-- in that region. To learn how to create a private workforce, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
updateWorkforceResponse_workforce :: Lens.Lens' UpdateWorkforceResponse Workforce
updateWorkforceResponse_workforce = Lens.lens (\UpdateWorkforceResponse' {workforce} -> workforce) (\s@UpdateWorkforceResponse' {} a -> s {workforce = a} :: UpdateWorkforceResponse)

instance Prelude.NFData UpdateWorkforceResponse where
  rnf UpdateWorkforceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workforce
