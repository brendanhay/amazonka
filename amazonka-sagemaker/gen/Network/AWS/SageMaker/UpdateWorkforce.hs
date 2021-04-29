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
-- Module      : Network.AWS.SageMaker.UpdateWorkforce
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- Use @SourceIpConfig@ to restrict worker access to tasks to a specific
-- range of IP addresses. You specify allowed IP addresses by creating a
-- list of up to ten
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>.
-- By default, a workforce isn\'t restricted to specific IP addresses. If
-- you specify a range of IP addresses, workers who attempt to access tasks
-- using any IP address outside the specified range are denied and get a
-- @Not Found@ error message on the worker portal.
--
-- Use @OidcConfig@ to update the configuration of a workforce created
-- using your own OIDC IdP.
--
-- You can only update your OIDC IdP configuration when there are no work
-- teams associated with your workforce. You can delete work teams using
-- the operation.
--
-- After restricting access to a range of IP addresses or updating your
-- OIDC IdP configuration with this operation, you can view details about
-- your update workforce using the operation.
--
-- This operation only applies to private workforces.
module Network.AWS.SageMaker.UpdateWorkforce
  ( -- * Creating a Request
    UpdateWorkforce (..),
    newUpdateWorkforce,

    -- * Request Lenses
    updateWorkforce_sourceIpConfig,
    updateWorkforce_oidcConfig,
    updateWorkforce_workforceName,

    -- * Destructuring the Response
    UpdateWorkforceResponse (..),
    newUpdateWorkforceResponse,

    -- * Response Lenses
    updateWorkforceResponse_httpStatus,
    updateWorkforceResponse_workforce,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateWorkforce' smart constructor.
data UpdateWorkforce = UpdateWorkforce'
  { -- | A list of one to ten worker IP address ranges
    -- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
    -- that can be used to access tasks assigned to this workforce.
    --
    -- Maximum: Ten CIDR values
    sourceIpConfig :: Prelude.Maybe SourceIpConfig,
    -- | Use this parameter to update your OIDC Identity Provider (IdP)
    -- configuration for a workforce made using your own IdP.
    oidcConfig :: Prelude.Maybe OidcConfig,
    -- | The name of the private workforce that you want to update. You can find
    -- your workforce name by using the operation.
    workforceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceIpConfig', 'updateWorkforce_sourceIpConfig' - A list of one to ten worker IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- that can be used to access tasks assigned to this workforce.
--
-- Maximum: Ten CIDR values
--
-- 'oidcConfig', 'updateWorkforce_oidcConfig' - Use this parameter to update your OIDC Identity Provider (IdP)
-- configuration for a workforce made using your own IdP.
--
-- 'workforceName', 'updateWorkforce_workforceName' - The name of the private workforce that you want to update. You can find
-- your workforce name by using the operation.
newUpdateWorkforce ::
  -- | 'workforceName'
  Prelude.Text ->
  UpdateWorkforce
newUpdateWorkforce pWorkforceName_ =
  UpdateWorkforce'
    { sourceIpConfig = Prelude.Nothing,
      oidcConfig = Prelude.Nothing,
      workforceName = pWorkforceName_
    }

-- | A list of one to ten worker IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- that can be used to access tasks assigned to this workforce.
--
-- Maximum: Ten CIDR values
updateWorkforce_sourceIpConfig :: Lens.Lens' UpdateWorkforce (Prelude.Maybe SourceIpConfig)
updateWorkforce_sourceIpConfig = Lens.lens (\UpdateWorkforce' {sourceIpConfig} -> sourceIpConfig) (\s@UpdateWorkforce' {} a -> s {sourceIpConfig = a} :: UpdateWorkforce)

-- | Use this parameter to update your OIDC Identity Provider (IdP)
-- configuration for a workforce made using your own IdP.
updateWorkforce_oidcConfig :: Lens.Lens' UpdateWorkforce (Prelude.Maybe OidcConfig)
updateWorkforce_oidcConfig = Lens.lens (\UpdateWorkforce' {oidcConfig} -> oidcConfig) (\s@UpdateWorkforce' {} a -> s {oidcConfig = a} :: UpdateWorkforce)

-- | The name of the private workforce that you want to update. You can find
-- your workforce name by using the operation.
updateWorkforce_workforceName :: Lens.Lens' UpdateWorkforce Prelude.Text
updateWorkforce_workforceName = Lens.lens (\UpdateWorkforce' {workforceName} -> workforceName) (\s@UpdateWorkforce' {} a -> s {workforceName = a} :: UpdateWorkforce)

instance Prelude.AWSRequest UpdateWorkforce where
  type Rs UpdateWorkforce = UpdateWorkforceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkforceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Workforce")
      )

instance Prelude.Hashable UpdateWorkforce

instance Prelude.NFData UpdateWorkforce

instance Prelude.ToHeaders UpdateWorkforce where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.UpdateWorkforce" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateWorkforce where
  toJSON UpdateWorkforce' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SourceIpConfig" Prelude..=)
              Prelude.<$> sourceIpConfig,
            ("OidcConfig" Prelude..=) Prelude.<$> oidcConfig,
            Prelude.Just
              ("WorkforceName" Prelude..= workforceName)
          ]
      )

instance Prelude.ToPath UpdateWorkforce where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateWorkforce where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkforceResponse' smart constructor.
data UpdateWorkforceResponse = UpdateWorkforceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A single private workforce. You can create one private work force in
    -- each AWS Region. By default, any workforce-related API operation used in
    -- a specific region will apply to the workforce created in that region. To
    -- learn how to create a private workforce, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
    workforce :: Workforce
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- each AWS Region. By default, any workforce-related API operation used in
-- a specific region will apply to the workforce created in that region. To
-- learn how to create a private workforce, see
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
-- each AWS Region. By default, any workforce-related API operation used in
-- a specific region will apply to the workforce created in that region. To
-- learn how to create a private workforce, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
updateWorkforceResponse_workforce :: Lens.Lens' UpdateWorkforceResponse Workforce
updateWorkforceResponse_workforce = Lens.lens (\UpdateWorkforceResponse' {workforce} -> workforce) (\s@UpdateWorkforceResponse' {} a -> s {workforce = a} :: UpdateWorkforceResponse)

instance Prelude.NFData UpdateWorkforceResponse
