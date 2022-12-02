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
-- Module      : Amazonka.CognitoIdentityProvider.DescribeRiskConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the risk configuration.
module Amazonka.CognitoIdentityProvider.DescribeRiskConfiguration
  ( -- * Creating a Request
    DescribeRiskConfiguration (..),
    newDescribeRiskConfiguration,

    -- * Request Lenses
    describeRiskConfiguration_clientId,
    describeRiskConfiguration_userPoolId,

    -- * Destructuring the Response
    DescribeRiskConfigurationResponse (..),
    newDescribeRiskConfigurationResponse,

    -- * Response Lenses
    describeRiskConfigurationResponse_httpStatus,
    describeRiskConfigurationResponse_riskConfiguration,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRiskConfiguration' smart constructor.
data DescribeRiskConfiguration = DescribeRiskConfiguration'
  { -- | The app client ID.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRiskConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'describeRiskConfiguration_clientId' - The app client ID.
--
-- 'userPoolId', 'describeRiskConfiguration_userPoolId' - The user pool ID.
newDescribeRiskConfiguration ::
  -- | 'userPoolId'
  Prelude.Text ->
  DescribeRiskConfiguration
newDescribeRiskConfiguration pUserPoolId_ =
  DescribeRiskConfiguration'
    { clientId =
        Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The app client ID.
describeRiskConfiguration_clientId :: Lens.Lens' DescribeRiskConfiguration (Prelude.Maybe Prelude.Text)
describeRiskConfiguration_clientId = Lens.lens (\DescribeRiskConfiguration' {clientId} -> clientId) (\s@DescribeRiskConfiguration' {} a -> s {clientId = a} :: DescribeRiskConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The user pool ID.
describeRiskConfiguration_userPoolId :: Lens.Lens' DescribeRiskConfiguration Prelude.Text
describeRiskConfiguration_userPoolId = Lens.lens (\DescribeRiskConfiguration' {userPoolId} -> userPoolId) (\s@DescribeRiskConfiguration' {} a -> s {userPoolId = a} :: DescribeRiskConfiguration)

instance Core.AWSRequest DescribeRiskConfiguration where
  type
    AWSResponse DescribeRiskConfiguration =
      DescribeRiskConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRiskConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RiskConfiguration")
      )

instance Prelude.Hashable DescribeRiskConfiguration where
  hashWithSalt _salt DescribeRiskConfiguration' {..} =
    _salt `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData DescribeRiskConfiguration where
  rnf DescribeRiskConfiguration' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders DescribeRiskConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DescribeRiskConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRiskConfiguration where
  toJSON DescribeRiskConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientId" Data..=) Prelude.<$> clientId,
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath DescribeRiskConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRiskConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRiskConfigurationResponse' smart constructor.
data DescribeRiskConfigurationResponse = DescribeRiskConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The risk configuration.
    riskConfiguration :: RiskConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRiskConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeRiskConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'riskConfiguration', 'describeRiskConfigurationResponse_riskConfiguration' - The risk configuration.
newDescribeRiskConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'riskConfiguration'
  RiskConfigurationType ->
  DescribeRiskConfigurationResponse
newDescribeRiskConfigurationResponse
  pHttpStatus_
  pRiskConfiguration_ =
    DescribeRiskConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        riskConfiguration = pRiskConfiguration_
      }

-- | The response's http status code.
describeRiskConfigurationResponse_httpStatus :: Lens.Lens' DescribeRiskConfigurationResponse Prelude.Int
describeRiskConfigurationResponse_httpStatus = Lens.lens (\DescribeRiskConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeRiskConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeRiskConfigurationResponse)

-- | The risk configuration.
describeRiskConfigurationResponse_riskConfiguration :: Lens.Lens' DescribeRiskConfigurationResponse RiskConfigurationType
describeRiskConfigurationResponse_riskConfiguration = Lens.lens (\DescribeRiskConfigurationResponse' {riskConfiguration} -> riskConfiguration) (\s@DescribeRiskConfigurationResponse' {} a -> s {riskConfiguration = a} :: DescribeRiskConfigurationResponse)

instance
  Prelude.NFData
    DescribeRiskConfigurationResponse
  where
  rnf DescribeRiskConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf riskConfiguration
