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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the risk configuration.
module Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRiskConfiguration' smart constructor.
data DescribeRiskConfiguration = DescribeRiskConfiguration'
  { -- | The app client ID.
    clientId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The user pool ID.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeRiskConfiguration
newDescribeRiskConfiguration pUserPoolId_ =
  DescribeRiskConfiguration'
    { clientId = Core.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The app client ID.
describeRiskConfiguration_clientId :: Lens.Lens' DescribeRiskConfiguration (Core.Maybe Core.Text)
describeRiskConfiguration_clientId = Lens.lens (\DescribeRiskConfiguration' {clientId} -> clientId) (\s@DescribeRiskConfiguration' {} a -> s {clientId = a} :: DescribeRiskConfiguration) Core.. Lens.mapping Core._Sensitive

-- | The user pool ID.
describeRiskConfiguration_userPoolId :: Lens.Lens' DescribeRiskConfiguration Core.Text
describeRiskConfiguration_userPoolId = Lens.lens (\DescribeRiskConfiguration' {userPoolId} -> userPoolId) (\s@DescribeRiskConfiguration' {} a -> s {userPoolId = a} :: DescribeRiskConfiguration)

instance Core.AWSRequest DescribeRiskConfiguration where
  type
    AWSResponse DescribeRiskConfiguration =
      DescribeRiskConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRiskConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "RiskConfiguration")
      )

instance Core.Hashable DescribeRiskConfiguration

instance Core.NFData DescribeRiskConfiguration

instance Core.ToHeaders DescribeRiskConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DescribeRiskConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeRiskConfiguration where
  toJSON DescribeRiskConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientId" Core..=) Core.<$> clientId,
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath DescribeRiskConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRiskConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRiskConfigurationResponse' smart constructor.
data DescribeRiskConfigurationResponse = DescribeRiskConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The risk configuration.
    riskConfiguration :: RiskConfigurationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
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
describeRiskConfigurationResponse_httpStatus :: Lens.Lens' DescribeRiskConfigurationResponse Core.Int
describeRiskConfigurationResponse_httpStatus = Lens.lens (\DescribeRiskConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeRiskConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeRiskConfigurationResponse)

-- | The risk configuration.
describeRiskConfigurationResponse_riskConfiguration :: Lens.Lens' DescribeRiskConfigurationResponse RiskConfigurationType
describeRiskConfigurationResponse_riskConfiguration = Lens.lens (\DescribeRiskConfigurationResponse' {riskConfiguration} -> riskConfiguration) (\s@DescribeRiskConfigurationResponse' {} a -> s {riskConfiguration = a} :: DescribeRiskConfigurationResponse)

instance
  Core.NFData
    DescribeRiskConfigurationResponse
