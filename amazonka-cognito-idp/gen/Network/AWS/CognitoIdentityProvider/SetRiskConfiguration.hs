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
-- Module      : Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures actions on detected risks. To delete the risk configuration
-- for @UserPoolId@ or @ClientId@, pass null values for all four
-- configuration types.
--
-- To enable Amazon Cognito advanced security features, update the user
-- pool to include the @UserPoolAddOns@ key@AdvancedSecurityMode@.
module Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
  ( -- * Creating a Request
    SetRiskConfiguration (..),
    newSetRiskConfiguration,

    -- * Request Lenses
    setRiskConfiguration_accountTakeoverRiskConfiguration,
    setRiskConfiguration_clientId,
    setRiskConfiguration_riskExceptionConfiguration,
    setRiskConfiguration_compromisedCredentialsRiskConfiguration,
    setRiskConfiguration_userPoolId,

    -- * Destructuring the Response
    SetRiskConfigurationResponse (..),
    newSetRiskConfigurationResponse,

    -- * Response Lenses
    setRiskConfigurationResponse_httpStatus,
    setRiskConfigurationResponse_riskConfiguration,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetRiskConfiguration' smart constructor.
data SetRiskConfiguration = SetRiskConfiguration'
  { -- | The account takeover risk configuration.
    accountTakeoverRiskConfiguration :: Core.Maybe AccountTakeoverRiskConfigurationType,
    -- | The app client ID. If @ClientId@ is null, then the risk configuration is
    -- mapped to @userPoolId@. When the client ID is null, the same risk
    -- configuration is applied to all the clients in the userPool.
    --
    -- Otherwise, @ClientId@ is mapped to the client. When the client ID is not
    -- null, the user pool configuration is overridden and the risk
    -- configuration for the client is used instead.
    clientId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The configuration to override the risk decision.
    riskExceptionConfiguration :: Core.Maybe RiskExceptionConfigurationType,
    -- | The compromised credentials risk configuration.
    compromisedCredentialsRiskConfiguration :: Core.Maybe CompromisedCredentialsRiskConfigurationType,
    -- | The user pool ID.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetRiskConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountTakeoverRiskConfiguration', 'setRiskConfiguration_accountTakeoverRiskConfiguration' - The account takeover risk configuration.
--
-- 'clientId', 'setRiskConfiguration_clientId' - The app client ID. If @ClientId@ is null, then the risk configuration is
-- mapped to @userPoolId@. When the client ID is null, the same risk
-- configuration is applied to all the clients in the userPool.
--
-- Otherwise, @ClientId@ is mapped to the client. When the client ID is not
-- null, the user pool configuration is overridden and the risk
-- configuration for the client is used instead.
--
-- 'riskExceptionConfiguration', 'setRiskConfiguration_riskExceptionConfiguration' - The configuration to override the risk decision.
--
-- 'compromisedCredentialsRiskConfiguration', 'setRiskConfiguration_compromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration.
--
-- 'userPoolId', 'setRiskConfiguration_userPoolId' - The user pool ID.
newSetRiskConfiguration ::
  -- | 'userPoolId'
  Core.Text ->
  SetRiskConfiguration
newSetRiskConfiguration pUserPoolId_ =
  SetRiskConfiguration'
    { accountTakeoverRiskConfiguration =
        Core.Nothing,
      clientId = Core.Nothing,
      riskExceptionConfiguration = Core.Nothing,
      compromisedCredentialsRiskConfiguration =
        Core.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The account takeover risk configuration.
setRiskConfiguration_accountTakeoverRiskConfiguration :: Lens.Lens' SetRiskConfiguration (Core.Maybe AccountTakeoverRiskConfigurationType)
setRiskConfiguration_accountTakeoverRiskConfiguration = Lens.lens (\SetRiskConfiguration' {accountTakeoverRiskConfiguration} -> accountTakeoverRiskConfiguration) (\s@SetRiskConfiguration' {} a -> s {accountTakeoverRiskConfiguration = a} :: SetRiskConfiguration)

-- | The app client ID. If @ClientId@ is null, then the risk configuration is
-- mapped to @userPoolId@. When the client ID is null, the same risk
-- configuration is applied to all the clients in the userPool.
--
-- Otherwise, @ClientId@ is mapped to the client. When the client ID is not
-- null, the user pool configuration is overridden and the risk
-- configuration for the client is used instead.
setRiskConfiguration_clientId :: Lens.Lens' SetRiskConfiguration (Core.Maybe Core.Text)
setRiskConfiguration_clientId = Lens.lens (\SetRiskConfiguration' {clientId} -> clientId) (\s@SetRiskConfiguration' {} a -> s {clientId = a} :: SetRiskConfiguration) Core.. Lens.mapping Core._Sensitive

-- | The configuration to override the risk decision.
setRiskConfiguration_riskExceptionConfiguration :: Lens.Lens' SetRiskConfiguration (Core.Maybe RiskExceptionConfigurationType)
setRiskConfiguration_riskExceptionConfiguration = Lens.lens (\SetRiskConfiguration' {riskExceptionConfiguration} -> riskExceptionConfiguration) (\s@SetRiskConfiguration' {} a -> s {riskExceptionConfiguration = a} :: SetRiskConfiguration)

-- | The compromised credentials risk configuration.
setRiskConfiguration_compromisedCredentialsRiskConfiguration :: Lens.Lens' SetRiskConfiguration (Core.Maybe CompromisedCredentialsRiskConfigurationType)
setRiskConfiguration_compromisedCredentialsRiskConfiguration = Lens.lens (\SetRiskConfiguration' {compromisedCredentialsRiskConfiguration} -> compromisedCredentialsRiskConfiguration) (\s@SetRiskConfiguration' {} a -> s {compromisedCredentialsRiskConfiguration = a} :: SetRiskConfiguration)

-- | The user pool ID.
setRiskConfiguration_userPoolId :: Lens.Lens' SetRiskConfiguration Core.Text
setRiskConfiguration_userPoolId = Lens.lens (\SetRiskConfiguration' {userPoolId} -> userPoolId) (\s@SetRiskConfiguration' {} a -> s {userPoolId = a} :: SetRiskConfiguration)

instance Core.AWSRequest SetRiskConfiguration where
  type
    AWSResponse SetRiskConfiguration =
      SetRiskConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetRiskConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "RiskConfiguration")
      )

instance Core.Hashable SetRiskConfiguration

instance Core.NFData SetRiskConfiguration

instance Core.ToHeaders SetRiskConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.SetRiskConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetRiskConfiguration where
  toJSON SetRiskConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountTakeoverRiskConfiguration" Core..=)
              Core.<$> accountTakeoverRiskConfiguration,
            ("ClientId" Core..=) Core.<$> clientId,
            ("RiskExceptionConfiguration" Core..=)
              Core.<$> riskExceptionConfiguration,
            ("CompromisedCredentialsRiskConfiguration" Core..=)
              Core.<$> compromisedCredentialsRiskConfiguration,
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath SetRiskConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery SetRiskConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetRiskConfigurationResponse' smart constructor.
data SetRiskConfigurationResponse = SetRiskConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The risk configuration.
    riskConfiguration :: RiskConfigurationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetRiskConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setRiskConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'riskConfiguration', 'setRiskConfigurationResponse_riskConfiguration' - The risk configuration.
newSetRiskConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'riskConfiguration'
  RiskConfigurationType ->
  SetRiskConfigurationResponse
newSetRiskConfigurationResponse
  pHttpStatus_
  pRiskConfiguration_ =
    SetRiskConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        riskConfiguration = pRiskConfiguration_
      }

-- | The response's http status code.
setRiskConfigurationResponse_httpStatus :: Lens.Lens' SetRiskConfigurationResponse Core.Int
setRiskConfigurationResponse_httpStatus = Lens.lens (\SetRiskConfigurationResponse' {httpStatus} -> httpStatus) (\s@SetRiskConfigurationResponse' {} a -> s {httpStatus = a} :: SetRiskConfigurationResponse)

-- | The risk configuration.
setRiskConfigurationResponse_riskConfiguration :: Lens.Lens' SetRiskConfigurationResponse RiskConfigurationType
setRiskConfigurationResponse_riskConfiguration = Lens.lens (\SetRiskConfigurationResponse' {riskConfiguration} -> riskConfiguration) (\s@SetRiskConfigurationResponse' {} a -> s {riskConfiguration = a} :: SetRiskConfigurationResponse)

instance Core.NFData SetRiskConfigurationResponse
