{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The risk configuration type.
--
-- /See:/ 'newRiskConfigurationType' smart constructor.
data RiskConfigurationType = RiskConfigurationType'
  { -- | The account takeover risk configuration object including the
    -- @NotifyConfiguration@ object and @Actions@ to take in the case of an
    -- account takeover.
    accountTakeoverRiskConfiguration :: Core.Maybe AccountTakeoverRiskConfigurationType,
    -- | The last modified date.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The app client ID.
    clientId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The configuration to override the risk decision.
    riskExceptionConfiguration :: Core.Maybe RiskExceptionConfigurationType,
    -- | The user pool ID.
    userPoolId :: Core.Maybe Core.Text,
    -- | The compromised credentials risk configuration object including the
    -- @EventFilter@ and the @EventAction@
    compromisedCredentialsRiskConfiguration :: Core.Maybe CompromisedCredentialsRiskConfigurationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'RiskConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountTakeoverRiskConfiguration', 'riskConfigurationType_accountTakeoverRiskConfiguration' - The account takeover risk configuration object including the
-- @NotifyConfiguration@ object and @Actions@ to take in the case of an
-- account takeover.
--
-- 'lastModifiedDate', 'riskConfigurationType_lastModifiedDate' - The last modified date.
--
-- 'clientId', 'riskConfigurationType_clientId' - The app client ID.
--
-- 'riskExceptionConfiguration', 'riskConfigurationType_riskExceptionConfiguration' - The configuration to override the risk decision.
--
-- 'userPoolId', 'riskConfigurationType_userPoolId' - The user pool ID.
--
-- 'compromisedCredentialsRiskConfiguration', 'riskConfigurationType_compromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration object including the
-- @EventFilter@ and the @EventAction@
newRiskConfigurationType ::
  RiskConfigurationType
newRiskConfigurationType =
  RiskConfigurationType'
    { accountTakeoverRiskConfiguration =
        Core.Nothing,
      lastModifiedDate = Core.Nothing,
      clientId = Core.Nothing,
      riskExceptionConfiguration = Core.Nothing,
      userPoolId = Core.Nothing,
      compromisedCredentialsRiskConfiguration =
        Core.Nothing
    }

-- | The account takeover risk configuration object including the
-- @NotifyConfiguration@ object and @Actions@ to take in the case of an
-- account takeover.
riskConfigurationType_accountTakeoverRiskConfiguration :: Lens.Lens' RiskConfigurationType (Core.Maybe AccountTakeoverRiskConfigurationType)
riskConfigurationType_accountTakeoverRiskConfiguration = Lens.lens (\RiskConfigurationType' {accountTakeoverRiskConfiguration} -> accountTakeoverRiskConfiguration) (\s@RiskConfigurationType' {} a -> s {accountTakeoverRiskConfiguration = a} :: RiskConfigurationType)

-- | The last modified date.
riskConfigurationType_lastModifiedDate :: Lens.Lens' RiskConfigurationType (Core.Maybe Core.UTCTime)
riskConfigurationType_lastModifiedDate = Lens.lens (\RiskConfigurationType' {lastModifiedDate} -> lastModifiedDate) (\s@RiskConfigurationType' {} a -> s {lastModifiedDate = a} :: RiskConfigurationType) Core.. Lens.mapping Core._Time

-- | The app client ID.
riskConfigurationType_clientId :: Lens.Lens' RiskConfigurationType (Core.Maybe Core.Text)
riskConfigurationType_clientId = Lens.lens (\RiskConfigurationType' {clientId} -> clientId) (\s@RiskConfigurationType' {} a -> s {clientId = a} :: RiskConfigurationType) Core.. Lens.mapping Core._Sensitive

-- | The configuration to override the risk decision.
riskConfigurationType_riskExceptionConfiguration :: Lens.Lens' RiskConfigurationType (Core.Maybe RiskExceptionConfigurationType)
riskConfigurationType_riskExceptionConfiguration = Lens.lens (\RiskConfigurationType' {riskExceptionConfiguration} -> riskExceptionConfiguration) (\s@RiskConfigurationType' {} a -> s {riskExceptionConfiguration = a} :: RiskConfigurationType)

-- | The user pool ID.
riskConfigurationType_userPoolId :: Lens.Lens' RiskConfigurationType (Core.Maybe Core.Text)
riskConfigurationType_userPoolId = Lens.lens (\RiskConfigurationType' {userPoolId} -> userPoolId) (\s@RiskConfigurationType' {} a -> s {userPoolId = a} :: RiskConfigurationType)

-- | The compromised credentials risk configuration object including the
-- @EventFilter@ and the @EventAction@
riskConfigurationType_compromisedCredentialsRiskConfiguration :: Lens.Lens' RiskConfigurationType (Core.Maybe CompromisedCredentialsRiskConfigurationType)
riskConfigurationType_compromisedCredentialsRiskConfiguration = Lens.lens (\RiskConfigurationType' {compromisedCredentialsRiskConfiguration} -> compromisedCredentialsRiskConfiguration) (\s@RiskConfigurationType' {} a -> s {compromisedCredentialsRiskConfiguration = a} :: RiskConfigurationType)

instance Core.FromJSON RiskConfigurationType where
  parseJSON =
    Core.withObject
      "RiskConfigurationType"
      ( \x ->
          RiskConfigurationType'
            Core.<$> (x Core..:? "AccountTakeoverRiskConfiguration")
            Core.<*> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ClientId")
            Core.<*> (x Core..:? "RiskExceptionConfiguration")
            Core.<*> (x Core..:? "UserPoolId")
            Core.<*> ( x
                         Core..:? "CompromisedCredentialsRiskConfiguration"
                     )
      )

instance Core.Hashable RiskConfigurationType

instance Core.NFData RiskConfigurationType
