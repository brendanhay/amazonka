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
import qualified Network.AWS.Prelude as Prelude

-- | The risk configuration type.
--
-- /See:/ 'newRiskConfigurationType' smart constructor.
data RiskConfigurationType = RiskConfigurationType'
  { -- | The configuration to override the risk decision.
    riskExceptionConfiguration :: Prelude.Maybe RiskExceptionConfigurationType,
    -- | The app client ID.
    clientId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The account takeover risk configuration object including the
    -- @NotifyConfiguration@ object and @Actions@ to take in the case of an
    -- account takeover.
    accountTakeoverRiskConfiguration :: Prelude.Maybe AccountTakeoverRiskConfigurationType,
    -- | The last modified date.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The user pool ID.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The compromised credentials risk configuration object including the
    -- @EventFilter@ and the @EventAction@
    compromisedCredentialsRiskConfiguration :: Prelude.Maybe CompromisedCredentialsRiskConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RiskConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'riskExceptionConfiguration', 'riskConfigurationType_riskExceptionConfiguration' - The configuration to override the risk decision.
--
-- 'clientId', 'riskConfigurationType_clientId' - The app client ID.
--
-- 'accountTakeoverRiskConfiguration', 'riskConfigurationType_accountTakeoverRiskConfiguration' - The account takeover risk configuration object including the
-- @NotifyConfiguration@ object and @Actions@ to take in the case of an
-- account takeover.
--
-- 'lastModifiedDate', 'riskConfigurationType_lastModifiedDate' - The last modified date.
--
-- 'userPoolId', 'riskConfigurationType_userPoolId' - The user pool ID.
--
-- 'compromisedCredentialsRiskConfiguration', 'riskConfigurationType_compromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration object including the
-- @EventFilter@ and the @EventAction@
newRiskConfigurationType ::
  RiskConfigurationType
newRiskConfigurationType =
  RiskConfigurationType'
    { riskExceptionConfiguration =
        Prelude.Nothing,
      clientId = Prelude.Nothing,
      accountTakeoverRiskConfiguration = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      compromisedCredentialsRiskConfiguration =
        Prelude.Nothing
    }

-- | The configuration to override the risk decision.
riskConfigurationType_riskExceptionConfiguration :: Lens.Lens' RiskConfigurationType (Prelude.Maybe RiskExceptionConfigurationType)
riskConfigurationType_riskExceptionConfiguration = Lens.lens (\RiskConfigurationType' {riskExceptionConfiguration} -> riskExceptionConfiguration) (\s@RiskConfigurationType' {} a -> s {riskExceptionConfiguration = a} :: RiskConfigurationType)

-- | The app client ID.
riskConfigurationType_clientId :: Lens.Lens' RiskConfigurationType (Prelude.Maybe Prelude.Text)
riskConfigurationType_clientId = Lens.lens (\RiskConfigurationType' {clientId} -> clientId) (\s@RiskConfigurationType' {} a -> s {clientId = a} :: RiskConfigurationType) Prelude.. Lens.mapping Core._Sensitive

-- | The account takeover risk configuration object including the
-- @NotifyConfiguration@ object and @Actions@ to take in the case of an
-- account takeover.
riskConfigurationType_accountTakeoverRiskConfiguration :: Lens.Lens' RiskConfigurationType (Prelude.Maybe AccountTakeoverRiskConfigurationType)
riskConfigurationType_accountTakeoverRiskConfiguration = Lens.lens (\RiskConfigurationType' {accountTakeoverRiskConfiguration} -> accountTakeoverRiskConfiguration) (\s@RiskConfigurationType' {} a -> s {accountTakeoverRiskConfiguration = a} :: RiskConfigurationType)

-- | The last modified date.
riskConfigurationType_lastModifiedDate :: Lens.Lens' RiskConfigurationType (Prelude.Maybe Prelude.UTCTime)
riskConfigurationType_lastModifiedDate = Lens.lens (\RiskConfigurationType' {lastModifiedDate} -> lastModifiedDate) (\s@RiskConfigurationType' {} a -> s {lastModifiedDate = a} :: RiskConfigurationType) Prelude.. Lens.mapping Core._Time

-- | The user pool ID.
riskConfigurationType_userPoolId :: Lens.Lens' RiskConfigurationType (Prelude.Maybe Prelude.Text)
riskConfigurationType_userPoolId = Lens.lens (\RiskConfigurationType' {userPoolId} -> userPoolId) (\s@RiskConfigurationType' {} a -> s {userPoolId = a} :: RiskConfigurationType)

-- | The compromised credentials risk configuration object including the
-- @EventFilter@ and the @EventAction@
riskConfigurationType_compromisedCredentialsRiskConfiguration :: Lens.Lens' RiskConfigurationType (Prelude.Maybe CompromisedCredentialsRiskConfigurationType)
riskConfigurationType_compromisedCredentialsRiskConfiguration = Lens.lens (\RiskConfigurationType' {compromisedCredentialsRiskConfiguration} -> compromisedCredentialsRiskConfiguration) (\s@RiskConfigurationType' {} a -> s {compromisedCredentialsRiskConfiguration = a} :: RiskConfigurationType)

instance Core.FromJSON RiskConfigurationType where
  parseJSON =
    Core.withObject
      "RiskConfigurationType"
      ( \x ->
          RiskConfigurationType'
            Prelude.<$> (x Core..:? "RiskExceptionConfiguration")
            Prelude.<*> (x Core..:? "ClientId")
            Prelude.<*> (x Core..:? "AccountTakeoverRiskConfiguration")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "UserPoolId")
            Prelude.<*> ( x
                            Core..:? "CompromisedCredentialsRiskConfiguration"
                        )
      )

instance Prelude.Hashable RiskConfigurationType

instance Prelude.NFData RiskConfigurationType
