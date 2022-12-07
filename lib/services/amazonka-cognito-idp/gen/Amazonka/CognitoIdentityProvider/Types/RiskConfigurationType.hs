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
-- Module      : Amazonka.CognitoIdentityProvider.Types.RiskConfigurationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.RiskConfigurationType where

import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The risk configuration type.
--
-- /See:/ 'newRiskConfigurationType' smart constructor.
data RiskConfigurationType = RiskConfigurationType'
  { -- | The app client ID.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The last modified date.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The account takeover risk configuration object, including the
    -- @NotifyConfiguration@ object and @Actions@ to take if there is an
    -- account takeover.
    accountTakeoverRiskConfiguration :: Prelude.Maybe AccountTakeoverRiskConfigurationType,
    -- | The configuration to override the risk decision.
    riskExceptionConfiguration :: Prelude.Maybe RiskExceptionConfigurationType,
    -- | The user pool ID.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The compromised credentials risk configuration object, including the
    -- @EventFilter@ and the @EventAction@.
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
-- 'clientId', 'riskConfigurationType_clientId' - The app client ID.
--
-- 'lastModifiedDate', 'riskConfigurationType_lastModifiedDate' - The last modified date.
--
-- 'accountTakeoverRiskConfiguration', 'riskConfigurationType_accountTakeoverRiskConfiguration' - The account takeover risk configuration object, including the
-- @NotifyConfiguration@ object and @Actions@ to take if there is an
-- account takeover.
--
-- 'riskExceptionConfiguration', 'riskConfigurationType_riskExceptionConfiguration' - The configuration to override the risk decision.
--
-- 'userPoolId', 'riskConfigurationType_userPoolId' - The user pool ID.
--
-- 'compromisedCredentialsRiskConfiguration', 'riskConfigurationType_compromisedCredentialsRiskConfiguration' - The compromised credentials risk configuration object, including the
-- @EventFilter@ and the @EventAction@.
newRiskConfigurationType ::
  RiskConfigurationType
newRiskConfigurationType =
  RiskConfigurationType'
    { clientId = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      accountTakeoverRiskConfiguration = Prelude.Nothing,
      riskExceptionConfiguration = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      compromisedCredentialsRiskConfiguration =
        Prelude.Nothing
    }

-- | The app client ID.
riskConfigurationType_clientId :: Lens.Lens' RiskConfigurationType (Prelude.Maybe Prelude.Text)
riskConfigurationType_clientId = Lens.lens (\RiskConfigurationType' {clientId} -> clientId) (\s@RiskConfigurationType' {} a -> s {clientId = a} :: RiskConfigurationType) Prelude.. Lens.mapping Data._Sensitive

-- | The last modified date.
riskConfigurationType_lastModifiedDate :: Lens.Lens' RiskConfigurationType (Prelude.Maybe Prelude.UTCTime)
riskConfigurationType_lastModifiedDate = Lens.lens (\RiskConfigurationType' {lastModifiedDate} -> lastModifiedDate) (\s@RiskConfigurationType' {} a -> s {lastModifiedDate = a} :: RiskConfigurationType) Prelude.. Lens.mapping Data._Time

-- | The account takeover risk configuration object, including the
-- @NotifyConfiguration@ object and @Actions@ to take if there is an
-- account takeover.
riskConfigurationType_accountTakeoverRiskConfiguration :: Lens.Lens' RiskConfigurationType (Prelude.Maybe AccountTakeoverRiskConfigurationType)
riskConfigurationType_accountTakeoverRiskConfiguration = Lens.lens (\RiskConfigurationType' {accountTakeoverRiskConfiguration} -> accountTakeoverRiskConfiguration) (\s@RiskConfigurationType' {} a -> s {accountTakeoverRiskConfiguration = a} :: RiskConfigurationType)

-- | The configuration to override the risk decision.
riskConfigurationType_riskExceptionConfiguration :: Lens.Lens' RiskConfigurationType (Prelude.Maybe RiskExceptionConfigurationType)
riskConfigurationType_riskExceptionConfiguration = Lens.lens (\RiskConfigurationType' {riskExceptionConfiguration} -> riskExceptionConfiguration) (\s@RiskConfigurationType' {} a -> s {riskExceptionConfiguration = a} :: RiskConfigurationType)

-- | The user pool ID.
riskConfigurationType_userPoolId :: Lens.Lens' RiskConfigurationType (Prelude.Maybe Prelude.Text)
riskConfigurationType_userPoolId = Lens.lens (\RiskConfigurationType' {userPoolId} -> userPoolId) (\s@RiskConfigurationType' {} a -> s {userPoolId = a} :: RiskConfigurationType)

-- | The compromised credentials risk configuration object, including the
-- @EventFilter@ and the @EventAction@.
riskConfigurationType_compromisedCredentialsRiskConfiguration :: Lens.Lens' RiskConfigurationType (Prelude.Maybe CompromisedCredentialsRiskConfigurationType)
riskConfigurationType_compromisedCredentialsRiskConfiguration = Lens.lens (\RiskConfigurationType' {compromisedCredentialsRiskConfiguration} -> compromisedCredentialsRiskConfiguration) (\s@RiskConfigurationType' {} a -> s {compromisedCredentialsRiskConfiguration = a} :: RiskConfigurationType)

instance Data.FromJSON RiskConfigurationType where
  parseJSON =
    Data.withObject
      "RiskConfigurationType"
      ( \x ->
          RiskConfigurationType'
            Prelude.<$> (x Data..:? "ClientId")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "AccountTakeoverRiskConfiguration")
            Prelude.<*> (x Data..:? "RiskExceptionConfiguration")
            Prelude.<*> (x Data..:? "UserPoolId")
            Prelude.<*> ( x
                            Data..:? "CompromisedCredentialsRiskConfiguration"
                        )
      )

instance Prelude.Hashable RiskConfigurationType where
  hashWithSalt _salt RiskConfigurationType' {..} =
    _salt `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` accountTakeoverRiskConfiguration
      `Prelude.hashWithSalt` riskExceptionConfiguration
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` compromisedCredentialsRiskConfiguration

instance Prelude.NFData RiskConfigurationType where
  rnf RiskConfigurationType' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf accountTakeoverRiskConfiguration
      `Prelude.seq` Prelude.rnf riskExceptionConfiguration
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf compromisedCredentialsRiskConfiguration
