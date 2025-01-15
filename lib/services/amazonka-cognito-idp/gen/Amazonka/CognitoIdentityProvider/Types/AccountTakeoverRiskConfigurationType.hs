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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType where

import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Amazonka.CognitoIdentityProvider.Types.NotifyConfigurationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration for mitigation actions and notification for different
-- levels of risk detected for a potential account takeover.
--
-- /See:/ 'newAccountTakeoverRiskConfigurationType' smart constructor.
data AccountTakeoverRiskConfigurationType = AccountTakeoverRiskConfigurationType'
  { -- | The notify configuration used to construct email notifications.
    notifyConfiguration :: Prelude.Maybe NotifyConfigurationType,
    -- | Account takeover risk configuration actions.
    actions :: AccountTakeoverActionsType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountTakeoverRiskConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notifyConfiguration', 'accountTakeoverRiskConfigurationType_notifyConfiguration' - The notify configuration used to construct email notifications.
--
-- 'actions', 'accountTakeoverRiskConfigurationType_actions' - Account takeover risk configuration actions.
newAccountTakeoverRiskConfigurationType ::
  -- | 'actions'
  AccountTakeoverActionsType ->
  AccountTakeoverRiskConfigurationType
newAccountTakeoverRiskConfigurationType pActions_ =
  AccountTakeoverRiskConfigurationType'
    { notifyConfiguration =
        Prelude.Nothing,
      actions = pActions_
    }

-- | The notify configuration used to construct email notifications.
accountTakeoverRiskConfigurationType_notifyConfiguration :: Lens.Lens' AccountTakeoverRiskConfigurationType (Prelude.Maybe NotifyConfigurationType)
accountTakeoverRiskConfigurationType_notifyConfiguration = Lens.lens (\AccountTakeoverRiskConfigurationType' {notifyConfiguration} -> notifyConfiguration) (\s@AccountTakeoverRiskConfigurationType' {} a -> s {notifyConfiguration = a} :: AccountTakeoverRiskConfigurationType)

-- | Account takeover risk configuration actions.
accountTakeoverRiskConfigurationType_actions :: Lens.Lens' AccountTakeoverRiskConfigurationType AccountTakeoverActionsType
accountTakeoverRiskConfigurationType_actions = Lens.lens (\AccountTakeoverRiskConfigurationType' {actions} -> actions) (\s@AccountTakeoverRiskConfigurationType' {} a -> s {actions = a} :: AccountTakeoverRiskConfigurationType)

instance
  Data.FromJSON
    AccountTakeoverRiskConfigurationType
  where
  parseJSON =
    Data.withObject
      "AccountTakeoverRiskConfigurationType"
      ( \x ->
          AccountTakeoverRiskConfigurationType'
            Prelude.<$> (x Data..:? "NotifyConfiguration")
            Prelude.<*> (x Data..: "Actions")
      )

instance
  Prelude.Hashable
    AccountTakeoverRiskConfigurationType
  where
  hashWithSalt
    _salt
    AccountTakeoverRiskConfigurationType' {..} =
      _salt
        `Prelude.hashWithSalt` notifyConfiguration
        `Prelude.hashWithSalt` actions

instance
  Prelude.NFData
    AccountTakeoverRiskConfigurationType
  where
  rnf AccountTakeoverRiskConfigurationType' {..} =
    Prelude.rnf notifyConfiguration `Prelude.seq`
      Prelude.rnf actions

instance
  Data.ToJSON
    AccountTakeoverRiskConfigurationType
  where
  toJSON AccountTakeoverRiskConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotifyConfiguration" Data..=)
              Prelude.<$> notifyConfiguration,
            Prelude.Just ("Actions" Data..= actions)
          ]
      )
