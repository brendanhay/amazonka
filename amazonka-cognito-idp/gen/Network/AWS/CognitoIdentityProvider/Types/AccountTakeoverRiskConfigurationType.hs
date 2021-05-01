{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration for mitigation actions and notification for different
-- levels of risk detected for a potential account takeover.
--
-- /See:/ 'newAccountTakeoverRiskConfigurationType' smart constructor.
data AccountTakeoverRiskConfigurationType = AccountTakeoverRiskConfigurationType'
  { -- | The notify configuration used to construct email notifications.
    notifyConfiguration :: Prelude.Maybe NotifyConfigurationType,
    -- | Account takeover risk configuration actions
    actions :: AccountTakeoverActionsType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'actions', 'accountTakeoverRiskConfigurationType_actions' - Account takeover risk configuration actions
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

-- | Account takeover risk configuration actions
accountTakeoverRiskConfigurationType_actions :: Lens.Lens' AccountTakeoverRiskConfigurationType AccountTakeoverActionsType
accountTakeoverRiskConfigurationType_actions = Lens.lens (\AccountTakeoverRiskConfigurationType' {actions} -> actions) (\s@AccountTakeoverRiskConfigurationType' {} a -> s {actions = a} :: AccountTakeoverRiskConfigurationType)

instance
  Prelude.FromJSON
    AccountTakeoverRiskConfigurationType
  where
  parseJSON =
    Prelude.withObject
      "AccountTakeoverRiskConfigurationType"
      ( \x ->
          AccountTakeoverRiskConfigurationType'
            Prelude.<$> (x Prelude..:? "NotifyConfiguration")
            Prelude.<*> (x Prelude..: "Actions")
      )

instance
  Prelude.Hashable
    AccountTakeoverRiskConfigurationType

instance
  Prelude.NFData
    AccountTakeoverRiskConfigurationType

instance
  Prelude.ToJSON
    AccountTakeoverRiskConfigurationType
  where
  toJSON AccountTakeoverRiskConfigurationType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotifyConfiguration" Prelude..=)
              Prelude.<$> notifyConfiguration,
            Prelude.Just ("Actions" Prelude..= actions)
          ]
      )
