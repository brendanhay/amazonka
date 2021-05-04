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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Account takeover actions type.
--
-- /See:/ 'newAccountTakeoverActionsType' smart constructor.
data AccountTakeoverActionsType = AccountTakeoverActionsType'
  { -- | Action to take for a low risk.
    lowAction :: Prelude.Maybe AccountTakeoverActionType,
    -- | Action to take for a medium risk.
    mediumAction :: Prelude.Maybe AccountTakeoverActionType,
    -- | Action to take for a high risk.
    highAction :: Prelude.Maybe AccountTakeoverActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountTakeoverActionsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowAction', 'accountTakeoverActionsType_lowAction' - Action to take for a low risk.
--
-- 'mediumAction', 'accountTakeoverActionsType_mediumAction' - Action to take for a medium risk.
--
-- 'highAction', 'accountTakeoverActionsType_highAction' - Action to take for a high risk.
newAccountTakeoverActionsType ::
  AccountTakeoverActionsType
newAccountTakeoverActionsType =
  AccountTakeoverActionsType'
    { lowAction =
        Prelude.Nothing,
      mediumAction = Prelude.Nothing,
      highAction = Prelude.Nothing
    }

-- | Action to take for a low risk.
accountTakeoverActionsType_lowAction :: Lens.Lens' AccountTakeoverActionsType (Prelude.Maybe AccountTakeoverActionType)
accountTakeoverActionsType_lowAction = Lens.lens (\AccountTakeoverActionsType' {lowAction} -> lowAction) (\s@AccountTakeoverActionsType' {} a -> s {lowAction = a} :: AccountTakeoverActionsType)

-- | Action to take for a medium risk.
accountTakeoverActionsType_mediumAction :: Lens.Lens' AccountTakeoverActionsType (Prelude.Maybe AccountTakeoverActionType)
accountTakeoverActionsType_mediumAction = Lens.lens (\AccountTakeoverActionsType' {mediumAction} -> mediumAction) (\s@AccountTakeoverActionsType' {} a -> s {mediumAction = a} :: AccountTakeoverActionsType)

-- | Action to take for a high risk.
accountTakeoverActionsType_highAction :: Lens.Lens' AccountTakeoverActionsType (Prelude.Maybe AccountTakeoverActionType)
accountTakeoverActionsType_highAction = Lens.lens (\AccountTakeoverActionsType' {highAction} -> highAction) (\s@AccountTakeoverActionsType' {} a -> s {highAction = a} :: AccountTakeoverActionsType)

instance Prelude.FromJSON AccountTakeoverActionsType where
  parseJSON =
    Prelude.withObject
      "AccountTakeoverActionsType"
      ( \x ->
          AccountTakeoverActionsType'
            Prelude.<$> (x Prelude..:? "LowAction")
            Prelude.<*> (x Prelude..:? "MediumAction")
            Prelude.<*> (x Prelude..:? "HighAction")
      )

instance Prelude.Hashable AccountTakeoverActionsType

instance Prelude.NFData AccountTakeoverActionsType

instance Prelude.ToJSON AccountTakeoverActionsType where
  toJSON AccountTakeoverActionsType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LowAction" Prelude..=) Prelude.<$> lowAction,
            ("MediumAction" Prelude..=) Prelude.<$> mediumAction,
            ("HighAction" Prelude..=) Prelude.<$> highAction
          ]
      )
