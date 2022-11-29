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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionsType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionsType where

import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.FromJSON AccountTakeoverActionsType where
  parseJSON =
    Core.withObject
      "AccountTakeoverActionsType"
      ( \x ->
          AccountTakeoverActionsType'
            Prelude.<$> (x Core..:? "LowAction")
            Prelude.<*> (x Core..:? "MediumAction")
            Prelude.<*> (x Core..:? "HighAction")
      )

instance Prelude.Hashable AccountTakeoverActionsType where
  hashWithSalt _salt AccountTakeoverActionsType' {..} =
    _salt `Prelude.hashWithSalt` lowAction
      `Prelude.hashWithSalt` mediumAction
      `Prelude.hashWithSalt` highAction

instance Prelude.NFData AccountTakeoverActionsType where
  rnf AccountTakeoverActionsType' {..} =
    Prelude.rnf lowAction
      `Prelude.seq` Prelude.rnf mediumAction
      `Prelude.seq` Prelude.rnf highAction

instance Core.ToJSON AccountTakeoverActionsType where
  toJSON AccountTakeoverActionsType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LowAction" Core..=) Prelude.<$> lowAction,
            ("MediumAction" Core..=) Prelude.<$> mediumAction,
            ("HighAction" Core..=) Prelude.<$> highAction
          ]
      )
