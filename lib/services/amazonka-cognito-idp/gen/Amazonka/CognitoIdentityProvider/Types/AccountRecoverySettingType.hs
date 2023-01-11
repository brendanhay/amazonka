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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AccountRecoverySettingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AccountRecoverySettingType where

import Amazonka.CognitoIdentityProvider.Types.RecoveryOptionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data type for @AccountRecoverySetting@.
--
-- /See:/ 'newAccountRecoverySettingType' smart constructor.
data AccountRecoverySettingType = AccountRecoverySettingType'
  { -- | The list of @RecoveryOptionTypes@.
    recoveryMechanisms :: Prelude.Maybe (Prelude.NonEmpty RecoveryOptionType)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountRecoverySettingType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryMechanisms', 'accountRecoverySettingType_recoveryMechanisms' - The list of @RecoveryOptionTypes@.
newAccountRecoverySettingType ::
  AccountRecoverySettingType
newAccountRecoverySettingType =
  AccountRecoverySettingType'
    { recoveryMechanisms =
        Prelude.Nothing
    }

-- | The list of @RecoveryOptionTypes@.
accountRecoverySettingType_recoveryMechanisms :: Lens.Lens' AccountRecoverySettingType (Prelude.Maybe (Prelude.NonEmpty RecoveryOptionType))
accountRecoverySettingType_recoveryMechanisms = Lens.lens (\AccountRecoverySettingType' {recoveryMechanisms} -> recoveryMechanisms) (\s@AccountRecoverySettingType' {} a -> s {recoveryMechanisms = a} :: AccountRecoverySettingType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AccountRecoverySettingType where
  parseJSON =
    Data.withObject
      "AccountRecoverySettingType"
      ( \x ->
          AccountRecoverySettingType'
            Prelude.<$> (x Data..:? "RecoveryMechanisms")
      )

instance Prelude.Hashable AccountRecoverySettingType where
  hashWithSalt _salt AccountRecoverySettingType' {..} =
    _salt `Prelude.hashWithSalt` recoveryMechanisms

instance Prelude.NFData AccountRecoverySettingType where
  rnf AccountRecoverySettingType' {..} =
    Prelude.rnf recoveryMechanisms

instance Data.ToJSON AccountRecoverySettingType where
  toJSON AccountRecoverySettingType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecoveryMechanisms" Data..=)
              Prelude.<$> recoveryMechanisms
          ]
      )
