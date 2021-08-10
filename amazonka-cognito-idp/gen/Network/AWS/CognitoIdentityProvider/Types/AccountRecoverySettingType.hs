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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType where

import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
accountRecoverySettingType_recoveryMechanisms = Lens.lens (\AccountRecoverySettingType' {recoveryMechanisms} -> recoveryMechanisms) (\s@AccountRecoverySettingType' {} a -> s {recoveryMechanisms = a} :: AccountRecoverySettingType) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON AccountRecoverySettingType where
  parseJSON =
    Core.withObject
      "AccountRecoverySettingType"
      ( \x ->
          AccountRecoverySettingType'
            Prelude.<$> (x Core..:? "RecoveryMechanisms")
      )

instance Prelude.Hashable AccountRecoverySettingType

instance Prelude.NFData AccountRecoverySettingType

instance Core.ToJSON AccountRecoverySettingType where
  toJSON AccountRecoverySettingType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RecoveryMechanisms" Core..=)
              Prelude.<$> recoveryMechanisms
          ]
      )
