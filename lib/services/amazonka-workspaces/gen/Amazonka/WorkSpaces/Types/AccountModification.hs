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
-- Module      : Amazonka.WorkSpaces.Types.AccountModification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.AccountModification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Amazonka.WorkSpaces.Types.DedicatedTenancySupportResultEnum

-- | Describes a modification to the configuration of Bring Your Own License
-- (BYOL) for the specified account.
--
-- /See:/ 'newAccountModification' smart constructor.
data AccountModification = AccountModification'
  { -- | The IP address range, specified as an IPv4 CIDR block, for the
    -- management network interface used for the account.
    dedicatedTenancyManagementCidrRange :: Prelude.Maybe Prelude.Text,
    -- | The status of BYOL (whether BYOL is being enabled or disabled).
    dedicatedTenancySupport :: Prelude.Maybe DedicatedTenancySupportResultEnum,
    -- | The error code that is returned if the configuration of BYOL cannot be
    -- modified.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message that is returned if the configuration of
    -- BYOL cannot be modified.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The state of the modification to the configuration of BYOL.
    modificationState :: Prelude.Maybe DedicatedTenancyModificationStateEnum,
    -- | The timestamp when the modification of the BYOL configuration was
    -- started.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountModification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedTenancyManagementCidrRange', 'accountModification_dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface used for the account.
--
-- 'dedicatedTenancySupport', 'accountModification_dedicatedTenancySupport' - The status of BYOL (whether BYOL is being enabled or disabled).
--
-- 'errorCode', 'accountModification_errorCode' - The error code that is returned if the configuration of BYOL cannot be
-- modified.
--
-- 'errorMessage', 'accountModification_errorMessage' - The text of the error message that is returned if the configuration of
-- BYOL cannot be modified.
--
-- 'modificationState', 'accountModification_modificationState' - The state of the modification to the configuration of BYOL.
--
-- 'startTime', 'accountModification_startTime' - The timestamp when the modification of the BYOL configuration was
-- started.
newAccountModification ::
  AccountModification
newAccountModification =
  AccountModification'
    { dedicatedTenancyManagementCidrRange =
        Prelude.Nothing,
      dedicatedTenancySupport = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      modificationState = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface used for the account.
accountModification_dedicatedTenancyManagementCidrRange :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.Text)
accountModification_dedicatedTenancyManagementCidrRange = Lens.lens (\AccountModification' {dedicatedTenancyManagementCidrRange} -> dedicatedTenancyManagementCidrRange) (\s@AccountModification' {} a -> s {dedicatedTenancyManagementCidrRange = a} :: AccountModification)

-- | The status of BYOL (whether BYOL is being enabled or disabled).
accountModification_dedicatedTenancySupport :: Lens.Lens' AccountModification (Prelude.Maybe DedicatedTenancySupportResultEnum)
accountModification_dedicatedTenancySupport = Lens.lens (\AccountModification' {dedicatedTenancySupport} -> dedicatedTenancySupport) (\s@AccountModification' {} a -> s {dedicatedTenancySupport = a} :: AccountModification)

-- | The error code that is returned if the configuration of BYOL cannot be
-- modified.
accountModification_errorCode :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.Text)
accountModification_errorCode = Lens.lens (\AccountModification' {errorCode} -> errorCode) (\s@AccountModification' {} a -> s {errorCode = a} :: AccountModification)

-- | The text of the error message that is returned if the configuration of
-- BYOL cannot be modified.
accountModification_errorMessage :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.Text)
accountModification_errorMessage = Lens.lens (\AccountModification' {errorMessage} -> errorMessage) (\s@AccountModification' {} a -> s {errorMessage = a} :: AccountModification)

-- | The state of the modification to the configuration of BYOL.
accountModification_modificationState :: Lens.Lens' AccountModification (Prelude.Maybe DedicatedTenancyModificationStateEnum)
accountModification_modificationState = Lens.lens (\AccountModification' {modificationState} -> modificationState) (\s@AccountModification' {} a -> s {modificationState = a} :: AccountModification)

-- | The timestamp when the modification of the BYOL configuration was
-- started.
accountModification_startTime :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.UTCTime)
accountModification_startTime = Lens.lens (\AccountModification' {startTime} -> startTime) (\s@AccountModification' {} a -> s {startTime = a} :: AccountModification) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AccountModification where
  parseJSON =
    Data.withObject
      "AccountModification"
      ( \x ->
          AccountModification'
            Prelude.<$> (x Data..:? "DedicatedTenancyManagementCidrRange")
            Prelude.<*> (x Data..:? "DedicatedTenancySupport")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ModificationState")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable AccountModification where
  hashWithSalt _salt AccountModification' {..} =
    _salt
      `Prelude.hashWithSalt` dedicatedTenancyManagementCidrRange
      `Prelude.hashWithSalt` dedicatedTenancySupport
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` modificationState
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData AccountModification where
  rnf AccountModification' {..} =
    Prelude.rnf dedicatedTenancyManagementCidrRange
      `Prelude.seq` Prelude.rnf dedicatedTenancySupport
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf modificationState
      `Prelude.seq` Prelude.rnf startTime
