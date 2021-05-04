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
-- Module      : Network.AWS.WorkSpaces.Types.AccountModification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.AccountModification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum

-- | Describes a modification to the configuration of Bring Your Own License
-- (BYOL) for the specified account.
--
-- /See:/ 'newAccountModification' smart constructor.
data AccountModification = AccountModification'
  { -- | The status of BYOL (whether BYOL is being enabled or disabled).
    dedicatedTenancySupport :: Prelude.Maybe DedicatedTenancySupportResultEnum,
    -- | The timestamp when the modification of the BYOL configuration was
    -- started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The IP address range, specified as an IPv4 CIDR block, for the
    -- management network interface used for the account.
    dedicatedTenancyManagementCidrRange :: Prelude.Maybe Prelude.Text,
    -- | The state of the modification to the configuration of BYOL.
    modificationState :: Prelude.Maybe DedicatedTenancyModificationStateEnum,
    -- | The text of the error message that is returned if the configuration of
    -- BYOL cannot be modified.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code that is returned if the configuration of BYOL cannot be
    -- modified.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountModification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedTenancySupport', 'accountModification_dedicatedTenancySupport' - The status of BYOL (whether BYOL is being enabled or disabled).
--
-- 'startTime', 'accountModification_startTime' - The timestamp when the modification of the BYOL configuration was
-- started.
--
-- 'dedicatedTenancyManagementCidrRange', 'accountModification_dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface used for the account.
--
-- 'modificationState', 'accountModification_modificationState' - The state of the modification to the configuration of BYOL.
--
-- 'errorMessage', 'accountModification_errorMessage' - The text of the error message that is returned if the configuration of
-- BYOL cannot be modified.
--
-- 'errorCode', 'accountModification_errorCode' - The error code that is returned if the configuration of BYOL cannot be
-- modified.
newAccountModification ::
  AccountModification
newAccountModification =
  AccountModification'
    { dedicatedTenancySupport =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      dedicatedTenancyManagementCidrRange =
        Prelude.Nothing,
      modificationState = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The status of BYOL (whether BYOL is being enabled or disabled).
accountModification_dedicatedTenancySupport :: Lens.Lens' AccountModification (Prelude.Maybe DedicatedTenancySupportResultEnum)
accountModification_dedicatedTenancySupport = Lens.lens (\AccountModification' {dedicatedTenancySupport} -> dedicatedTenancySupport) (\s@AccountModification' {} a -> s {dedicatedTenancySupport = a} :: AccountModification)

-- | The timestamp when the modification of the BYOL configuration was
-- started.
accountModification_startTime :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.UTCTime)
accountModification_startTime = Lens.lens (\AccountModification' {startTime} -> startTime) (\s@AccountModification' {} a -> s {startTime = a} :: AccountModification) Prelude.. Lens.mapping Prelude._Time

-- | The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface used for the account.
accountModification_dedicatedTenancyManagementCidrRange :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.Text)
accountModification_dedicatedTenancyManagementCidrRange = Lens.lens (\AccountModification' {dedicatedTenancyManagementCidrRange} -> dedicatedTenancyManagementCidrRange) (\s@AccountModification' {} a -> s {dedicatedTenancyManagementCidrRange = a} :: AccountModification)

-- | The state of the modification to the configuration of BYOL.
accountModification_modificationState :: Lens.Lens' AccountModification (Prelude.Maybe DedicatedTenancyModificationStateEnum)
accountModification_modificationState = Lens.lens (\AccountModification' {modificationState} -> modificationState) (\s@AccountModification' {} a -> s {modificationState = a} :: AccountModification)

-- | The text of the error message that is returned if the configuration of
-- BYOL cannot be modified.
accountModification_errorMessage :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.Text)
accountModification_errorMessage = Lens.lens (\AccountModification' {errorMessage} -> errorMessage) (\s@AccountModification' {} a -> s {errorMessage = a} :: AccountModification)

-- | The error code that is returned if the configuration of BYOL cannot be
-- modified.
accountModification_errorCode :: Lens.Lens' AccountModification (Prelude.Maybe Prelude.Text)
accountModification_errorCode = Lens.lens (\AccountModification' {errorCode} -> errorCode) (\s@AccountModification' {} a -> s {errorCode = a} :: AccountModification)

instance Prelude.FromJSON AccountModification where
  parseJSON =
    Prelude.withObject
      "AccountModification"
      ( \x ->
          AccountModification'
            Prelude.<$> (x Prelude..:? "DedicatedTenancySupport")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "DedicatedTenancyManagementCidrRange")
            Prelude.<*> (x Prelude..:? "ModificationState")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable AccountModification

instance Prelude.NFData AccountModification
