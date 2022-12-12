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
-- Module      : Amazonka.Connect.Types.UserPhoneConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserPhoneConfig where

import Amazonka.Connect.Types.PhoneType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the phone configuration settings for a user.
--
-- /See:/ 'newUserPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { -- | The After Call Work (ACW) timeout setting, in seconds.
    --
    -- When returned by a @SearchUsers@ call, @AfterContactWorkTimeLimit@ is
    -- returned in milliseconds.
    afterContactWorkTimeLimit :: Prelude.Maybe Prelude.Natural,
    -- | The Auto accept setting.
    autoAccept :: Prelude.Maybe Prelude.Bool,
    -- | The phone number for the user\'s desk phone.
    deskPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The phone type.
    phoneType :: PhoneType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPhoneConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'afterContactWorkTimeLimit', 'userPhoneConfig_afterContactWorkTimeLimit' - The After Call Work (ACW) timeout setting, in seconds.
--
-- When returned by a @SearchUsers@ call, @AfterContactWorkTimeLimit@ is
-- returned in milliseconds.
--
-- 'autoAccept', 'userPhoneConfig_autoAccept' - The Auto accept setting.
--
-- 'deskPhoneNumber', 'userPhoneConfig_deskPhoneNumber' - The phone number for the user\'s desk phone.
--
-- 'phoneType', 'userPhoneConfig_phoneType' - The phone type.
newUserPhoneConfig ::
  -- | 'phoneType'
  PhoneType ->
  UserPhoneConfig
newUserPhoneConfig pPhoneType_ =
  UserPhoneConfig'
    { afterContactWorkTimeLimit =
        Prelude.Nothing,
      autoAccept = Prelude.Nothing,
      deskPhoneNumber = Prelude.Nothing,
      phoneType = pPhoneType_
    }

-- | The After Call Work (ACW) timeout setting, in seconds.
--
-- When returned by a @SearchUsers@ call, @AfterContactWorkTimeLimit@ is
-- returned in milliseconds.
userPhoneConfig_afterContactWorkTimeLimit :: Lens.Lens' UserPhoneConfig (Prelude.Maybe Prelude.Natural)
userPhoneConfig_afterContactWorkTimeLimit = Lens.lens (\UserPhoneConfig' {afterContactWorkTimeLimit} -> afterContactWorkTimeLimit) (\s@UserPhoneConfig' {} a -> s {afterContactWorkTimeLimit = a} :: UserPhoneConfig)

-- | The Auto accept setting.
userPhoneConfig_autoAccept :: Lens.Lens' UserPhoneConfig (Prelude.Maybe Prelude.Bool)
userPhoneConfig_autoAccept = Lens.lens (\UserPhoneConfig' {autoAccept} -> autoAccept) (\s@UserPhoneConfig' {} a -> s {autoAccept = a} :: UserPhoneConfig)

-- | The phone number for the user\'s desk phone.
userPhoneConfig_deskPhoneNumber :: Lens.Lens' UserPhoneConfig (Prelude.Maybe Prelude.Text)
userPhoneConfig_deskPhoneNumber = Lens.lens (\UserPhoneConfig' {deskPhoneNumber} -> deskPhoneNumber) (\s@UserPhoneConfig' {} a -> s {deskPhoneNumber = a} :: UserPhoneConfig)

-- | The phone type.
userPhoneConfig_phoneType :: Lens.Lens' UserPhoneConfig PhoneType
userPhoneConfig_phoneType = Lens.lens (\UserPhoneConfig' {phoneType} -> phoneType) (\s@UserPhoneConfig' {} a -> s {phoneType = a} :: UserPhoneConfig)

instance Data.FromJSON UserPhoneConfig where
  parseJSON =
    Data.withObject
      "UserPhoneConfig"
      ( \x ->
          UserPhoneConfig'
            Prelude.<$> (x Data..:? "AfterContactWorkTimeLimit")
            Prelude.<*> (x Data..:? "AutoAccept")
            Prelude.<*> (x Data..:? "DeskPhoneNumber")
            Prelude.<*> (x Data..: "PhoneType")
      )

instance Prelude.Hashable UserPhoneConfig where
  hashWithSalt _salt UserPhoneConfig' {..} =
    _salt
      `Prelude.hashWithSalt` afterContactWorkTimeLimit
      `Prelude.hashWithSalt` autoAccept
      `Prelude.hashWithSalt` deskPhoneNumber
      `Prelude.hashWithSalt` phoneType

instance Prelude.NFData UserPhoneConfig where
  rnf UserPhoneConfig' {..} =
    Prelude.rnf afterContactWorkTimeLimit
      `Prelude.seq` Prelude.rnf autoAccept
      `Prelude.seq` Prelude.rnf deskPhoneNumber
      `Prelude.seq` Prelude.rnf phoneType

instance Data.ToJSON UserPhoneConfig where
  toJSON UserPhoneConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AfterContactWorkTimeLimit" Data..=)
              Prelude.<$> afterContactWorkTimeLimit,
            ("AutoAccept" Data..=) Prelude.<$> autoAccept,
            ("DeskPhoneNumber" Data..=)
              Prelude.<$> deskPhoneNumber,
            Prelude.Just ("PhoneType" Data..= phoneType)
          ]
      )
