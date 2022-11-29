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
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the phone configuration settings for a user.
--
-- /See:/ 'newUserPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { -- | The Auto accept setting.
    autoAccept :: Prelude.Maybe Prelude.Bool,
    -- | The phone number for the user\'s desk phone.
    deskPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The After Call Work (ACW) timeout setting, in seconds.
    --
    -- When returned by a @SearchUsers@ call, @AfterContactWorkTimeLimit@ is
    -- returned in milliseconds.
    afterContactWorkTimeLimit :: Prelude.Maybe Prelude.Natural,
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
-- 'autoAccept', 'userPhoneConfig_autoAccept' - The Auto accept setting.
--
-- 'deskPhoneNumber', 'userPhoneConfig_deskPhoneNumber' - The phone number for the user\'s desk phone.
--
-- 'afterContactWorkTimeLimit', 'userPhoneConfig_afterContactWorkTimeLimit' - The After Call Work (ACW) timeout setting, in seconds.
--
-- When returned by a @SearchUsers@ call, @AfterContactWorkTimeLimit@ is
-- returned in milliseconds.
--
-- 'phoneType', 'userPhoneConfig_phoneType' - The phone type.
newUserPhoneConfig ::
  -- | 'phoneType'
  PhoneType ->
  UserPhoneConfig
newUserPhoneConfig pPhoneType_ =
  UserPhoneConfig'
    { autoAccept = Prelude.Nothing,
      deskPhoneNumber = Prelude.Nothing,
      afterContactWorkTimeLimit = Prelude.Nothing,
      phoneType = pPhoneType_
    }

-- | The Auto accept setting.
userPhoneConfig_autoAccept :: Lens.Lens' UserPhoneConfig (Prelude.Maybe Prelude.Bool)
userPhoneConfig_autoAccept = Lens.lens (\UserPhoneConfig' {autoAccept} -> autoAccept) (\s@UserPhoneConfig' {} a -> s {autoAccept = a} :: UserPhoneConfig)

-- | The phone number for the user\'s desk phone.
userPhoneConfig_deskPhoneNumber :: Lens.Lens' UserPhoneConfig (Prelude.Maybe Prelude.Text)
userPhoneConfig_deskPhoneNumber = Lens.lens (\UserPhoneConfig' {deskPhoneNumber} -> deskPhoneNumber) (\s@UserPhoneConfig' {} a -> s {deskPhoneNumber = a} :: UserPhoneConfig)

-- | The After Call Work (ACW) timeout setting, in seconds.
--
-- When returned by a @SearchUsers@ call, @AfterContactWorkTimeLimit@ is
-- returned in milliseconds.
userPhoneConfig_afterContactWorkTimeLimit :: Lens.Lens' UserPhoneConfig (Prelude.Maybe Prelude.Natural)
userPhoneConfig_afterContactWorkTimeLimit = Lens.lens (\UserPhoneConfig' {afterContactWorkTimeLimit} -> afterContactWorkTimeLimit) (\s@UserPhoneConfig' {} a -> s {afterContactWorkTimeLimit = a} :: UserPhoneConfig)

-- | The phone type.
userPhoneConfig_phoneType :: Lens.Lens' UserPhoneConfig PhoneType
userPhoneConfig_phoneType = Lens.lens (\UserPhoneConfig' {phoneType} -> phoneType) (\s@UserPhoneConfig' {} a -> s {phoneType = a} :: UserPhoneConfig)

instance Core.FromJSON UserPhoneConfig where
  parseJSON =
    Core.withObject
      "UserPhoneConfig"
      ( \x ->
          UserPhoneConfig'
            Prelude.<$> (x Core..:? "AutoAccept")
            Prelude.<*> (x Core..:? "DeskPhoneNumber")
            Prelude.<*> (x Core..:? "AfterContactWorkTimeLimit")
            Prelude.<*> (x Core..: "PhoneType")
      )

instance Prelude.Hashable UserPhoneConfig where
  hashWithSalt _salt UserPhoneConfig' {..} =
    _salt `Prelude.hashWithSalt` autoAccept
      `Prelude.hashWithSalt` deskPhoneNumber
      `Prelude.hashWithSalt` afterContactWorkTimeLimit
      `Prelude.hashWithSalt` phoneType

instance Prelude.NFData UserPhoneConfig where
  rnf UserPhoneConfig' {..} =
    Prelude.rnf autoAccept
      `Prelude.seq` Prelude.rnf deskPhoneNumber
      `Prelude.seq` Prelude.rnf afterContactWorkTimeLimit
      `Prelude.seq` Prelude.rnf phoneType

instance Core.ToJSON UserPhoneConfig where
  toJSON UserPhoneConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AutoAccept" Core..=) Prelude.<$> autoAccept,
            ("DeskPhoneNumber" Core..=)
              Prelude.<$> deskPhoneNumber,
            ("AfterContactWorkTimeLimit" Core..=)
              Prelude.<$> afterContactWorkTimeLimit,
            Prelude.Just ("PhoneType" Core..= phoneType)
          ]
      )
