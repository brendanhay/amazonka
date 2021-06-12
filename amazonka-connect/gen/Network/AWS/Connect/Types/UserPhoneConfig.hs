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
-- Module      : Network.AWS.Connect.Types.UserPhoneConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserPhoneConfig where

import Network.AWS.Connect.Types.PhoneType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the phone configuration settings for a user.
--
-- /See:/ 'newUserPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { -- | The Auto accept setting.
    autoAccept :: Core.Maybe Core.Bool,
    -- | The After Call Work (ACW) timeout setting, in seconds.
    afterContactWorkTimeLimit :: Core.Maybe Core.Natural,
    -- | The phone number for the user\'s desk phone.
    deskPhoneNumber :: Core.Maybe Core.Text,
    -- | The phone type.
    phoneType :: PhoneType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'afterContactWorkTimeLimit', 'userPhoneConfig_afterContactWorkTimeLimit' - The After Call Work (ACW) timeout setting, in seconds.
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
    { autoAccept = Core.Nothing,
      afterContactWorkTimeLimit = Core.Nothing,
      deskPhoneNumber = Core.Nothing,
      phoneType = pPhoneType_
    }

-- | The Auto accept setting.
userPhoneConfig_autoAccept :: Lens.Lens' UserPhoneConfig (Core.Maybe Core.Bool)
userPhoneConfig_autoAccept = Lens.lens (\UserPhoneConfig' {autoAccept} -> autoAccept) (\s@UserPhoneConfig' {} a -> s {autoAccept = a} :: UserPhoneConfig)

-- | The After Call Work (ACW) timeout setting, in seconds.
userPhoneConfig_afterContactWorkTimeLimit :: Lens.Lens' UserPhoneConfig (Core.Maybe Core.Natural)
userPhoneConfig_afterContactWorkTimeLimit = Lens.lens (\UserPhoneConfig' {afterContactWorkTimeLimit} -> afterContactWorkTimeLimit) (\s@UserPhoneConfig' {} a -> s {afterContactWorkTimeLimit = a} :: UserPhoneConfig)

-- | The phone number for the user\'s desk phone.
userPhoneConfig_deskPhoneNumber :: Lens.Lens' UserPhoneConfig (Core.Maybe Core.Text)
userPhoneConfig_deskPhoneNumber = Lens.lens (\UserPhoneConfig' {deskPhoneNumber} -> deskPhoneNumber) (\s@UserPhoneConfig' {} a -> s {deskPhoneNumber = a} :: UserPhoneConfig)

-- | The phone type.
userPhoneConfig_phoneType :: Lens.Lens' UserPhoneConfig PhoneType
userPhoneConfig_phoneType = Lens.lens (\UserPhoneConfig' {phoneType} -> phoneType) (\s@UserPhoneConfig' {} a -> s {phoneType = a} :: UserPhoneConfig)

instance Core.FromJSON UserPhoneConfig where
  parseJSON =
    Core.withObject
      "UserPhoneConfig"
      ( \x ->
          UserPhoneConfig'
            Core.<$> (x Core..:? "AutoAccept")
            Core.<*> (x Core..:? "AfterContactWorkTimeLimit")
            Core.<*> (x Core..:? "DeskPhoneNumber")
            Core.<*> (x Core..: "PhoneType")
      )

instance Core.Hashable UserPhoneConfig

instance Core.NFData UserPhoneConfig

instance Core.ToJSON UserPhoneConfig where
  toJSON UserPhoneConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AutoAccept" Core..=) Core.<$> autoAccept,
            ("AfterContactWorkTimeLimit" Core..=)
              Core.<$> afterContactWorkTimeLimit,
            ("DeskPhoneNumber" Core..=) Core.<$> deskPhoneNumber,
            Core.Just ("PhoneType" Core..= phoneType)
          ]
      )
