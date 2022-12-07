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
-- Module      : Amazonka.WorkMail.Types.MobileDeviceAccessOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.MobileDeviceAccessOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.MobileDeviceAccessRuleEffect

-- | The override object.
--
-- /See:/ 'newMobileDeviceAccessOverride' smart constructor.
data MobileDeviceAccessOverride = MobileDeviceAccessOverride'
  { -- | The device to which the override applies.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The effect of the override, @ALLOW@ or @DENY@.
    effect :: Prelude.Maybe MobileDeviceAccessRuleEffect,
    -- | A description of the override.
    description :: Prelude.Maybe Prelude.Text,
    -- | The WorkMail user to which the access override applies.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The date the override was first created.
    dateCreated :: Prelude.Maybe Data.POSIX,
    -- | The date the override was last modified.
    dateModified :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MobileDeviceAccessOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'mobileDeviceAccessOverride_deviceId' - The device to which the override applies.
--
-- 'effect', 'mobileDeviceAccessOverride_effect' - The effect of the override, @ALLOW@ or @DENY@.
--
-- 'description', 'mobileDeviceAccessOverride_description' - A description of the override.
--
-- 'userId', 'mobileDeviceAccessOverride_userId' - The WorkMail user to which the access override applies.
--
-- 'dateCreated', 'mobileDeviceAccessOverride_dateCreated' - The date the override was first created.
--
-- 'dateModified', 'mobileDeviceAccessOverride_dateModified' - The date the override was last modified.
newMobileDeviceAccessOverride ::
  MobileDeviceAccessOverride
newMobileDeviceAccessOverride =
  MobileDeviceAccessOverride'
    { deviceId =
        Prelude.Nothing,
      effect = Prelude.Nothing,
      description = Prelude.Nothing,
      userId = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateModified = Prelude.Nothing
    }

-- | The device to which the override applies.
mobileDeviceAccessOverride_deviceId :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.Text)
mobileDeviceAccessOverride_deviceId = Lens.lens (\MobileDeviceAccessOverride' {deviceId} -> deviceId) (\s@MobileDeviceAccessOverride' {} a -> s {deviceId = a} :: MobileDeviceAccessOverride)

-- | The effect of the override, @ALLOW@ or @DENY@.
mobileDeviceAccessOverride_effect :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe MobileDeviceAccessRuleEffect)
mobileDeviceAccessOverride_effect = Lens.lens (\MobileDeviceAccessOverride' {effect} -> effect) (\s@MobileDeviceAccessOverride' {} a -> s {effect = a} :: MobileDeviceAccessOverride)

-- | A description of the override.
mobileDeviceAccessOverride_description :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.Text)
mobileDeviceAccessOverride_description = Lens.lens (\MobileDeviceAccessOverride' {description} -> description) (\s@MobileDeviceAccessOverride' {} a -> s {description = a} :: MobileDeviceAccessOverride)

-- | The WorkMail user to which the access override applies.
mobileDeviceAccessOverride_userId :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.Text)
mobileDeviceAccessOverride_userId = Lens.lens (\MobileDeviceAccessOverride' {userId} -> userId) (\s@MobileDeviceAccessOverride' {} a -> s {userId = a} :: MobileDeviceAccessOverride)

-- | The date the override was first created.
mobileDeviceAccessOverride_dateCreated :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessOverride_dateCreated = Lens.lens (\MobileDeviceAccessOverride' {dateCreated} -> dateCreated) (\s@MobileDeviceAccessOverride' {} a -> s {dateCreated = a} :: MobileDeviceAccessOverride) Prelude.. Lens.mapping Data._Time

-- | The date the override was last modified.
mobileDeviceAccessOverride_dateModified :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessOverride_dateModified = Lens.lens (\MobileDeviceAccessOverride' {dateModified} -> dateModified) (\s@MobileDeviceAccessOverride' {} a -> s {dateModified = a} :: MobileDeviceAccessOverride) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON MobileDeviceAccessOverride where
  parseJSON =
    Data.withObject
      "MobileDeviceAccessOverride"
      ( \x ->
          MobileDeviceAccessOverride'
            Prelude.<$> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "Effect")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "UserId")
            Prelude.<*> (x Data..:? "DateCreated")
            Prelude.<*> (x Data..:? "DateModified")
      )

instance Prelude.Hashable MobileDeviceAccessOverride where
  hashWithSalt _salt MobileDeviceAccessOverride' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` effect
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateModified

instance Prelude.NFData MobileDeviceAccessOverride where
  rnf MobileDeviceAccessOverride' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf effect
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateModified
