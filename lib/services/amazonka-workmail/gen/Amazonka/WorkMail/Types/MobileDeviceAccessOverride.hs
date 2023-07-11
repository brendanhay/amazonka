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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The date the override was first created.
    dateCreated :: Prelude.Maybe Data.POSIX,
    -- | The date the override was last modified.
    dateModified :: Prelude.Maybe Data.POSIX,
    -- | A description of the override.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device to which the override applies.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The effect of the override, @ALLOW@ or @DENY@.
    effect :: Prelude.Maybe MobileDeviceAccessRuleEffect,
    -- | The WorkMail user to which the access override applies.
    userId :: Prelude.Maybe Prelude.Text
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
-- 'dateCreated', 'mobileDeviceAccessOverride_dateCreated' - The date the override was first created.
--
-- 'dateModified', 'mobileDeviceAccessOverride_dateModified' - The date the override was last modified.
--
-- 'description', 'mobileDeviceAccessOverride_description' - A description of the override.
--
-- 'deviceId', 'mobileDeviceAccessOverride_deviceId' - The device to which the override applies.
--
-- 'effect', 'mobileDeviceAccessOverride_effect' - The effect of the override, @ALLOW@ or @DENY@.
--
-- 'userId', 'mobileDeviceAccessOverride_userId' - The WorkMail user to which the access override applies.
newMobileDeviceAccessOverride ::
  MobileDeviceAccessOverride
newMobileDeviceAccessOverride =
  MobileDeviceAccessOverride'
    { dateCreated =
        Prelude.Nothing,
      dateModified = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      effect = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The date the override was first created.
mobileDeviceAccessOverride_dateCreated :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessOverride_dateCreated = Lens.lens (\MobileDeviceAccessOverride' {dateCreated} -> dateCreated) (\s@MobileDeviceAccessOverride' {} a -> s {dateCreated = a} :: MobileDeviceAccessOverride) Prelude.. Lens.mapping Data._Time

-- | The date the override was last modified.
mobileDeviceAccessOverride_dateModified :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.UTCTime)
mobileDeviceAccessOverride_dateModified = Lens.lens (\MobileDeviceAccessOverride' {dateModified} -> dateModified) (\s@MobileDeviceAccessOverride' {} a -> s {dateModified = a} :: MobileDeviceAccessOverride) Prelude.. Lens.mapping Data._Time

-- | A description of the override.
mobileDeviceAccessOverride_description :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.Text)
mobileDeviceAccessOverride_description = Lens.lens (\MobileDeviceAccessOverride' {description} -> description) (\s@MobileDeviceAccessOverride' {} a -> s {description = a} :: MobileDeviceAccessOverride)

-- | The device to which the override applies.
mobileDeviceAccessOverride_deviceId :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.Text)
mobileDeviceAccessOverride_deviceId = Lens.lens (\MobileDeviceAccessOverride' {deviceId} -> deviceId) (\s@MobileDeviceAccessOverride' {} a -> s {deviceId = a} :: MobileDeviceAccessOverride)

-- | The effect of the override, @ALLOW@ or @DENY@.
mobileDeviceAccessOverride_effect :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe MobileDeviceAccessRuleEffect)
mobileDeviceAccessOverride_effect = Lens.lens (\MobileDeviceAccessOverride' {effect} -> effect) (\s@MobileDeviceAccessOverride' {} a -> s {effect = a} :: MobileDeviceAccessOverride)

-- | The WorkMail user to which the access override applies.
mobileDeviceAccessOverride_userId :: Lens.Lens' MobileDeviceAccessOverride (Prelude.Maybe Prelude.Text)
mobileDeviceAccessOverride_userId = Lens.lens (\MobileDeviceAccessOverride' {userId} -> userId) (\s@MobileDeviceAccessOverride' {} a -> s {userId = a} :: MobileDeviceAccessOverride)

instance Data.FromJSON MobileDeviceAccessOverride where
  parseJSON =
    Data.withObject
      "MobileDeviceAccessOverride"
      ( \x ->
          MobileDeviceAccessOverride'
            Prelude.<$> (x Data..:? "DateCreated")
            Prelude.<*> (x Data..:? "DateModified")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "Effect")
            Prelude.<*> (x Data..:? "UserId")
      )

instance Prelude.Hashable MobileDeviceAccessOverride where
  hashWithSalt _salt MobileDeviceAccessOverride' {..} =
    _salt
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateModified
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` effect
      `Prelude.hashWithSalt` userId

instance Prelude.NFData MobileDeviceAccessOverride where
  rnf MobileDeviceAccessOverride' {..} =
    Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateModified
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf effect
      `Prelude.seq` Prelude.rnf userId
