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
-- Module      : Network.AWS.OpsWorksCM.Types.AccountAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.AccountAttribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Stores account attributes.
--
-- /See:/ 'newAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | The current usage, such as the current number of servers that are
    -- associated with the account.
    used :: Core.Maybe Core.Int,
    -- | The attribute name. The following are supported attribute names.
    --
    -- -   /ServerLimit:/ The number of current servers\/maximum number of
    --     servers allowed. By default, you can have a maximum of 10 servers.
    --
    -- -   /ManualBackupLimit:/ The number of current manual backups\/maximum
    --     number of backups allowed. By default, you can have a maximum of 50
    --     manual backups saved.
    name :: Core.Maybe Core.Text,
    -- | The maximum allowed value.
    maximum :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccountAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'used', 'accountAttribute_used' - The current usage, such as the current number of servers that are
-- associated with the account.
--
-- 'name', 'accountAttribute_name' - The attribute name. The following are supported attribute names.
--
-- -   /ServerLimit:/ The number of current servers\/maximum number of
--     servers allowed. By default, you can have a maximum of 10 servers.
--
-- -   /ManualBackupLimit:/ The number of current manual backups\/maximum
--     number of backups allowed. By default, you can have a maximum of 50
--     manual backups saved.
--
-- 'maximum', 'accountAttribute_maximum' - The maximum allowed value.
newAccountAttribute ::
  AccountAttribute
newAccountAttribute =
  AccountAttribute'
    { used = Core.Nothing,
      name = Core.Nothing,
      maximum = Core.Nothing
    }

-- | The current usage, such as the current number of servers that are
-- associated with the account.
accountAttribute_used :: Lens.Lens' AccountAttribute (Core.Maybe Core.Int)
accountAttribute_used = Lens.lens (\AccountAttribute' {used} -> used) (\s@AccountAttribute' {} a -> s {used = a} :: AccountAttribute)

-- | The attribute name. The following are supported attribute names.
--
-- -   /ServerLimit:/ The number of current servers\/maximum number of
--     servers allowed. By default, you can have a maximum of 10 servers.
--
-- -   /ManualBackupLimit:/ The number of current manual backups\/maximum
--     number of backups allowed. By default, you can have a maximum of 50
--     manual backups saved.
accountAttribute_name :: Lens.Lens' AccountAttribute (Core.Maybe Core.Text)
accountAttribute_name = Lens.lens (\AccountAttribute' {name} -> name) (\s@AccountAttribute' {} a -> s {name = a} :: AccountAttribute)

-- | The maximum allowed value.
accountAttribute_maximum :: Lens.Lens' AccountAttribute (Core.Maybe Core.Int)
accountAttribute_maximum = Lens.lens (\AccountAttribute' {maximum} -> maximum) (\s@AccountAttribute' {} a -> s {maximum = a} :: AccountAttribute)

instance Core.FromJSON AccountAttribute where
  parseJSON =
    Core.withObject
      "AccountAttribute"
      ( \x ->
          AccountAttribute'
            Core.<$> (x Core..:? "Used")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Maximum")
      )

instance Core.Hashable AccountAttribute

instance Core.NFData AccountAttribute
