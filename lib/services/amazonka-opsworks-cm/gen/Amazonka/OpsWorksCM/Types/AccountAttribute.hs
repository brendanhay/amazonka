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
-- Module      : Amazonka.OpsWorksCM.Types.AccountAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Types.AccountAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Stores account attributes.
--
-- /See:/ 'newAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | The attribute name. The following are supported attribute names.
    --
    -- -   /ServerLimit:/ The number of current servers\/maximum number of
    --     servers allowed. By default, you can have a maximum of 10 servers.
    --
    -- -   /ManualBackupLimit:/ The number of current manual backups\/maximum
    --     number of backups allowed. By default, you can have a maximum of 50
    --     manual backups saved.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current usage, such as the current number of servers that are
    -- associated with the account.
    used :: Prelude.Maybe Prelude.Int,
    -- | The maximum allowed value.
    maximum :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'used', 'accountAttribute_used' - The current usage, such as the current number of servers that are
-- associated with the account.
--
-- 'maximum', 'accountAttribute_maximum' - The maximum allowed value.
newAccountAttribute ::
  AccountAttribute
newAccountAttribute =
  AccountAttribute'
    { name = Prelude.Nothing,
      used = Prelude.Nothing,
      maximum = Prelude.Nothing
    }

-- | The attribute name. The following are supported attribute names.
--
-- -   /ServerLimit:/ The number of current servers\/maximum number of
--     servers allowed. By default, you can have a maximum of 10 servers.
--
-- -   /ManualBackupLimit:/ The number of current manual backups\/maximum
--     number of backups allowed. By default, you can have a maximum of 50
--     manual backups saved.
accountAttribute_name :: Lens.Lens' AccountAttribute (Prelude.Maybe Prelude.Text)
accountAttribute_name = Lens.lens (\AccountAttribute' {name} -> name) (\s@AccountAttribute' {} a -> s {name = a} :: AccountAttribute)

-- | The current usage, such as the current number of servers that are
-- associated with the account.
accountAttribute_used :: Lens.Lens' AccountAttribute (Prelude.Maybe Prelude.Int)
accountAttribute_used = Lens.lens (\AccountAttribute' {used} -> used) (\s@AccountAttribute' {} a -> s {used = a} :: AccountAttribute)

-- | The maximum allowed value.
accountAttribute_maximum :: Lens.Lens' AccountAttribute (Prelude.Maybe Prelude.Int)
accountAttribute_maximum = Lens.lens (\AccountAttribute' {maximum} -> maximum) (\s@AccountAttribute' {} a -> s {maximum = a} :: AccountAttribute)

instance Data.FromJSON AccountAttribute where
  parseJSON =
    Data.withObject
      "AccountAttribute"
      ( \x ->
          AccountAttribute'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Used")
            Prelude.<*> (x Data..:? "Maximum")
      )

instance Prelude.Hashable AccountAttribute where
  hashWithSalt _salt AccountAttribute' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` used
      `Prelude.hashWithSalt` maximum

instance Prelude.NFData AccountAttribute where
  rnf AccountAttribute' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf used
      `Prelude.seq` Prelude.rnf maximum
