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
-- Module      : Amazonka.SnowDeviceManagement.Types.Command
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.Command where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.Reboot
import Amazonka.SnowDeviceManagement.Types.Unlock

-- | The command given to the device to execute.
--
-- /See:/ 'newCommand' smart constructor.
data Command = Command'
  { -- | Reboots the device.
    reboot :: Prelude.Maybe Reboot,
    -- | Unlocks the device.
    unlock :: Prelude.Maybe Unlock
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Command' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reboot', 'command_reboot' - Reboots the device.
--
-- 'unlock', 'command_unlock' - Unlocks the device.
newCommand ::
  Command
newCommand =
  Command'
    { reboot = Prelude.Nothing,
      unlock = Prelude.Nothing
    }

-- | Reboots the device.
command_reboot :: Lens.Lens' Command (Prelude.Maybe Reboot)
command_reboot = Lens.lens (\Command' {reboot} -> reboot) (\s@Command' {} a -> s {reboot = a} :: Command)

-- | Unlocks the device.
command_unlock :: Lens.Lens' Command (Prelude.Maybe Unlock)
command_unlock = Lens.lens (\Command' {unlock} -> unlock) (\s@Command' {} a -> s {unlock = a} :: Command)

instance Prelude.Hashable Command where
  hashWithSalt _salt Command' {..} =
    _salt `Prelude.hashWithSalt` reboot
      `Prelude.hashWithSalt` unlock

instance Prelude.NFData Command where
  rnf Command' {..} =
    Prelude.rnf reboot `Prelude.seq` Prelude.rnf unlock

instance Core.ToJSON Command where
  toJSON Command' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("reboot" Core..=) Prelude.<$> reboot,
            ("unlock" Core..=) Prelude.<$> unlock
          ]
      )
