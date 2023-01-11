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
-- Module      : Amazonka.GreengrassV2.Types.ComponentRunWith
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentRunWith where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.SystemResourceLimits
import qualified Amazonka.Prelude as Prelude

-- | Contains information system user and group that the IoT Greengrass Core
-- software uses to run component processes on the core device. For more
-- information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- /See:/ 'newComponentRunWith' smart constructor.
data ComponentRunWith = ComponentRunWith'
  { -- | The POSIX system user and, optionally, group to use to run this
    -- component on Linux core devices. The user, and group if specified, must
    -- exist on each Linux core device. Specify the user and group separated by
    -- a colon (@:@) in the following format: @user:group@. The group is
    -- optional. If you don\'t specify a group, the IoT Greengrass Core
    -- software uses the primary user for the group.
    --
    -- If you omit this parameter, the IoT Greengrass Core software uses the
    -- default system user and group that you configure on the Greengrass
    -- nucleus component. For more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>.
    posixUser :: Prelude.Maybe Prelude.Text,
    -- | The system resource limits to apply to this component\'s process on the
    -- core device. IoT Greengrass currently supports this feature on only
    -- Linux core devices.
    --
    -- If you omit this parameter, the IoT Greengrass Core software uses the
    -- default system resource limits that you configure on the Greengrass
    -- nucleus component. For more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-system-resource-limits Configure system resource limits for components>.
    systemResourceLimits :: Prelude.Maybe SystemResourceLimits,
    -- | The Windows user to use to run this component on Windows core devices.
    -- The user must exist on each Windows core device, and its name and
    -- password must be in the LocalSystem account\'s Credentials Manager
    -- instance.
    --
    -- If you omit this parameter, the IoT Greengrass Core software uses the
    -- default Windows user that you configure on the Greengrass nucleus
    -- component. For more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>.
    windowsUser :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentRunWith' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'posixUser', 'componentRunWith_posixUser' - The POSIX system user and, optionally, group to use to run this
-- component on Linux core devices. The user, and group if specified, must
-- exist on each Linux core device. Specify the user and group separated by
-- a colon (@:@) in the following format: @user:group@. The group is
-- optional. If you don\'t specify a group, the IoT Greengrass Core
-- software uses the primary user for the group.
--
-- If you omit this parameter, the IoT Greengrass Core software uses the
-- default system user and group that you configure on the Greengrass
-- nucleus component. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>.
--
-- 'systemResourceLimits', 'componentRunWith_systemResourceLimits' - The system resource limits to apply to this component\'s process on the
-- core device. IoT Greengrass currently supports this feature on only
-- Linux core devices.
--
-- If you omit this parameter, the IoT Greengrass Core software uses the
-- default system resource limits that you configure on the Greengrass
-- nucleus component. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-system-resource-limits Configure system resource limits for components>.
--
-- 'windowsUser', 'componentRunWith_windowsUser' - The Windows user to use to run this component on Windows core devices.
-- The user must exist on each Windows core device, and its name and
-- password must be in the LocalSystem account\'s Credentials Manager
-- instance.
--
-- If you omit this parameter, the IoT Greengrass Core software uses the
-- default Windows user that you configure on the Greengrass nucleus
-- component. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>.
newComponentRunWith ::
  ComponentRunWith
newComponentRunWith =
  ComponentRunWith'
    { posixUser = Prelude.Nothing,
      systemResourceLimits = Prelude.Nothing,
      windowsUser = Prelude.Nothing
    }

-- | The POSIX system user and, optionally, group to use to run this
-- component on Linux core devices. The user, and group if specified, must
-- exist on each Linux core device. Specify the user and group separated by
-- a colon (@:@) in the following format: @user:group@. The group is
-- optional. If you don\'t specify a group, the IoT Greengrass Core
-- software uses the primary user for the group.
--
-- If you omit this parameter, the IoT Greengrass Core software uses the
-- default system user and group that you configure on the Greengrass
-- nucleus component. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>.
componentRunWith_posixUser :: Lens.Lens' ComponentRunWith (Prelude.Maybe Prelude.Text)
componentRunWith_posixUser = Lens.lens (\ComponentRunWith' {posixUser} -> posixUser) (\s@ComponentRunWith' {} a -> s {posixUser = a} :: ComponentRunWith)

-- | The system resource limits to apply to this component\'s process on the
-- core device. IoT Greengrass currently supports this feature on only
-- Linux core devices.
--
-- If you omit this parameter, the IoT Greengrass Core software uses the
-- default system resource limits that you configure on the Greengrass
-- nucleus component. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-system-resource-limits Configure system resource limits for components>.
componentRunWith_systemResourceLimits :: Lens.Lens' ComponentRunWith (Prelude.Maybe SystemResourceLimits)
componentRunWith_systemResourceLimits = Lens.lens (\ComponentRunWith' {systemResourceLimits} -> systemResourceLimits) (\s@ComponentRunWith' {} a -> s {systemResourceLimits = a} :: ComponentRunWith)

-- | The Windows user to use to run this component on Windows core devices.
-- The user must exist on each Windows core device, and its name and
-- password must be in the LocalSystem account\'s Credentials Manager
-- instance.
--
-- If you omit this parameter, the IoT Greengrass Core software uses the
-- default Windows user that you configure on the Greengrass nucleus
-- component. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>.
componentRunWith_windowsUser :: Lens.Lens' ComponentRunWith (Prelude.Maybe Prelude.Text)
componentRunWith_windowsUser = Lens.lens (\ComponentRunWith' {windowsUser} -> windowsUser) (\s@ComponentRunWith' {} a -> s {windowsUser = a} :: ComponentRunWith)

instance Data.FromJSON ComponentRunWith where
  parseJSON =
    Data.withObject
      "ComponentRunWith"
      ( \x ->
          ComponentRunWith'
            Prelude.<$> (x Data..:? "posixUser")
            Prelude.<*> (x Data..:? "systemResourceLimits")
            Prelude.<*> (x Data..:? "windowsUser")
      )

instance Prelude.Hashable ComponentRunWith where
  hashWithSalt _salt ComponentRunWith' {..} =
    _salt `Prelude.hashWithSalt` posixUser
      `Prelude.hashWithSalt` systemResourceLimits
      `Prelude.hashWithSalt` windowsUser

instance Prelude.NFData ComponentRunWith where
  rnf ComponentRunWith' {..} =
    Prelude.rnf posixUser
      `Prelude.seq` Prelude.rnf systemResourceLimits
      `Prelude.seq` Prelude.rnf windowsUser

instance Data.ToJSON ComponentRunWith where
  toJSON ComponentRunWith' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("posixUser" Data..=) Prelude.<$> posixUser,
            ("systemResourceLimits" Data..=)
              Prelude.<$> systemResourceLimits,
            ("windowsUser" Data..=) Prelude.<$> windowsUser
          ]
      )
