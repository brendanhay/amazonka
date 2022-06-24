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
-- Module      : Amazonka.EC2.Types.LaunchPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchPermission where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PermissionGroup
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch permission.
--
-- /See:/ 'newLaunchPermission' smart constructor.
data LaunchPermission = LaunchPermission'
  { -- | The Amazon Web Services account ID.
    --
    -- Constraints: Up to 10 000 account IDs can be specified in a single
    -- request.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    group' :: Prelude.Maybe PermissionGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'launchPermission_userId' - The Amazon Web Services account ID.
--
-- Constraints: Up to 10 000 account IDs can be specified in a single
-- request.
--
-- 'group'', 'launchPermission_group' - The name of the group.
newLaunchPermission ::
  LaunchPermission
newLaunchPermission =
  LaunchPermission'
    { userId = Prelude.Nothing,
      group' = Prelude.Nothing
    }

-- | The Amazon Web Services account ID.
--
-- Constraints: Up to 10 000 account IDs can be specified in a single
-- request.
launchPermission_userId :: Lens.Lens' LaunchPermission (Prelude.Maybe Prelude.Text)
launchPermission_userId = Lens.lens (\LaunchPermission' {userId} -> userId) (\s@LaunchPermission' {} a -> s {userId = a} :: LaunchPermission)

-- | The name of the group.
launchPermission_group :: Lens.Lens' LaunchPermission (Prelude.Maybe PermissionGroup)
launchPermission_group = Lens.lens (\LaunchPermission' {group'} -> group') (\s@LaunchPermission' {} a -> s {group' = a} :: LaunchPermission)

instance Core.FromXML LaunchPermission where
  parseXML x =
    LaunchPermission'
      Prelude.<$> (x Core..@? "userId")
      Prelude.<*> (x Core..@? "group")

instance Prelude.Hashable LaunchPermission where
  hashWithSalt _salt LaunchPermission' {..} =
    _salt `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` group'

instance Prelude.NFData LaunchPermission where
  rnf LaunchPermission' {..} =
    Prelude.rnf userId `Prelude.seq` Prelude.rnf group'

instance Core.ToQuery LaunchPermission where
  toQuery LaunchPermission' {..} =
    Prelude.mconcat
      ["UserId" Core.=: userId, "Group" Core.=: group']
