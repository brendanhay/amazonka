{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.LaunchPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchPermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a launch permission.
--
-- /See:/ 'newLaunchPermission' smart constructor.
data LaunchPermission = LaunchPermission'
  { -- | The name of the group.
    group' :: Prelude.Maybe PermissionGroup,
    -- | The AWS account ID.
    --
    -- Constraints: Up to 10 000 account IDs can be specified in a single
    -- request.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'launchPermission_group' - The name of the group.
--
-- 'userId', 'launchPermission_userId' - The AWS account ID.
--
-- Constraints: Up to 10 000 account IDs can be specified in a single
-- request.
newLaunchPermission ::
  LaunchPermission
newLaunchPermission =
  LaunchPermission'
    { group' = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The name of the group.
launchPermission_group :: Lens.Lens' LaunchPermission (Prelude.Maybe PermissionGroup)
launchPermission_group = Lens.lens (\LaunchPermission' {group'} -> group') (\s@LaunchPermission' {} a -> s {group' = a} :: LaunchPermission)

-- | The AWS account ID.
--
-- Constraints: Up to 10 000 account IDs can be specified in a single
-- request.
launchPermission_userId :: Lens.Lens' LaunchPermission (Prelude.Maybe Prelude.Text)
launchPermission_userId = Lens.lens (\LaunchPermission' {userId} -> userId) (\s@LaunchPermission' {} a -> s {userId = a} :: LaunchPermission)

instance Prelude.FromXML LaunchPermission where
  parseXML x =
    LaunchPermission'
      Prelude.<$> (x Prelude..@? "group")
      Prelude.<*> (x Prelude..@? "userId")

instance Prelude.Hashable LaunchPermission

instance Prelude.NFData LaunchPermission

instance Prelude.ToQuery LaunchPermission where
  toQuery LaunchPermission' {..} =
    Prelude.mconcat
      [ "Group" Prelude.=: group',
        "UserId" Prelude.=: userId
      ]
