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
-- Module      : Network.AWS.IoT.Types.GroupNameAndArn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.GroupNameAndArn where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The name and ARN of a group.
--
-- /See:/ 'newGroupNameAndArn' smart constructor.
data GroupNameAndArn = GroupNameAndArn'
  { -- | The group name.
    groupName :: Core.Maybe Core.Text,
    -- | The group ARN.
    groupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupNameAndArn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'groupNameAndArn_groupName' - The group name.
--
-- 'groupArn', 'groupNameAndArn_groupArn' - The group ARN.
newGroupNameAndArn ::
  GroupNameAndArn
newGroupNameAndArn =
  GroupNameAndArn'
    { groupName = Core.Nothing,
      groupArn = Core.Nothing
    }

-- | The group name.
groupNameAndArn_groupName :: Lens.Lens' GroupNameAndArn (Core.Maybe Core.Text)
groupNameAndArn_groupName = Lens.lens (\GroupNameAndArn' {groupName} -> groupName) (\s@GroupNameAndArn' {} a -> s {groupName = a} :: GroupNameAndArn)

-- | The group ARN.
groupNameAndArn_groupArn :: Lens.Lens' GroupNameAndArn (Core.Maybe Core.Text)
groupNameAndArn_groupArn = Lens.lens (\GroupNameAndArn' {groupArn} -> groupArn) (\s@GroupNameAndArn' {} a -> s {groupArn = a} :: GroupNameAndArn)

instance Core.FromJSON GroupNameAndArn where
  parseJSON =
    Core.withObject
      "GroupNameAndArn"
      ( \x ->
          GroupNameAndArn'
            Core.<$> (x Core..:? "groupName")
            Core.<*> (x Core..:? "groupArn")
      )

instance Core.Hashable GroupNameAndArn

instance Core.NFData GroupNameAndArn
