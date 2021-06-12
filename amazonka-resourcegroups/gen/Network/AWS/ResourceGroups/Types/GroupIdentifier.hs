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
-- Module      : Network.AWS.ResourceGroups.Types.GroupIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupIdentifier where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The unique identifiers for a resource group.
--
-- /See:/ 'newGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { -- | The name of the resource group.
    groupName :: Core.Maybe Core.Text,
    -- | The ARN of the resource group.
    groupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'groupIdentifier_groupName' - The name of the resource group.
--
-- 'groupArn', 'groupIdentifier_groupArn' - The ARN of the resource group.
newGroupIdentifier ::
  GroupIdentifier
newGroupIdentifier =
  GroupIdentifier'
    { groupName = Core.Nothing,
      groupArn = Core.Nothing
    }

-- | The name of the resource group.
groupIdentifier_groupName :: Lens.Lens' GroupIdentifier (Core.Maybe Core.Text)
groupIdentifier_groupName = Lens.lens (\GroupIdentifier' {groupName} -> groupName) (\s@GroupIdentifier' {} a -> s {groupName = a} :: GroupIdentifier)

-- | The ARN of the resource group.
groupIdentifier_groupArn :: Lens.Lens' GroupIdentifier (Core.Maybe Core.Text)
groupIdentifier_groupArn = Lens.lens (\GroupIdentifier' {groupArn} -> groupArn) (\s@GroupIdentifier' {} a -> s {groupArn = a} :: GroupIdentifier)

instance Core.FromJSON GroupIdentifier where
  parseJSON =
    Core.withObject
      "GroupIdentifier"
      ( \x ->
          GroupIdentifier'
            Core.<$> (x Core..:? "GroupName")
            Core.<*> (x Core..:? "GroupArn")
      )

instance Core.Hashable GroupIdentifier

instance Core.NFData GroupIdentifier
