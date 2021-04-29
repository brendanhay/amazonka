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
-- Module      : Network.AWS.ResourceGroups.Types.GroupIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupIdentifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The unique identifiers for a resource group.
--
-- /See:/ 'newGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { -- | The name of the resource group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource group.
    groupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { groupName = Prelude.Nothing,
      groupArn = Prelude.Nothing
    }

-- | The name of the resource group.
groupIdentifier_groupName :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupName = Lens.lens (\GroupIdentifier' {groupName} -> groupName) (\s@GroupIdentifier' {} a -> s {groupName = a} :: GroupIdentifier)

-- | The ARN of the resource group.
groupIdentifier_groupArn :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupArn = Lens.lens (\GroupIdentifier' {groupArn} -> groupArn) (\s@GroupIdentifier' {} a -> s {groupArn = a} :: GroupIdentifier)

instance Prelude.FromJSON GroupIdentifier where
  parseJSON =
    Prelude.withObject
      "GroupIdentifier"
      ( \x ->
          GroupIdentifier'
            Prelude.<$> (x Prelude..:? "GroupName")
            Prelude.<*> (x Prelude..:? "GroupArn")
      )

instance Prelude.Hashable GroupIdentifier

instance Prelude.NFData GroupIdentifier
