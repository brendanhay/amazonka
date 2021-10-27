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
-- Module      : Network.AWS.IdentityStore.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IdentityStore.Types.Group where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A group object, which contains a specified group’s metadata and
-- attributes.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text,
    -- | Contains the group’s display name value. The length limit is 1,024
    -- characters. This value can consist of letters, accented characters,
    -- symbols, numbers, punctuation, tab, new line, carriage return, space,
    -- and nonbreaking space in this attribute. The characters @\<>;:%@ are
    -- excluded. This value is specified at the time the group is created and
    -- stored as an attribute of the group object in the identity store.
    displayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'group_groupId' - The identifier for a group in the identity store.
--
-- 'displayName', 'group_displayName' - Contains the group’s display name value. The length limit is 1,024
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, punctuation, tab, new line, carriage return, space,
-- and nonbreaking space in this attribute. The characters @\<>;:%@ are
-- excluded. This value is specified at the time the group is created and
-- stored as an attribute of the group object in the identity store.
newGroup ::
  -- | 'groupId'
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  Group
newGroup pGroupId_ pDisplayName_ =
  Group'
    { groupId = pGroupId_,
      displayName = pDisplayName_
    }

-- | The identifier for a group in the identity store.
group_groupId :: Lens.Lens' Group Prelude.Text
group_groupId = Lens.lens (\Group' {groupId} -> groupId) (\s@Group' {} a -> s {groupId = a} :: Group)

-- | Contains the group’s display name value. The length limit is 1,024
-- characters. This value can consist of letters, accented characters,
-- symbols, numbers, punctuation, tab, new line, carriage return, space,
-- and nonbreaking space in this attribute. The characters @\<>;:%@ are
-- excluded. This value is specified at the time the group is created and
-- stored as an attribute of the group object in the identity store.
group_displayName :: Lens.Lens' Group Prelude.Text
group_displayName = Lens.lens (\Group' {displayName} -> displayName) (\s@Group' {} a -> s {displayName = a} :: Group)

instance Core.FromJSON Group where
  parseJSON =
    Core.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Core..: "GroupId")
            Prelude.<*> (x Core..: "DisplayName")
      )

instance Prelude.Hashable Group

instance Prelude.NFData Group
