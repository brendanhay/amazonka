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
-- Module      : Network.AWS.IoT.Types.GroupNameAndArn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.GroupNameAndArn where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The name and ARN of a group.
--
-- /See:/ 'newGroupNameAndArn' smart constructor.
data GroupNameAndArn = GroupNameAndArn'
  { -- | The group name.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The group ARN.
    groupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { groupName = Prelude.Nothing,
      groupArn = Prelude.Nothing
    }

-- | The group name.
groupNameAndArn_groupName :: Lens.Lens' GroupNameAndArn (Prelude.Maybe Prelude.Text)
groupNameAndArn_groupName = Lens.lens (\GroupNameAndArn' {groupName} -> groupName) (\s@GroupNameAndArn' {} a -> s {groupName = a} :: GroupNameAndArn)

-- | The group ARN.
groupNameAndArn_groupArn :: Lens.Lens' GroupNameAndArn (Prelude.Maybe Prelude.Text)
groupNameAndArn_groupArn = Lens.lens (\GroupNameAndArn' {groupArn} -> groupArn) (\s@GroupNameAndArn' {} a -> s {groupArn = a} :: GroupNameAndArn)

instance Prelude.FromJSON GroupNameAndArn where
  parseJSON =
    Prelude.withObject
      "GroupNameAndArn"
      ( \x ->
          GroupNameAndArn'
            Prelude.<$> (x Prelude..:? "groupName")
            Prelude.<*> (x Prelude..:? "groupArn")
      )

instance Prelude.Hashable GroupNameAndArn

instance Prelude.NFData GroupNameAndArn
