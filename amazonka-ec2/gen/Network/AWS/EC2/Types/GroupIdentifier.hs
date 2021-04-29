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
-- Module      : Network.AWS.EC2.Types.GroupIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GroupIdentifier where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a security group.
--
-- /See:/ 'newGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text
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
-- 'groupName', 'groupIdentifier_groupName' - The name of the security group.
--
-- 'groupId', 'groupIdentifier_groupId' - The ID of the security group.
newGroupIdentifier ::
  GroupIdentifier
newGroupIdentifier =
  GroupIdentifier'
    { groupName = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | The name of the security group.
groupIdentifier_groupName :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupName = Lens.lens (\GroupIdentifier' {groupName} -> groupName) (\s@GroupIdentifier' {} a -> s {groupName = a} :: GroupIdentifier)

-- | The ID of the security group.
groupIdentifier_groupId :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupId = Lens.lens (\GroupIdentifier' {groupId} -> groupId) (\s@GroupIdentifier' {} a -> s {groupId = a} :: GroupIdentifier)

instance Prelude.FromXML GroupIdentifier where
  parseXML x =
    GroupIdentifier'
      Prelude.<$> (x Prelude..@? "groupName")
      Prelude.<*> (x Prelude..@? "groupId")

instance Prelude.Hashable GroupIdentifier

instance Prelude.NFData GroupIdentifier

instance Prelude.ToQuery GroupIdentifier where
  toQuery GroupIdentifier' {..} =
    Prelude.mconcat
      [ "GroupName" Prelude.=: groupName,
        "GroupId" Prelude.=: groupId
      ]
