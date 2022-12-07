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
-- Module      : Amazonka.EC2.Types.GroupIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.GroupIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a security group.
--
-- /See:/ 'newGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML GroupIdentifier where
  parseXML x =
    GroupIdentifier'
      Prelude.<$> (x Data..@? "groupName")
      Prelude.<*> (x Data..@? "groupId")

instance Prelude.Hashable GroupIdentifier where
  hashWithSalt _salt GroupIdentifier' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData GroupIdentifier where
  rnf GroupIdentifier' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToQuery GroupIdentifier where
  toQuery GroupIdentifier' {..} =
    Prelude.mconcat
      [ "GroupName" Data.=: groupName,
        "GroupId" Data.=: groupId
      ]
