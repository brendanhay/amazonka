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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text
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
-- 'groupId', 'groupIdentifier_groupId' - The ID of the security group.
--
-- 'groupName', 'groupIdentifier_groupName' - The name of the security group.
newGroupIdentifier ::
  GroupIdentifier
newGroupIdentifier =
  GroupIdentifier'
    { groupId = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The ID of the security group.
groupIdentifier_groupId :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupId = Lens.lens (\GroupIdentifier' {groupId} -> groupId) (\s@GroupIdentifier' {} a -> s {groupId = a} :: GroupIdentifier)

-- | The name of the security group.
groupIdentifier_groupName :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupName = Lens.lens (\GroupIdentifier' {groupName} -> groupName) (\s@GroupIdentifier' {} a -> s {groupName = a} :: GroupIdentifier)

instance Data.FromXML GroupIdentifier where
  parseXML x =
    GroupIdentifier'
      Prelude.<$> (x Data..@? "groupId")
      Prelude.<*> (x Data..@? "groupName")

instance Prelude.Hashable GroupIdentifier where
  hashWithSalt _salt GroupIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData GroupIdentifier where
  rnf GroupIdentifier' {..} =
    Prelude.rnf groupId `Prelude.seq`
      Prelude.rnf groupName

instance Data.ToQuery GroupIdentifier where
  toQuery GroupIdentifier' {..} =
    Prelude.mconcat
      [ "GroupId" Data.=: groupId,
        "GroupName" Data.=: groupName
      ]
