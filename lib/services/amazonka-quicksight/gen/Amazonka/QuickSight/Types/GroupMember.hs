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
-- Module      : Amazonka.QuickSight.Types.GroupMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GroupMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A member of an Amazon QuickSight group. Currently, group members must be
-- users. Groups can\'t be members of another group. .
--
-- /See:/ 'newGroupMember' smart constructor.
data GroupMember = GroupMember'
  { -- | The Amazon Resource Name (ARN) for the group member (user).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the group member (user).
    memberName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'groupMember_arn' - The Amazon Resource Name (ARN) for the group member (user).
--
-- 'memberName', 'groupMember_memberName' - The name of the group member (user).
newGroupMember ::
  GroupMember
newGroupMember =
  GroupMember'
    { arn = Prelude.Nothing,
      memberName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the group member (user).
groupMember_arn :: Lens.Lens' GroupMember (Prelude.Maybe Prelude.Text)
groupMember_arn = Lens.lens (\GroupMember' {arn} -> arn) (\s@GroupMember' {} a -> s {arn = a} :: GroupMember)

-- | The name of the group member (user).
groupMember_memberName :: Lens.Lens' GroupMember (Prelude.Maybe Prelude.Text)
groupMember_memberName = Lens.lens (\GroupMember' {memberName} -> memberName) (\s@GroupMember' {} a -> s {memberName = a} :: GroupMember)

instance Data.FromJSON GroupMember where
  parseJSON =
    Data.withObject
      "GroupMember"
      ( \x ->
          GroupMember'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "MemberName")
      )

instance Prelude.Hashable GroupMember where
  hashWithSalt _salt GroupMember' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` memberName

instance Prelude.NFData GroupMember where
  rnf GroupMember' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf memberName
