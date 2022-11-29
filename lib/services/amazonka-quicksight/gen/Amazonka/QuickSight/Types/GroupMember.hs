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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GroupMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A member of an Amazon QuickSight group. Currently, group members must be
-- users. Groups can\'t be members of another group. .
--
-- /See:/ 'newGroupMember' smart constructor.
data GroupMember = GroupMember'
  { -- | The name of the group member (user).
    memberName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the group member (user).
    arn :: Prelude.Maybe Prelude.Text
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
-- 'memberName', 'groupMember_memberName' - The name of the group member (user).
--
-- 'arn', 'groupMember_arn' - The Amazon Resource Name (ARN) for the group member (user).
newGroupMember ::
  GroupMember
newGroupMember =
  GroupMember'
    { memberName = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The name of the group member (user).
groupMember_memberName :: Lens.Lens' GroupMember (Prelude.Maybe Prelude.Text)
groupMember_memberName = Lens.lens (\GroupMember' {memberName} -> memberName) (\s@GroupMember' {} a -> s {memberName = a} :: GroupMember)

-- | The Amazon Resource Name (ARN) for the group member (user).
groupMember_arn :: Lens.Lens' GroupMember (Prelude.Maybe Prelude.Text)
groupMember_arn = Lens.lens (\GroupMember' {arn} -> arn) (\s@GroupMember' {} a -> s {arn = a} :: GroupMember)

instance Core.FromJSON GroupMember where
  parseJSON =
    Core.withObject
      "GroupMember"
      ( \x ->
          GroupMember'
            Prelude.<$> (x Core..:? "MemberName")
            Prelude.<*> (x Core..:? "Arn")
      )

instance Prelude.Hashable GroupMember where
  hashWithSalt _salt GroupMember' {..} =
    _salt `Prelude.hashWithSalt` memberName
      `Prelude.hashWithSalt` arn

instance Prelude.NFData GroupMember where
  rnf GroupMember' {..} =
    Prelude.rnf memberName
      `Prelude.seq` Prelude.rnf arn
