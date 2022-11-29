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
-- Module      : Amazonka.WorkMail.Types.Member
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.Member where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.EntityState
import Amazonka.WorkMail.Types.MemberType

-- | The representation of a user or group.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The name of the member.
    name :: Prelude.Maybe Prelude.Text,
    -- | A member can be a user or group.
    type' :: Prelude.Maybe MemberType,
    -- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The identifier of the member.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the member was enabled for WorkMail use.
    enabledDate :: Prelude.Maybe Core.POSIX,
    -- | The date indicating when the member was disabled from WorkMail use.
    disabledDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'member_name' - The name of the member.
--
-- 'type'', 'member_type' - A member can be a user or group.
--
-- 'state', 'member_state' - The state of the member, which can be ENABLED, DISABLED, or DELETED.
--
-- 'id', 'member_id' - The identifier of the member.
--
-- 'enabledDate', 'member_enabledDate' - The date indicating when the member was enabled for WorkMail use.
--
-- 'disabledDate', 'member_disabledDate' - The date indicating when the member was disabled from WorkMail use.
newMember ::
  Member
newMember =
  Member'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      disabledDate = Prelude.Nothing
    }

-- | The name of the member.
member_name :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_name = Lens.lens (\Member' {name} -> name) (\s@Member' {} a -> s {name = a} :: Member)

-- | A member can be a user or group.
member_type :: Lens.Lens' Member (Prelude.Maybe MemberType)
member_type = Lens.lens (\Member' {type'} -> type') (\s@Member' {} a -> s {type' = a} :: Member)

-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
member_state :: Lens.Lens' Member (Prelude.Maybe EntityState)
member_state = Lens.lens (\Member' {state} -> state) (\s@Member' {} a -> s {state = a} :: Member)

-- | The identifier of the member.
member_id :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_id = Lens.lens (\Member' {id} -> id) (\s@Member' {} a -> s {id = a} :: Member)

-- | The date indicating when the member was enabled for WorkMail use.
member_enabledDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_enabledDate = Lens.lens (\Member' {enabledDate} -> enabledDate) (\s@Member' {} a -> s {enabledDate = a} :: Member) Prelude.. Lens.mapping Core._Time

-- | The date indicating when the member was disabled from WorkMail use.
member_disabledDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_disabledDate = Lens.lens (\Member' {disabledDate} -> disabledDate) (\s@Member' {} a -> s {disabledDate = a} :: Member) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EnabledDate")
            Prelude.<*> (x Core..:? "DisabledDate")
      )

instance Prelude.Hashable Member where
  hashWithSalt _salt Member' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabledDate
      `Prelude.hashWithSalt` disabledDate

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf disabledDate
