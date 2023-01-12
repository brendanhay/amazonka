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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.Member where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.EntityState
import Amazonka.WorkMail.Types.MemberType

-- | The representation of a user or group.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The date indicating when the member was disabled from WorkMail use.
    disabledDate :: Prelude.Maybe Data.POSIX,
    -- | The date indicating when the member was enabled for WorkMail use.
    enabledDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the member.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the member.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | A member can be a user or group.
    type' :: Prelude.Maybe MemberType
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
-- 'disabledDate', 'member_disabledDate' - The date indicating when the member was disabled from WorkMail use.
--
-- 'enabledDate', 'member_enabledDate' - The date indicating when the member was enabled for WorkMail use.
--
-- 'id', 'member_id' - The identifier of the member.
--
-- 'name', 'member_name' - The name of the member.
--
-- 'state', 'member_state' - The state of the member, which can be ENABLED, DISABLED, or DELETED.
--
-- 'type'', 'member_type' - A member can be a user or group.
newMember ::
  Member
newMember =
  Member'
    { disabledDate = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date indicating when the member was disabled from WorkMail use.
member_disabledDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_disabledDate = Lens.lens (\Member' {disabledDate} -> disabledDate) (\s@Member' {} a -> s {disabledDate = a} :: Member) Prelude.. Lens.mapping Data._Time

-- | The date indicating when the member was enabled for WorkMail use.
member_enabledDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_enabledDate = Lens.lens (\Member' {enabledDate} -> enabledDate) (\s@Member' {} a -> s {enabledDate = a} :: Member) Prelude.. Lens.mapping Data._Time

-- | The identifier of the member.
member_id :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_id = Lens.lens (\Member' {id} -> id) (\s@Member' {} a -> s {id = a} :: Member)

-- | The name of the member.
member_name :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_name = Lens.lens (\Member' {name} -> name) (\s@Member' {} a -> s {name = a} :: Member)

-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
member_state :: Lens.Lens' Member (Prelude.Maybe EntityState)
member_state = Lens.lens (\Member' {state} -> state) (\s@Member' {} a -> s {state = a} :: Member)

-- | A member can be a user or group.
member_type :: Lens.Lens' Member (Prelude.Maybe MemberType)
member_type = Lens.lens (\Member' {type'} -> type') (\s@Member' {} a -> s {type' = a} :: Member)

instance Data.FromJSON Member where
  parseJSON =
    Data.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Data..:? "DisabledDate")
            Prelude.<*> (x Data..:? "EnabledDate")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Member where
  hashWithSalt _salt Member' {..} =
    _salt `Prelude.hashWithSalt` disabledDate
      `Prelude.hashWithSalt` enabledDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf disabledDate
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
