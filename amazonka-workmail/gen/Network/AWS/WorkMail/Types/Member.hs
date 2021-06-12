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
-- Module      : Network.AWS.WorkMail.Types.Member
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Member where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.MemberType

-- | The representation of a user or group.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The date indicating when the member was enabled for Amazon WorkMail use.
    enabledDate :: Core.Maybe Core.POSIX,
    -- | The identifier of the member.
    id :: Core.Maybe Core.Text,
    -- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
    state :: Core.Maybe EntityState,
    -- | The name of the member.
    name :: Core.Maybe Core.Text,
    -- | The date indicating when the member was disabled from Amazon WorkMail
    -- use.
    disabledDate :: Core.Maybe Core.POSIX,
    -- | A member can be a user or group.
    type' :: Core.Maybe MemberType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledDate', 'member_enabledDate' - The date indicating when the member was enabled for Amazon WorkMail use.
--
-- 'id', 'member_id' - The identifier of the member.
--
-- 'state', 'member_state' - The state of the member, which can be ENABLED, DISABLED, or DELETED.
--
-- 'name', 'member_name' - The name of the member.
--
-- 'disabledDate', 'member_disabledDate' - The date indicating when the member was disabled from Amazon WorkMail
-- use.
--
-- 'type'', 'member_type' - A member can be a user or group.
newMember ::
  Member
newMember =
  Member'
    { enabledDate = Core.Nothing,
      id = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      disabledDate = Core.Nothing,
      type' = Core.Nothing
    }

-- | The date indicating when the member was enabled for Amazon WorkMail use.
member_enabledDate :: Lens.Lens' Member (Core.Maybe Core.UTCTime)
member_enabledDate = Lens.lens (\Member' {enabledDate} -> enabledDate) (\s@Member' {} a -> s {enabledDate = a} :: Member) Core.. Lens.mapping Core._Time

-- | The identifier of the member.
member_id :: Lens.Lens' Member (Core.Maybe Core.Text)
member_id = Lens.lens (\Member' {id} -> id) (\s@Member' {} a -> s {id = a} :: Member)

-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
member_state :: Lens.Lens' Member (Core.Maybe EntityState)
member_state = Lens.lens (\Member' {state} -> state) (\s@Member' {} a -> s {state = a} :: Member)

-- | The name of the member.
member_name :: Lens.Lens' Member (Core.Maybe Core.Text)
member_name = Lens.lens (\Member' {name} -> name) (\s@Member' {} a -> s {name = a} :: Member)

-- | The date indicating when the member was disabled from Amazon WorkMail
-- use.
member_disabledDate :: Lens.Lens' Member (Core.Maybe Core.UTCTime)
member_disabledDate = Lens.lens (\Member' {disabledDate} -> disabledDate) (\s@Member' {} a -> s {disabledDate = a} :: Member) Core.. Lens.mapping Core._Time

-- | A member can be a user or group.
member_type :: Lens.Lens' Member (Core.Maybe MemberType)
member_type = Lens.lens (\Member' {type'} -> type') (\s@Member' {} a -> s {type' = a} :: Member)

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject
      "Member"
      ( \x ->
          Member'
            Core.<$> (x Core..:? "EnabledDate")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "DisabledDate")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Member

instance Core.NFData Member
