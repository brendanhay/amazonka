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
-- Module      : Network.AWS.WorkMail.Types.Member
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Member where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.MemberType

-- | The representation of a user or group.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The date indicating when the member was enabled for Amazon WorkMail use.
    enabledDate :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the member.
    id :: Prelude.Maybe Prelude.Text,
    -- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The name of the member.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the member was disabled from Amazon WorkMail
    -- use.
    disabledDate :: Prelude.Maybe Prelude.POSIX,
    -- | A member can be a user or group.
    type' :: Prelude.Maybe MemberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { enabledDate = Prelude.Nothing,
      id = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      disabledDate = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date indicating when the member was enabled for Amazon WorkMail use.
member_enabledDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_enabledDate = Lens.lens (\Member' {enabledDate} -> enabledDate) (\s@Member' {} a -> s {enabledDate = a} :: Member) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the member.
member_id :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_id = Lens.lens (\Member' {id} -> id) (\s@Member' {} a -> s {id = a} :: Member)

-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
member_state :: Lens.Lens' Member (Prelude.Maybe EntityState)
member_state = Lens.lens (\Member' {state} -> state) (\s@Member' {} a -> s {state = a} :: Member)

-- | The name of the member.
member_name :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_name = Lens.lens (\Member' {name} -> name) (\s@Member' {} a -> s {name = a} :: Member)

-- | The date indicating when the member was disabled from Amazon WorkMail
-- use.
member_disabledDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_disabledDate = Lens.lens (\Member' {disabledDate} -> disabledDate) (\s@Member' {} a -> s {disabledDate = a} :: Member) Prelude.. Lens.mapping Prelude._Time

-- | A member can be a user or group.
member_type :: Lens.Lens' Member (Prelude.Maybe MemberType)
member_type = Lens.lens (\Member' {type'} -> type') (\s@Member' {} a -> s {type' = a} :: Member)

instance Prelude.FromJSON Member where
  parseJSON =
    Prelude.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Prelude..:? "EnabledDate")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "DisabledDate")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Member

instance Prelude.NFData Member
