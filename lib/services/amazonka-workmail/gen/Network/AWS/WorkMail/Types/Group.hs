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
-- Module      : Network.AWS.WorkMail.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Group where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.EntityState

-- | The representation of an Amazon WorkMail group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The date indicating when the group was enabled for Amazon WorkMail use.
    enabledDate :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The date indicating when the group was disabled from Amazon WorkMail
    -- use.
    disabledDate :: Prelude.Maybe Core.POSIX,
    -- | The email of the group.
    email :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledDate', 'group_enabledDate' - The date indicating when the group was enabled for Amazon WorkMail use.
--
-- 'id', 'group_id' - The identifier of the group.
--
-- 'name', 'group_name' - The name of the group.
--
-- 'state', 'group_state' - The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- 'disabledDate', 'group_disabledDate' - The date indicating when the group was disabled from Amazon WorkMail
-- use.
--
-- 'email', 'group_email' - The email of the group.
newGroup ::
  Group
newGroup =
  Group'
    { enabledDate = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      disabledDate = Prelude.Nothing,
      email = Prelude.Nothing
    }

-- | The date indicating when the group was enabled for Amazon WorkMail use.
group_enabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_enabledDate = Lens.lens (\Group' {enabledDate} -> enabledDate) (\s@Group' {} a -> s {enabledDate = a} :: Group) Prelude.. Lens.mapping Core._Time

-- | The identifier of the group.
group_id :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_id = Lens.lens (\Group' {id} -> id) (\s@Group' {} a -> s {id = a} :: Group)

-- | The name of the group.
group_name :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_name = Lens.lens (\Group' {name} -> name) (\s@Group' {} a -> s {name = a} :: Group)

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
group_state :: Lens.Lens' Group (Prelude.Maybe EntityState)
group_state = Lens.lens (\Group' {state} -> state) (\s@Group' {} a -> s {state = a} :: Group)

-- | The date indicating when the group was disabled from Amazon WorkMail
-- use.
group_disabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_disabledDate = Lens.lens (\Group' {disabledDate} -> disabledDate) (\s@Group' {} a -> s {disabledDate = a} :: Group) Prelude.. Lens.mapping Core._Time

-- | The email of the group.
group_email :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_email = Lens.lens (\Group' {email} -> email) (\s@Group' {} a -> s {email = a} :: Group)

instance Core.FromJSON Group where
  parseJSON =
    Core.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Core..:? "EnabledDate")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "DisabledDate")
            Prelude.<*> (x Core..:? "Email")
      )

instance Prelude.Hashable Group

instance Prelude.NFData Group
