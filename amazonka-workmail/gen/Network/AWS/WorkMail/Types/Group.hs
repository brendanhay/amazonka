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
-- Module      : Network.AWS.WorkMail.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Group where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.EntityState

-- | The representation of an Amazon WorkMail group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The date indicating when the group was enabled for Amazon WorkMail use.
    enabledDate :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The email of the group.
    email :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the group was disabled from Amazon WorkMail
    -- use.
    disabledDate :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'state', 'group_state' - The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- 'name', 'group_name' - The name of the group.
--
-- 'email', 'group_email' - The email of the group.
--
-- 'disabledDate', 'group_disabledDate' - The date indicating when the group was disabled from Amazon WorkMail
-- use.
newGroup ::
  Group
newGroup =
  Group'
    { enabledDate = Prelude.Nothing,
      id = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      email = Prelude.Nothing,
      disabledDate = Prelude.Nothing
    }

-- | The date indicating when the group was enabled for Amazon WorkMail use.
group_enabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_enabledDate = Lens.lens (\Group' {enabledDate} -> enabledDate) (\s@Group' {} a -> s {enabledDate = a} :: Group) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the group.
group_id :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_id = Lens.lens (\Group' {id} -> id) (\s@Group' {} a -> s {id = a} :: Group)

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
group_state :: Lens.Lens' Group (Prelude.Maybe EntityState)
group_state = Lens.lens (\Group' {state} -> state) (\s@Group' {} a -> s {state = a} :: Group)

-- | The name of the group.
group_name :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_name = Lens.lens (\Group' {name} -> name) (\s@Group' {} a -> s {name = a} :: Group)

-- | The email of the group.
group_email :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_email = Lens.lens (\Group' {email} -> email) (\s@Group' {} a -> s {email = a} :: Group)

-- | The date indicating when the group was disabled from Amazon WorkMail
-- use.
group_disabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_disabledDate = Lens.lens (\Group' {disabledDate} -> disabledDate) (\s@Group' {} a -> s {disabledDate = a} :: Group) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON Group where
  parseJSON =
    Prelude.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Prelude..:? "EnabledDate")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Email")
            Prelude.<*> (x Prelude..:? "DisabledDate")
      )

instance Prelude.Hashable Group

instance Prelude.NFData Group
