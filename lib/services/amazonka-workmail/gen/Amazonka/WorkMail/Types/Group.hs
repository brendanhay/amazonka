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
-- Module      : Amazonka.WorkMail.Types.Group
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.EntityState

-- | The representation of an WorkMail group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The email of the group.
    email :: Prelude.Maybe Prelude.Text,
    -- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The identifier of the group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the group was enabled for WorkMail use.
    enabledDate :: Prelude.Maybe Data.POSIX,
    -- | The date indicating when the group was disabled from WorkMail use.
    disabledDate :: Prelude.Maybe Data.POSIX
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
-- 'name', 'group_name' - The name of the group.
--
-- 'email', 'group_email' - The email of the group.
--
-- 'state', 'group_state' - The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- 'id', 'group_id' - The identifier of the group.
--
-- 'enabledDate', 'group_enabledDate' - The date indicating when the group was enabled for WorkMail use.
--
-- 'disabledDate', 'group_disabledDate' - The date indicating when the group was disabled from WorkMail use.
newGroup ::
  Group
newGroup =
  Group'
    { name = Prelude.Nothing,
      email = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      disabledDate = Prelude.Nothing
    }

-- | The name of the group.
group_name :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_name = Lens.lens (\Group' {name} -> name) (\s@Group' {} a -> s {name = a} :: Group)

-- | The email of the group.
group_email :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_email = Lens.lens (\Group' {email} -> email) (\s@Group' {} a -> s {email = a} :: Group)

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
group_state :: Lens.Lens' Group (Prelude.Maybe EntityState)
group_state = Lens.lens (\Group' {state} -> state) (\s@Group' {} a -> s {state = a} :: Group)

-- | The identifier of the group.
group_id :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_id = Lens.lens (\Group' {id} -> id) (\s@Group' {} a -> s {id = a} :: Group)

-- | The date indicating when the group was enabled for WorkMail use.
group_enabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_enabledDate = Lens.lens (\Group' {enabledDate} -> enabledDate) (\s@Group' {} a -> s {enabledDate = a} :: Group) Prelude.. Lens.mapping Data._Time

-- | The date indicating when the group was disabled from WorkMail use.
group_disabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_disabledDate = Lens.lens (\Group' {disabledDate} -> disabledDate) (\s@Group' {} a -> s {disabledDate = a} :: Group) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Group where
  parseJSON =
    Data.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "EnabledDate")
            Prelude.<*> (x Data..:? "DisabledDate")
      )

instance Prelude.Hashable Group where
  hashWithSalt _salt Group' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabledDate
      `Prelude.hashWithSalt` disabledDate

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf disabledDate
