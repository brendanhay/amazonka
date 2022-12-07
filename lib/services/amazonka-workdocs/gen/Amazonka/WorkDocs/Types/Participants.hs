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
-- Module      : Amazonka.WorkDocs.Types.Participants
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.Participants where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.GroupMetadata
import Amazonka.WorkDocs.Types.UserMetadata

-- | Describes the users or user groups.
--
-- /See:/ 'newParticipants' smart constructor.
data Participants = Participants'
  { -- | The list of users.
    users :: Prelude.Maybe [UserMetadata],
    -- | The list of user groups.
    groups :: Prelude.Maybe [GroupMetadata]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Participants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'users', 'participants_users' - The list of users.
--
-- 'groups', 'participants_groups' - The list of user groups.
newParticipants ::
  Participants
newParticipants =
  Participants'
    { users = Prelude.Nothing,
      groups = Prelude.Nothing
    }

-- | The list of users.
participants_users :: Lens.Lens' Participants (Prelude.Maybe [UserMetadata])
participants_users = Lens.lens (\Participants' {users} -> users) (\s@Participants' {} a -> s {users = a} :: Participants) Prelude.. Lens.mapping Lens.coerced

-- | The list of user groups.
participants_groups :: Lens.Lens' Participants (Prelude.Maybe [GroupMetadata])
participants_groups = Lens.lens (\Participants' {groups} -> groups) (\s@Participants' {} a -> s {groups = a} :: Participants) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Participants where
  parseJSON =
    Data.withObject
      "Participants"
      ( \x ->
          Participants'
            Prelude.<$> (x Data..:? "Users" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Groups" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Participants where
  hashWithSalt _salt Participants' {..} =
    _salt `Prelude.hashWithSalt` users
      `Prelude.hashWithSalt` groups

instance Prelude.NFData Participants where
  rnf Participants' {..} =
    Prelude.rnf users `Prelude.seq` Prelude.rnf groups
