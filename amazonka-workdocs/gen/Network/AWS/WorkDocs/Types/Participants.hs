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
-- Module      : Network.AWS.WorkDocs.Types.Participants
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Participants where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.GroupMetadata
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the users or user groups.
--
-- /See:/ 'newParticipants' smart constructor.
data Participants = Participants'
  { -- | The list of user groups.
    groups :: Core.Maybe [GroupMetadata],
    -- | The list of users.
    users :: Core.Maybe [UserMetadata]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Participants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'participants_groups' - The list of user groups.
--
-- 'users', 'participants_users' - The list of users.
newParticipants ::
  Participants
newParticipants =
  Participants'
    { groups = Core.Nothing,
      users = Core.Nothing
    }

-- | The list of user groups.
participants_groups :: Lens.Lens' Participants (Core.Maybe [GroupMetadata])
participants_groups = Lens.lens (\Participants' {groups} -> groups) (\s@Participants' {} a -> s {groups = a} :: Participants) Core.. Lens.mapping Lens._Coerce

-- | The list of users.
participants_users :: Lens.Lens' Participants (Core.Maybe [UserMetadata])
participants_users = Lens.lens (\Participants' {users} -> users) (\s@Participants' {} a -> s {users = a} :: Participants) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Participants where
  parseJSON =
    Core.withObject
      "Participants"
      ( \x ->
          Participants'
            Core.<$> (x Core..:? "Groups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Users" Core..!= Core.mempty)
      )

instance Core.Hashable Participants

instance Core.NFData Participants
