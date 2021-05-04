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
-- Module      : Network.AWS.MQ.Types.UserPendingChanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.UserPendingChanges where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.ChangeType
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about the status of the changes pending for the
-- ActiveMQ user.
--
-- /See:/ 'newUserPendingChanges' smart constructor.
data UserPendingChanges = UserPendingChanges'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Required. The type of change pending for the ActiveMQ user.
    pendingChange :: Prelude.Maybe ChangeType,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserPendingChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'userPendingChanges_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'pendingChange', 'userPendingChanges_pendingChange' - Required. The type of change pending for the ActiveMQ user.
--
-- 'consoleAccess', 'userPendingChanges_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
newUserPendingChanges ::
  UserPendingChanges
newUserPendingChanges =
  UserPendingChanges'
    { groups = Prelude.Nothing,
      pendingChange = Prelude.Nothing,
      consoleAccess = Prelude.Nothing
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
userPendingChanges_groups :: Lens.Lens' UserPendingChanges (Prelude.Maybe [Prelude.Text])
userPendingChanges_groups = Lens.lens (\UserPendingChanges' {groups} -> groups) (\s@UserPendingChanges' {} a -> s {groups = a} :: UserPendingChanges) Prelude.. Lens.mapping Prelude._Coerce

-- | Required. The type of change pending for the ActiveMQ user.
userPendingChanges_pendingChange :: Lens.Lens' UserPendingChanges (Prelude.Maybe ChangeType)
userPendingChanges_pendingChange = Lens.lens (\UserPendingChanges' {pendingChange} -> pendingChange) (\s@UserPendingChanges' {} a -> s {pendingChange = a} :: UserPendingChanges)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
userPendingChanges_consoleAccess :: Lens.Lens' UserPendingChanges (Prelude.Maybe Prelude.Bool)
userPendingChanges_consoleAccess = Lens.lens (\UserPendingChanges' {consoleAccess} -> consoleAccess) (\s@UserPendingChanges' {} a -> s {consoleAccess = a} :: UserPendingChanges)

instance Prelude.FromJSON UserPendingChanges where
  parseJSON =
    Prelude.withObject
      "UserPendingChanges"
      ( \x ->
          UserPendingChanges'
            Prelude.<$> (x Prelude..:? "groups" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "pendingChange")
            Prelude.<*> (x Prelude..:? "consoleAccess")
      )

instance Prelude.Hashable UserPendingChanges

instance Prelude.NFData UserPendingChanges
