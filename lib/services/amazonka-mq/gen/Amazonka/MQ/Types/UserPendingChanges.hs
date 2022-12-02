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
-- Module      : Amazonka.MQ.Types.UserPendingChanges
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.UserPendingChanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.ChangeType
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the status of the changes pending for the
-- ActiveMQ user.
--
-- /See:/ 'newUserPendingChanges' smart constructor.
data UserPendingChanges = UserPendingChanges'
  { -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Required. The type of change pending for the ActiveMQ user.
    pendingChange :: ChangeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPendingChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consoleAccess', 'userPendingChanges_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'groups', 'userPendingChanges_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'pendingChange', 'userPendingChanges_pendingChange' - Required. The type of change pending for the ActiveMQ user.
newUserPendingChanges ::
  -- | 'pendingChange'
  ChangeType ->
  UserPendingChanges
newUserPendingChanges pPendingChange_ =
  UserPendingChanges'
    { consoleAccess =
        Prelude.Nothing,
      groups = Prelude.Nothing,
      pendingChange = pPendingChange_
    }

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
userPendingChanges_consoleAccess :: Lens.Lens' UserPendingChanges (Prelude.Maybe Prelude.Bool)
userPendingChanges_consoleAccess = Lens.lens (\UserPendingChanges' {consoleAccess} -> consoleAccess) (\s@UserPendingChanges' {} a -> s {consoleAccess = a} :: UserPendingChanges)

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
userPendingChanges_groups :: Lens.Lens' UserPendingChanges (Prelude.Maybe [Prelude.Text])
userPendingChanges_groups = Lens.lens (\UserPendingChanges' {groups} -> groups) (\s@UserPendingChanges' {} a -> s {groups = a} :: UserPendingChanges) Prelude.. Lens.mapping Lens.coerced

-- | Required. The type of change pending for the ActiveMQ user.
userPendingChanges_pendingChange :: Lens.Lens' UserPendingChanges ChangeType
userPendingChanges_pendingChange = Lens.lens (\UserPendingChanges' {pendingChange} -> pendingChange) (\s@UserPendingChanges' {} a -> s {pendingChange = a} :: UserPendingChanges)

instance Data.FromJSON UserPendingChanges where
  parseJSON =
    Data.withObject
      "UserPendingChanges"
      ( \x ->
          UserPendingChanges'
            Prelude.<$> (x Data..:? "consoleAccess")
            Prelude.<*> (x Data..:? "groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "pendingChange")
      )

instance Prelude.Hashable UserPendingChanges where
  hashWithSalt _salt UserPendingChanges' {..} =
    _salt `Prelude.hashWithSalt` consoleAccess
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` pendingChange

instance Prelude.NFData UserPendingChanges where
  rnf UserPendingChanges' {..} =
    Prelude.rnf consoleAccess
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf pendingChange
