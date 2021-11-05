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
-- Module      : Amazonka.MemoryDb.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types.Authentication
import qualified Amazonka.Prelude as Prelude

-- | You create users and assign them specific permissions by using an access
-- string. You assign the users to Access Control Lists aligned with a
-- specific role (administrators, human resources) that are then deployed
-- to one or more MemoryDB clusters.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | Indicates the user status. Can be \"active\", \"modifying\" or
    -- \"deleting\".
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Denotes whether the user requires a password to authenticate.
    authentication :: Prelude.Maybe Authentication,
    -- | Access permissions string used for this user.
    accessString :: Prelude.Maybe Prelude.Text,
    -- | The name of the user
    name :: Prelude.Maybe Prelude.Text,
    -- | The names of the Access Control Lists to which the user belongs
    aCLNames :: Prelude.Maybe [Prelude.Text],
    -- | The minimum engine version supported for the user
    minimumEngineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'user_status' - Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) of the user.
--
-- 'authentication', 'user_authentication' - Denotes whether the user requires a password to authenticate.
--
-- 'accessString', 'user_accessString' - Access permissions string used for this user.
--
-- 'name', 'user_name' - The name of the user
--
-- 'aCLNames', 'user_aCLNames' - The names of the Access Control Lists to which the user belongs
--
-- 'minimumEngineVersion', 'user_minimumEngineVersion' - The minimum engine version supported for the user
newUser ::
  User
newUser =
  User'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      authentication = Prelude.Nothing,
      accessString = Prelude.Nothing,
      name = Prelude.Nothing,
      aCLNames = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing
    }

-- | Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
user_status :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The Amazon Resource Name (ARN) of the user.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | Denotes whether the user requires a password to authenticate.
user_authentication :: Lens.Lens' User (Prelude.Maybe Authentication)
user_authentication = Lens.lens (\User' {authentication} -> authentication) (\s@User' {} a -> s {authentication = a} :: User)

-- | Access permissions string used for this user.
user_accessString :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_accessString = Lens.lens (\User' {accessString} -> accessString) (\s@User' {} a -> s {accessString = a} :: User)

-- | The name of the user
user_name :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_name = Lens.lens (\User' {name} -> name) (\s@User' {} a -> s {name = a} :: User)

-- | The names of the Access Control Lists to which the user belongs
user_aCLNames :: Lens.Lens' User (Prelude.Maybe [Prelude.Text])
user_aCLNames = Lens.lens (\User' {aCLNames} -> aCLNames) (\s@User' {} a -> s {aCLNames = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | The minimum engine version supported for the user
user_minimumEngineVersion :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_minimumEngineVersion = Lens.lens (\User' {minimumEngineVersion} -> minimumEngineVersion) (\s@User' {} a -> s {minimumEngineVersion = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "Authentication")
            Prelude.<*> (x Core..:? "AccessString")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ACLNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MinimumEngineVersion")
      )

instance Prelude.Hashable User

instance Prelude.NFData User
