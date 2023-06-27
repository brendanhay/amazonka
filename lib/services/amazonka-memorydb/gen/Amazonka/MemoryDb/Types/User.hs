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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.Authentication
import qualified Amazonka.Prelude as Prelude

-- | You create users and assign them specific permissions by using an access
-- string. You assign the users to Access Control Lists aligned with a
-- specific role (administrators, human resources) that are then deployed
-- to one or more MemoryDB clusters.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The names of the Access Control Lists to which the user belongs
    aCLNames :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Access permissions string used for this user.
    accessString :: Prelude.Maybe Prelude.Text,
    -- | Denotes whether the user requires a password to authenticate.
    authentication :: Prelude.Maybe Authentication,
    -- | The minimum engine version supported for the user
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the user
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates the user status. Can be \"active\", \"modifying\" or
    -- \"deleting\".
    status :: Prelude.Maybe Prelude.Text
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
-- 'aCLNames', 'user_aCLNames' - The names of the Access Control Lists to which the user belongs
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) of the user.
--
-- 'accessString', 'user_accessString' - Access permissions string used for this user.
--
-- 'authentication', 'user_authentication' - Denotes whether the user requires a password to authenticate.
--
-- 'minimumEngineVersion', 'user_minimumEngineVersion' - The minimum engine version supported for the user
--
-- 'name', 'user_name' - The name of the user
--
-- 'status', 'user_status' - Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
newUser ::
  User
newUser =
  User'
    { aCLNames = Prelude.Nothing,
      arn = Prelude.Nothing,
      accessString = Prelude.Nothing,
      authentication = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The names of the Access Control Lists to which the user belongs
user_aCLNames :: Lens.Lens' User (Prelude.Maybe [Prelude.Text])
user_aCLNames = Lens.lens (\User' {aCLNames} -> aCLNames) (\s@User' {} a -> s {aCLNames = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the user.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | Access permissions string used for this user.
user_accessString :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_accessString = Lens.lens (\User' {accessString} -> accessString) (\s@User' {} a -> s {accessString = a} :: User)

-- | Denotes whether the user requires a password to authenticate.
user_authentication :: Lens.Lens' User (Prelude.Maybe Authentication)
user_authentication = Lens.lens (\User' {authentication} -> authentication) (\s@User' {} a -> s {authentication = a} :: User)

-- | The minimum engine version supported for the user
user_minimumEngineVersion :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_minimumEngineVersion = Lens.lens (\User' {minimumEngineVersion} -> minimumEngineVersion) (\s@User' {} a -> s {minimumEngineVersion = a} :: User)

-- | The name of the user
user_name :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_name = Lens.lens (\User' {name} -> name) (\s@User' {} a -> s {name = a} :: User)

-- | Indicates the user status. Can be \"active\", \"modifying\" or
-- \"deleting\".
user_status :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "ACLNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "AccessString")
            Prelude.<*> (x Data..:? "Authentication")
            Prelude.<*> (x Data..:? "MinimumEngineVersion")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt
      `Prelude.hashWithSalt` aCLNames
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` accessString
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` minimumEngineVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf aCLNames
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf accessString
      `Prelude.seq` Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf minimumEngineVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
