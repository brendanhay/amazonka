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
-- Module      : Amazonka.WorkDocs.Types.UserMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.UserMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the metadata of the user.
--
-- /See:/ 'newUserMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { -- | The email address of the user.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The given name of the user before a rename operation.
    givenName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The surname of the user.
    surname :: Prelude.Maybe Prelude.Text,
    -- | The name of the user.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'userMetadata_emailAddress' - The email address of the user.
--
-- 'givenName', 'userMetadata_givenName' - The given name of the user before a rename operation.
--
-- 'id', 'userMetadata_id' - The ID of the user.
--
-- 'surname', 'userMetadata_surname' - The surname of the user.
--
-- 'username', 'userMetadata_username' - The name of the user.
newUserMetadata ::
  UserMetadata
newUserMetadata =
  UserMetadata'
    { emailAddress = Prelude.Nothing,
      givenName = Prelude.Nothing,
      id = Prelude.Nothing,
      surname = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The email address of the user.
userMetadata_emailAddress :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_emailAddress = Lens.lens (\UserMetadata' {emailAddress} -> emailAddress) (\s@UserMetadata' {} a -> s {emailAddress = a} :: UserMetadata) Prelude.. Lens.mapping Data._Sensitive

-- | The given name of the user before a rename operation.
userMetadata_givenName :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_givenName = Lens.lens (\UserMetadata' {givenName} -> givenName) (\s@UserMetadata' {} a -> s {givenName = a} :: UserMetadata)

-- | The ID of the user.
userMetadata_id :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_id = Lens.lens (\UserMetadata' {id} -> id) (\s@UserMetadata' {} a -> s {id = a} :: UserMetadata)

-- | The surname of the user.
userMetadata_surname :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_surname = Lens.lens (\UserMetadata' {surname} -> surname) (\s@UserMetadata' {} a -> s {surname = a} :: UserMetadata)

-- | The name of the user.
userMetadata_username :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_username = Lens.lens (\UserMetadata' {username} -> username) (\s@UserMetadata' {} a -> s {username = a} :: UserMetadata)

instance Data.FromJSON UserMetadata where
  parseJSON =
    Data.withObject
      "UserMetadata"
      ( \x ->
          UserMetadata'
            Prelude.<$> (x Data..:? "EmailAddress")
            Prelude.<*> (x Data..:? "GivenName")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Surname")
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable UserMetadata where
  hashWithSalt _salt UserMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` givenName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` surname
      `Prelude.hashWithSalt` username

instance Prelude.NFData UserMetadata where
  rnf UserMetadata' {..} =
    Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf givenName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf surname
      `Prelude.seq` Prelude.rnf username
