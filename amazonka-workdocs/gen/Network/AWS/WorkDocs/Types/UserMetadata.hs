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
-- Module      : Network.AWS.WorkDocs.Types.UserMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the metadata of the user.
--
-- /See:/ 'newUserMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { -- | The surname of the user.
    surname :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The given name of the user before a rename operation.
    givenName :: Prelude.Maybe Prelude.Text,
    -- | The name of the user.
    username :: Prelude.Maybe Prelude.Text,
    -- | The email address of the user.
    emailAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'surname', 'userMetadata_surname' - The surname of the user.
--
-- 'id', 'userMetadata_id' - The ID of the user.
--
-- 'givenName', 'userMetadata_givenName' - The given name of the user before a rename operation.
--
-- 'username', 'userMetadata_username' - The name of the user.
--
-- 'emailAddress', 'userMetadata_emailAddress' - The email address of the user.
newUserMetadata ::
  UserMetadata
newUserMetadata =
  UserMetadata'
    { surname = Prelude.Nothing,
      id = Prelude.Nothing,
      givenName = Prelude.Nothing,
      username = Prelude.Nothing,
      emailAddress = Prelude.Nothing
    }

-- | The surname of the user.
userMetadata_surname :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_surname = Lens.lens (\UserMetadata' {surname} -> surname) (\s@UserMetadata' {} a -> s {surname = a} :: UserMetadata)

-- | The ID of the user.
userMetadata_id :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_id = Lens.lens (\UserMetadata' {id} -> id) (\s@UserMetadata' {} a -> s {id = a} :: UserMetadata)

-- | The given name of the user before a rename operation.
userMetadata_givenName :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_givenName = Lens.lens (\UserMetadata' {givenName} -> givenName) (\s@UserMetadata' {} a -> s {givenName = a} :: UserMetadata)

-- | The name of the user.
userMetadata_username :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_username = Lens.lens (\UserMetadata' {username} -> username) (\s@UserMetadata' {} a -> s {username = a} :: UserMetadata)

-- | The email address of the user.
userMetadata_emailAddress :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_emailAddress = Lens.lens (\UserMetadata' {emailAddress} -> emailAddress) (\s@UserMetadata' {} a -> s {emailAddress = a} :: UserMetadata)

instance Prelude.FromJSON UserMetadata where
  parseJSON =
    Prelude.withObject
      "UserMetadata"
      ( \x ->
          UserMetadata'
            Prelude.<$> (x Prelude..:? "Surname")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "GivenName")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "EmailAddress")
      )

instance Prelude.Hashable UserMetadata

instance Prelude.NFData UserMetadata
