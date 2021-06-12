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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the metadata of the user.
--
-- /See:/ 'newUserMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { -- | The surname of the user.
    surname :: Core.Maybe Core.Text,
    -- | The ID of the user.
    id :: Core.Maybe Core.Text,
    -- | The given name of the user before a rename operation.
    givenName :: Core.Maybe Core.Text,
    -- | The name of the user.
    username :: Core.Maybe Core.Text,
    -- | The email address of the user.
    emailAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { surname = Core.Nothing,
      id = Core.Nothing,
      givenName = Core.Nothing,
      username = Core.Nothing,
      emailAddress = Core.Nothing
    }

-- | The surname of the user.
userMetadata_surname :: Lens.Lens' UserMetadata (Core.Maybe Core.Text)
userMetadata_surname = Lens.lens (\UserMetadata' {surname} -> surname) (\s@UserMetadata' {} a -> s {surname = a} :: UserMetadata)

-- | The ID of the user.
userMetadata_id :: Lens.Lens' UserMetadata (Core.Maybe Core.Text)
userMetadata_id = Lens.lens (\UserMetadata' {id} -> id) (\s@UserMetadata' {} a -> s {id = a} :: UserMetadata)

-- | The given name of the user before a rename operation.
userMetadata_givenName :: Lens.Lens' UserMetadata (Core.Maybe Core.Text)
userMetadata_givenName = Lens.lens (\UserMetadata' {givenName} -> givenName) (\s@UserMetadata' {} a -> s {givenName = a} :: UserMetadata)

-- | The name of the user.
userMetadata_username :: Lens.Lens' UserMetadata (Core.Maybe Core.Text)
userMetadata_username = Lens.lens (\UserMetadata' {username} -> username) (\s@UserMetadata' {} a -> s {username = a} :: UserMetadata)

-- | The email address of the user.
userMetadata_emailAddress :: Lens.Lens' UserMetadata (Core.Maybe Core.Text)
userMetadata_emailAddress = Lens.lens (\UserMetadata' {emailAddress} -> emailAddress) (\s@UserMetadata' {} a -> s {emailAddress = a} :: UserMetadata)

instance Core.FromJSON UserMetadata where
  parseJSON =
    Core.withObject
      "UserMetadata"
      ( \x ->
          UserMetadata'
            Core.<$> (x Core..:? "Surname")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "GivenName")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "EmailAddress")
      )

instance Core.Hashable UserMetadata

instance Core.NFData UserMetadata
