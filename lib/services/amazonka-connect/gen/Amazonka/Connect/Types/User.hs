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
-- Module      : Amazonka.Connect.Types.User
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.User where

import Amazonka.Connect.Types.UserIdentityInfo
import Amazonka.Connect.Types.UserPhoneConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a user account for an Amazon Connect
-- instance.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The Amazon Resource Name (ARN) of the user account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user account in the directory used for identity
    -- management.
    directoryUserId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the hierarchy group for the user.
    hierarchyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user account.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information about the user identity.
    identityInfo :: Prelude.Maybe UserIdentityInfo,
    -- | Information about the phone configuration for the user.
    phoneConfig :: Prelude.Maybe UserPhoneConfig,
    -- | The identifier of the routing profile for the user.
    routingProfileId :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the security profiles for the user.
    securityProfileIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The user name assigned to the user account.
    username :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) of the user account.
--
-- 'directoryUserId', 'user_directoryUserId' - The identifier of the user account in the directory used for identity
-- management.
--
-- 'hierarchyGroupId', 'user_hierarchyGroupId' - The identifier of the hierarchy group for the user.
--
-- 'id', 'user_id' - The identifier of the user account.
--
-- 'identityInfo', 'user_identityInfo' - Information about the user identity.
--
-- 'phoneConfig', 'user_phoneConfig' - Information about the phone configuration for the user.
--
-- 'routingProfileId', 'user_routingProfileId' - The identifier of the routing profile for the user.
--
-- 'securityProfileIds', 'user_securityProfileIds' - The identifiers of the security profiles for the user.
--
-- 'tags', 'user_tags' - The tags.
--
-- 'username', 'user_username' - The user name assigned to the user account.
newUser ::
  User
newUser =
  User'
    { arn = Prelude.Nothing,
      directoryUserId = Prelude.Nothing,
      hierarchyGroupId = Prelude.Nothing,
      id = Prelude.Nothing,
      identityInfo = Prelude.Nothing,
      phoneConfig = Prelude.Nothing,
      routingProfileId = Prelude.Nothing,
      securityProfileIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user account.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | The identifier of the user account in the directory used for identity
-- management.
user_directoryUserId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_directoryUserId = Lens.lens (\User' {directoryUserId} -> directoryUserId) (\s@User' {} a -> s {directoryUserId = a} :: User)

-- | The identifier of the hierarchy group for the user.
user_hierarchyGroupId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_hierarchyGroupId = Lens.lens (\User' {hierarchyGroupId} -> hierarchyGroupId) (\s@User' {} a -> s {hierarchyGroupId = a} :: User)

-- | The identifier of the user account.
user_id :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | Information about the user identity.
user_identityInfo :: Lens.Lens' User (Prelude.Maybe UserIdentityInfo)
user_identityInfo = Lens.lens (\User' {identityInfo} -> identityInfo) (\s@User' {} a -> s {identityInfo = a} :: User)

-- | Information about the phone configuration for the user.
user_phoneConfig :: Lens.Lens' User (Prelude.Maybe UserPhoneConfig)
user_phoneConfig = Lens.lens (\User' {phoneConfig} -> phoneConfig) (\s@User' {} a -> s {phoneConfig = a} :: User)

-- | The identifier of the routing profile for the user.
user_routingProfileId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_routingProfileId = Lens.lens (\User' {routingProfileId} -> routingProfileId) (\s@User' {} a -> s {routingProfileId = a} :: User)

-- | The identifiers of the security profiles for the user.
user_securityProfileIds :: Lens.Lens' User (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
user_securityProfileIds = Lens.lens (\User' {securityProfileIds} -> securityProfileIds) (\s@User' {} a -> s {securityProfileIds = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | The tags.
user_tags :: Lens.Lens' User (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
user_tags = Lens.lens (\User' {tags} -> tags) (\s@User' {} a -> s {tags = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | The user name assigned to the user account.
user_username :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "DirectoryUserId")
            Prelude.<*> (x Data..:? "HierarchyGroupId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "IdentityInfo")
            Prelude.<*> (x Data..:? "PhoneConfig")
            Prelude.<*> (x Data..:? "RoutingProfileId")
            Prelude.<*> (x Data..:? "SecurityProfileIds")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` directoryUserId
      `Prelude.hashWithSalt` hierarchyGroupId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` identityInfo
      `Prelude.hashWithSalt` phoneConfig
      `Prelude.hashWithSalt` routingProfileId
      `Prelude.hashWithSalt` securityProfileIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` username

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf directoryUserId
      `Prelude.seq` Prelude.rnf hierarchyGroupId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf identityInfo
      `Prelude.seq` Prelude.rnf phoneConfig
      `Prelude.seq` Prelude.rnf routingProfileId
      `Prelude.seq` Prelude.rnf securityProfileIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf username
