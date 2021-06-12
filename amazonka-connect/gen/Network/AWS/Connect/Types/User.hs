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
-- Module      : Network.AWS.Connect.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.User where

import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a user account for a Amazon Connect instance.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The identifiers of the security profiles for the user.
    securityProfileIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Information about the user identity.
    identityInfo :: Core.Maybe UserIdentityInfo,
    -- | The Amazon Resource Name (ARN) of the user account.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the user account.
    id :: Core.Maybe Core.Text,
    -- | The identifier of the hierarchy group for the user.
    hierarchyGroupId :: Core.Maybe Core.Text,
    -- | The identifier of the user account in the directory used for identity
    -- management.
    directoryUserId :: Core.Maybe Core.Text,
    -- | The identifier of the routing profile for the user.
    routingProfileId :: Core.Maybe Core.Text,
    -- | The tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Information about the phone configuration for the user.
    phoneConfig :: Core.Maybe UserPhoneConfig,
    -- | The user name assigned to the user account.
    username :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileIds', 'user_securityProfileIds' - The identifiers of the security profiles for the user.
--
-- 'identityInfo', 'user_identityInfo' - Information about the user identity.
--
-- 'arn', 'user_arn' - The Amazon Resource Name (ARN) of the user account.
--
-- 'id', 'user_id' - The identifier of the user account.
--
-- 'hierarchyGroupId', 'user_hierarchyGroupId' - The identifier of the hierarchy group for the user.
--
-- 'directoryUserId', 'user_directoryUserId' - The identifier of the user account in the directory used for identity
-- management.
--
-- 'routingProfileId', 'user_routingProfileId' - The identifier of the routing profile for the user.
--
-- 'tags', 'user_tags' - The tags.
--
-- 'phoneConfig', 'user_phoneConfig' - Information about the phone configuration for the user.
--
-- 'username', 'user_username' - The user name assigned to the user account.
newUser ::
  User
newUser =
  User'
    { securityProfileIds = Core.Nothing,
      identityInfo = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      hierarchyGroupId = Core.Nothing,
      directoryUserId = Core.Nothing,
      routingProfileId = Core.Nothing,
      tags = Core.Nothing,
      phoneConfig = Core.Nothing,
      username = Core.Nothing
    }

-- | The identifiers of the security profiles for the user.
user_securityProfileIds :: Lens.Lens' User (Core.Maybe (Core.NonEmpty Core.Text))
user_securityProfileIds = Lens.lens (\User' {securityProfileIds} -> securityProfileIds) (\s@User' {} a -> s {securityProfileIds = a} :: User) Core.. Lens.mapping Lens._Coerce

-- | Information about the user identity.
user_identityInfo :: Lens.Lens' User (Core.Maybe UserIdentityInfo)
user_identityInfo = Lens.lens (\User' {identityInfo} -> identityInfo) (\s@User' {} a -> s {identityInfo = a} :: User)

-- | The Amazon Resource Name (ARN) of the user account.
user_arn :: Lens.Lens' User (Core.Maybe Core.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | The identifier of the user account.
user_id :: Lens.Lens' User (Core.Maybe Core.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The identifier of the hierarchy group for the user.
user_hierarchyGroupId :: Lens.Lens' User (Core.Maybe Core.Text)
user_hierarchyGroupId = Lens.lens (\User' {hierarchyGroupId} -> hierarchyGroupId) (\s@User' {} a -> s {hierarchyGroupId = a} :: User)

-- | The identifier of the user account in the directory used for identity
-- management.
user_directoryUserId :: Lens.Lens' User (Core.Maybe Core.Text)
user_directoryUserId = Lens.lens (\User' {directoryUserId} -> directoryUserId) (\s@User' {} a -> s {directoryUserId = a} :: User)

-- | The identifier of the routing profile for the user.
user_routingProfileId :: Lens.Lens' User (Core.Maybe Core.Text)
user_routingProfileId = Lens.lens (\User' {routingProfileId} -> routingProfileId) (\s@User' {} a -> s {routingProfileId = a} :: User)

-- | The tags.
user_tags :: Lens.Lens' User (Core.Maybe (Core.HashMap Core.Text Core.Text))
user_tags = Lens.lens (\User' {tags} -> tags) (\s@User' {} a -> s {tags = a} :: User) Core.. Lens.mapping Lens._Coerce

-- | Information about the phone configuration for the user.
user_phoneConfig :: Lens.Lens' User (Core.Maybe UserPhoneConfig)
user_phoneConfig = Lens.lens (\User' {phoneConfig} -> phoneConfig) (\s@User' {} a -> s {phoneConfig = a} :: User)

-- | The user name assigned to the user account.
user_username :: Lens.Lens' User (Core.Maybe Core.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Core.<$> (x Core..:? "SecurityProfileIds")
            Core.<*> (x Core..:? "IdentityInfo")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "HierarchyGroupId")
            Core.<*> (x Core..:? "DirectoryUserId")
            Core.<*> (x Core..:? "RoutingProfileId")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "PhoneConfig")
            Core.<*> (x Core..:? "Username")
      )

instance Core.Hashable User

instance Core.NFData User
