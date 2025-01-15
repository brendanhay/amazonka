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
-- Module      : Amazonka.Connect.Types.UserSearchSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserSearchSummary where

import Amazonka.Connect.Types.UserIdentityInfoLite
import Amazonka.Connect.Types.UserPhoneConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the returned users.
--
-- /See:/ 'newUserSearchSummary' smart constructor.
data UserSearchSummary = UserSearchSummary'
  { -- | The Amazon Resource Name (ARN) of the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The directory identifier of the user.
    directoryUserId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user\'s hierarchy group.
    hierarchyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user\'s summary.
    id :: Prelude.Maybe Prelude.Text,
    -- | The user\'s first name and last name.
    identityInfo :: Prelude.Maybe UserIdentityInfoLite,
    phoneConfig :: Prelude.Maybe UserPhoneConfig,
    -- | The identifier of the user\'s routing profile.
    routingProfileId :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the user\'s security profiles.
    securityProfileIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the user.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserSearchSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'userSearchSummary_arn' - The Amazon Resource Name (ARN) of the user.
--
-- 'directoryUserId', 'userSearchSummary_directoryUserId' - The directory identifier of the user.
--
-- 'hierarchyGroupId', 'userSearchSummary_hierarchyGroupId' - The identifier of the user\'s hierarchy group.
--
-- 'id', 'userSearchSummary_id' - The identifier of the user\'s summary.
--
-- 'identityInfo', 'userSearchSummary_identityInfo' - The user\'s first name and last name.
--
-- 'phoneConfig', 'userSearchSummary_phoneConfig' - Undocumented member.
--
-- 'routingProfileId', 'userSearchSummary_routingProfileId' - The identifier of the user\'s routing profile.
--
-- 'securityProfileIds', 'userSearchSummary_securityProfileIds' - The identifiers of the user\'s security profiles.
--
-- 'tags', 'userSearchSummary_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'username', 'userSearchSummary_username' - The name of the user.
newUserSearchSummary ::
  UserSearchSummary
newUserSearchSummary =
  UserSearchSummary'
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

-- | The Amazon Resource Name (ARN) of the user.
userSearchSummary_arn :: Lens.Lens' UserSearchSummary (Prelude.Maybe Prelude.Text)
userSearchSummary_arn = Lens.lens (\UserSearchSummary' {arn} -> arn) (\s@UserSearchSummary' {} a -> s {arn = a} :: UserSearchSummary)

-- | The directory identifier of the user.
userSearchSummary_directoryUserId :: Lens.Lens' UserSearchSummary (Prelude.Maybe Prelude.Text)
userSearchSummary_directoryUserId = Lens.lens (\UserSearchSummary' {directoryUserId} -> directoryUserId) (\s@UserSearchSummary' {} a -> s {directoryUserId = a} :: UserSearchSummary)

-- | The identifier of the user\'s hierarchy group.
userSearchSummary_hierarchyGroupId :: Lens.Lens' UserSearchSummary (Prelude.Maybe Prelude.Text)
userSearchSummary_hierarchyGroupId = Lens.lens (\UserSearchSummary' {hierarchyGroupId} -> hierarchyGroupId) (\s@UserSearchSummary' {} a -> s {hierarchyGroupId = a} :: UserSearchSummary)

-- | The identifier of the user\'s summary.
userSearchSummary_id :: Lens.Lens' UserSearchSummary (Prelude.Maybe Prelude.Text)
userSearchSummary_id = Lens.lens (\UserSearchSummary' {id} -> id) (\s@UserSearchSummary' {} a -> s {id = a} :: UserSearchSummary)

-- | The user\'s first name and last name.
userSearchSummary_identityInfo :: Lens.Lens' UserSearchSummary (Prelude.Maybe UserIdentityInfoLite)
userSearchSummary_identityInfo = Lens.lens (\UserSearchSummary' {identityInfo} -> identityInfo) (\s@UserSearchSummary' {} a -> s {identityInfo = a} :: UserSearchSummary)

-- | Undocumented member.
userSearchSummary_phoneConfig :: Lens.Lens' UserSearchSummary (Prelude.Maybe UserPhoneConfig)
userSearchSummary_phoneConfig = Lens.lens (\UserSearchSummary' {phoneConfig} -> phoneConfig) (\s@UserSearchSummary' {} a -> s {phoneConfig = a} :: UserSearchSummary)

-- | The identifier of the user\'s routing profile.
userSearchSummary_routingProfileId :: Lens.Lens' UserSearchSummary (Prelude.Maybe Prelude.Text)
userSearchSummary_routingProfileId = Lens.lens (\UserSearchSummary' {routingProfileId} -> routingProfileId) (\s@UserSearchSummary' {} a -> s {routingProfileId = a} :: UserSearchSummary)

-- | The identifiers of the user\'s security profiles.
userSearchSummary_securityProfileIds :: Lens.Lens' UserSearchSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userSearchSummary_securityProfileIds = Lens.lens (\UserSearchSummary' {securityProfileIds} -> securityProfileIds) (\s@UserSearchSummary' {} a -> s {securityProfileIds = a} :: UserSearchSummary) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
userSearchSummary_tags :: Lens.Lens' UserSearchSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
userSearchSummary_tags = Lens.lens (\UserSearchSummary' {tags} -> tags) (\s@UserSearchSummary' {} a -> s {tags = a} :: UserSearchSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the user.
userSearchSummary_username :: Lens.Lens' UserSearchSummary (Prelude.Maybe Prelude.Text)
userSearchSummary_username = Lens.lens (\UserSearchSummary' {username} -> username) (\s@UserSearchSummary' {} a -> s {username = a} :: UserSearchSummary)

instance Data.FromJSON UserSearchSummary where
  parseJSON =
    Data.withObject
      "UserSearchSummary"
      ( \x ->
          UserSearchSummary'
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

instance Prelude.Hashable UserSearchSummary where
  hashWithSalt _salt UserSearchSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` directoryUserId
      `Prelude.hashWithSalt` hierarchyGroupId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` identityInfo
      `Prelude.hashWithSalt` phoneConfig
      `Prelude.hashWithSalt` routingProfileId
      `Prelude.hashWithSalt` securityProfileIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` username

instance Prelude.NFData UserSearchSummary where
  rnf UserSearchSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf directoryUserId `Prelude.seq`
        Prelude.rnf hierarchyGroupId `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf identityInfo `Prelude.seq`
              Prelude.rnf phoneConfig `Prelude.seq`
                Prelude.rnf routingProfileId `Prelude.seq`
                  Prelude.rnf securityProfileIds `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf username
