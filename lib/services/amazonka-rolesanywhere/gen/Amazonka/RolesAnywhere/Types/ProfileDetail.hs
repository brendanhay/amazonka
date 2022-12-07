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
-- Module      : Amazonka.RolesAnywhere.Types.ProfileDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.ProfileDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The state of the profile after a read or write operation.
--
-- /See:/ 'newProfileDetail' smart constructor.
data ProfileDetail = ProfileDetail'
  { -- | The name of the profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the profile.
    profileId :: Prelude.Maybe Prelude.Text,
    -- | A list of managed policy ARNs that apply to the vended session
    -- credentials.
    managedPolicyArns :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the profile.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | A list of IAM roles that this profile can assume in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    roleArns :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the profile is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The number of seconds the vended session credentials are valid for.
    durationSeconds :: Prelude.Maybe Prelude.Int,
    -- | A session policy that applies to the trust boundary of the vended
    -- session credentials.
    sessionPolicy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account that created the profile.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether instance properties are required in
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- requests with this profile.
    requireInstanceProperties :: Prelude.Maybe Prelude.Bool,
    -- | The ISO-8601 timestamp when the profile was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ISO-8601 timestamp when the profile was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'profileDetail_name' - The name of the profile.
--
-- 'profileId', 'profileDetail_profileId' - The unique identifier of the profile.
--
-- 'managedPolicyArns', 'profileDetail_managedPolicyArns' - A list of managed policy ARNs that apply to the vended session
-- credentials.
--
-- 'profileArn', 'profileDetail_profileArn' - The ARN of the profile.
--
-- 'roleArns', 'profileDetail_roleArns' - A list of IAM roles that this profile can assume in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
--
-- 'enabled', 'profileDetail_enabled' - Indicates whether the profile is enabled.
--
-- 'durationSeconds', 'profileDetail_durationSeconds' - The number of seconds the vended session credentials are valid for.
--
-- 'sessionPolicy', 'profileDetail_sessionPolicy' - A session policy that applies to the trust boundary of the vended
-- session credentials.
--
-- 'createdBy', 'profileDetail_createdBy' - The Amazon Web Services account that created the profile.
--
-- 'requireInstanceProperties', 'profileDetail_requireInstanceProperties' - Specifies whether instance properties are required in
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- requests with this profile.
--
-- 'createdAt', 'profileDetail_createdAt' - The ISO-8601 timestamp when the profile was created.
--
-- 'updatedAt', 'profileDetail_updatedAt' - The ISO-8601 timestamp when the profile was last updated.
newProfileDetail ::
  ProfileDetail
newProfileDetail =
  ProfileDetail'
    { name = Prelude.Nothing,
      profileId = Prelude.Nothing,
      managedPolicyArns = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      roleArns = Prelude.Nothing,
      enabled = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      sessionPolicy = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      requireInstanceProperties = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The name of the profile.
profileDetail_name :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Text)
profileDetail_name = Lens.lens (\ProfileDetail' {name} -> name) (\s@ProfileDetail' {} a -> s {name = a} :: ProfileDetail)

-- | The unique identifier of the profile.
profileDetail_profileId :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Text)
profileDetail_profileId = Lens.lens (\ProfileDetail' {profileId} -> profileId) (\s@ProfileDetail' {} a -> s {profileId = a} :: ProfileDetail)

-- | A list of managed policy ARNs that apply to the vended session
-- credentials.
profileDetail_managedPolicyArns :: Lens.Lens' ProfileDetail (Prelude.Maybe [Prelude.Text])
profileDetail_managedPolicyArns = Lens.lens (\ProfileDetail' {managedPolicyArns} -> managedPolicyArns) (\s@ProfileDetail' {} a -> s {managedPolicyArns = a} :: ProfileDetail) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the profile.
profileDetail_profileArn :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Text)
profileDetail_profileArn = Lens.lens (\ProfileDetail' {profileArn} -> profileArn) (\s@ProfileDetail' {} a -> s {profileArn = a} :: ProfileDetail)

-- | A list of IAM roles that this profile can assume in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
profileDetail_roleArns :: Lens.Lens' ProfileDetail (Prelude.Maybe [Prelude.Text])
profileDetail_roleArns = Lens.lens (\ProfileDetail' {roleArns} -> roleArns) (\s@ProfileDetail' {} a -> s {roleArns = a} :: ProfileDetail) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the profile is enabled.
profileDetail_enabled :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Bool)
profileDetail_enabled = Lens.lens (\ProfileDetail' {enabled} -> enabled) (\s@ProfileDetail' {} a -> s {enabled = a} :: ProfileDetail)

-- | The number of seconds the vended session credentials are valid for.
profileDetail_durationSeconds :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Int)
profileDetail_durationSeconds = Lens.lens (\ProfileDetail' {durationSeconds} -> durationSeconds) (\s@ProfileDetail' {} a -> s {durationSeconds = a} :: ProfileDetail)

-- | A session policy that applies to the trust boundary of the vended
-- session credentials.
profileDetail_sessionPolicy :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Text)
profileDetail_sessionPolicy = Lens.lens (\ProfileDetail' {sessionPolicy} -> sessionPolicy) (\s@ProfileDetail' {} a -> s {sessionPolicy = a} :: ProfileDetail)

-- | The Amazon Web Services account that created the profile.
profileDetail_createdBy :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Text)
profileDetail_createdBy = Lens.lens (\ProfileDetail' {createdBy} -> createdBy) (\s@ProfileDetail' {} a -> s {createdBy = a} :: ProfileDetail)

-- | Specifies whether instance properties are required in
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- requests with this profile.
profileDetail_requireInstanceProperties :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.Bool)
profileDetail_requireInstanceProperties = Lens.lens (\ProfileDetail' {requireInstanceProperties} -> requireInstanceProperties) (\s@ProfileDetail' {} a -> s {requireInstanceProperties = a} :: ProfileDetail)

-- | The ISO-8601 timestamp when the profile was created.
profileDetail_createdAt :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.UTCTime)
profileDetail_createdAt = Lens.lens (\ProfileDetail' {createdAt} -> createdAt) (\s@ProfileDetail' {} a -> s {createdAt = a} :: ProfileDetail) Prelude.. Lens.mapping Data._Time

-- | The ISO-8601 timestamp when the profile was last updated.
profileDetail_updatedAt :: Lens.Lens' ProfileDetail (Prelude.Maybe Prelude.UTCTime)
profileDetail_updatedAt = Lens.lens (\ProfileDetail' {updatedAt} -> updatedAt) (\s@ProfileDetail' {} a -> s {updatedAt = a} :: ProfileDetail) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ProfileDetail where
  parseJSON =
    Data.withObject
      "ProfileDetail"
      ( \x ->
          ProfileDetail'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "profileId")
            Prelude.<*> ( x Data..:? "managedPolicyArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "profileArn")
            Prelude.<*> (x Data..:? "roleArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "durationSeconds")
            Prelude.<*> (x Data..:? "sessionPolicy")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "requireInstanceProperties")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable ProfileDetail where
  hashWithSalt _salt ProfileDetail' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` managedPolicyArns
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` roleArns
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` sessionPolicy
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` requireInstanceProperties
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ProfileDetail where
  rnf ProfileDetail' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf profileId
      `Prelude.seq` Prelude.rnf managedPolicyArns
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf roleArns
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf sessionPolicy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf requireInstanceProperties
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
