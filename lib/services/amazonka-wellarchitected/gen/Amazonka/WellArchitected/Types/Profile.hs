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
-- Module      : Amazonka.WellArchitected.Types.Profile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Profile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ProfileQuestion

-- | A profile.
--
-- /See:/ 'newProfile' smart constructor.
data Profile = Profile'
  { createdAt :: Prelude.Maybe Data.POSIX,
    owner :: Prelude.Maybe Prelude.Text,
    -- | The profile ARN.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The profile description.
    profileDescription :: Prelude.Maybe Prelude.Text,
    -- | The profile name.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | Profile questions.
    profileQuestions :: Prelude.Maybe [ProfileQuestion],
    -- | The profile version.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the profile.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Profile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'profile_createdAt' - Undocumented member.
--
-- 'owner', 'profile_owner' - Undocumented member.
--
-- 'profileArn', 'profile_profileArn' - The profile ARN.
--
-- 'profileDescription', 'profile_profileDescription' - The profile description.
--
-- 'profileName', 'profile_profileName' - The profile name.
--
-- 'profileQuestions', 'profile_profileQuestions' - Profile questions.
--
-- 'profileVersion', 'profile_profileVersion' - The profile version.
--
-- 'shareInvitationId', 'profile_shareInvitationId' - The ID assigned to the share invitation.
--
-- 'tags', 'profile_tags' - The tags assigned to the profile.
--
-- 'updatedAt', 'profile_updatedAt' - Undocumented member.
newProfile ::
  Profile
newProfile =
  Profile'
    { createdAt = Prelude.Nothing,
      owner = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      profileDescription = Prelude.Nothing,
      profileName = Prelude.Nothing,
      profileQuestions = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      shareInvitationId = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Undocumented member.
profile_createdAt :: Lens.Lens' Profile (Prelude.Maybe Prelude.UTCTime)
profile_createdAt = Lens.lens (\Profile' {createdAt} -> createdAt) (\s@Profile' {} a -> s {createdAt = a} :: Profile) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
profile_owner :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_owner = Lens.lens (\Profile' {owner} -> owner) (\s@Profile' {} a -> s {owner = a} :: Profile)

-- | The profile ARN.
profile_profileArn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileArn = Lens.lens (\Profile' {profileArn} -> profileArn) (\s@Profile' {} a -> s {profileArn = a} :: Profile)

-- | The profile description.
profile_profileDescription :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileDescription = Lens.lens (\Profile' {profileDescription} -> profileDescription) (\s@Profile' {} a -> s {profileDescription = a} :: Profile)

-- | The profile name.
profile_profileName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileName = Lens.lens (\Profile' {profileName} -> profileName) (\s@Profile' {} a -> s {profileName = a} :: Profile)

-- | Profile questions.
profile_profileQuestions :: Lens.Lens' Profile (Prelude.Maybe [ProfileQuestion])
profile_profileQuestions = Lens.lens (\Profile' {profileQuestions} -> profileQuestions) (\s@Profile' {} a -> s {profileQuestions = a} :: Profile) Prelude.. Lens.mapping Lens.coerced

-- | The profile version.
profile_profileVersion :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileVersion = Lens.lens (\Profile' {profileVersion} -> profileVersion) (\s@Profile' {} a -> s {profileVersion = a} :: Profile)

-- | The ID assigned to the share invitation.
profile_shareInvitationId :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_shareInvitationId = Lens.lens (\Profile' {shareInvitationId} -> shareInvitationId) (\s@Profile' {} a -> s {shareInvitationId = a} :: Profile)

-- | The tags assigned to the profile.
profile_tags :: Lens.Lens' Profile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profile_tags = Lens.lens (\Profile' {tags} -> tags) (\s@Profile' {} a -> s {tags = a} :: Profile) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
profile_updatedAt :: Lens.Lens' Profile (Prelude.Maybe Prelude.UTCTime)
profile_updatedAt = Lens.lens (\Profile' {updatedAt} -> updatedAt) (\s@Profile' {} a -> s {updatedAt = a} :: Profile) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Profile where
  parseJSON =
    Data.withObject
      "Profile"
      ( \x ->
          Profile'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "ProfileArn")
            Prelude.<*> (x Data..:? "ProfileDescription")
            Prelude.<*> (x Data..:? "ProfileName")
            Prelude.<*> ( x
                            Data..:? "ProfileQuestions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProfileVersion")
            Prelude.<*> (x Data..:? "ShareInvitationId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable Profile where
  hashWithSalt _salt Profile' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` profileDescription
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` profileQuestions
      `Prelude.hashWithSalt` profileVersion
      `Prelude.hashWithSalt` shareInvitationId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Profile where
  rnf Profile' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf profileDescription
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf profileQuestions
      `Prelude.seq` Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf shareInvitationId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updatedAt
