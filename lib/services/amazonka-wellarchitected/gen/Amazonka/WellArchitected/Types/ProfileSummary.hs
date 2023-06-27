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
-- Module      : Amazonka.WellArchitected.Types.ProfileSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of a profile.
--
-- /See:/ 'newProfileSummary' smart constructor.
data ProfileSummary = ProfileSummary'
  { createdAt :: Prelude.Maybe Data.POSIX,
    owner :: Prelude.Maybe Prelude.Text,
    -- | The profile ARN.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The profile description.
    profileDescription :: Prelude.Maybe Prelude.Text,
    -- | The profile name.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The profile version.
    profileVersion :: Prelude.Maybe Prelude.Text,
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'profileSummary_createdAt' - Undocumented member.
--
-- 'owner', 'profileSummary_owner' - Undocumented member.
--
-- 'profileArn', 'profileSummary_profileArn' - The profile ARN.
--
-- 'profileDescription', 'profileSummary_profileDescription' - The profile description.
--
-- 'profileName', 'profileSummary_profileName' - The profile name.
--
-- 'profileVersion', 'profileSummary_profileVersion' - The profile version.
--
-- 'updatedAt', 'profileSummary_updatedAt' - Undocumented member.
newProfileSummary ::
  ProfileSummary
newProfileSummary =
  ProfileSummary'
    { createdAt = Prelude.Nothing,
      owner = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      profileDescription = Prelude.Nothing,
      profileName = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Undocumented member.
profileSummary_createdAt :: Lens.Lens' ProfileSummary (Prelude.Maybe Prelude.UTCTime)
profileSummary_createdAt = Lens.lens (\ProfileSummary' {createdAt} -> createdAt) (\s@ProfileSummary' {} a -> s {createdAt = a} :: ProfileSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
profileSummary_owner :: Lens.Lens' ProfileSummary (Prelude.Maybe Prelude.Text)
profileSummary_owner = Lens.lens (\ProfileSummary' {owner} -> owner) (\s@ProfileSummary' {} a -> s {owner = a} :: ProfileSummary)

-- | The profile ARN.
profileSummary_profileArn :: Lens.Lens' ProfileSummary (Prelude.Maybe Prelude.Text)
profileSummary_profileArn = Lens.lens (\ProfileSummary' {profileArn} -> profileArn) (\s@ProfileSummary' {} a -> s {profileArn = a} :: ProfileSummary)

-- | The profile description.
profileSummary_profileDescription :: Lens.Lens' ProfileSummary (Prelude.Maybe Prelude.Text)
profileSummary_profileDescription = Lens.lens (\ProfileSummary' {profileDescription} -> profileDescription) (\s@ProfileSummary' {} a -> s {profileDescription = a} :: ProfileSummary)

-- | The profile name.
profileSummary_profileName :: Lens.Lens' ProfileSummary (Prelude.Maybe Prelude.Text)
profileSummary_profileName = Lens.lens (\ProfileSummary' {profileName} -> profileName) (\s@ProfileSummary' {} a -> s {profileName = a} :: ProfileSummary)

-- | The profile version.
profileSummary_profileVersion :: Lens.Lens' ProfileSummary (Prelude.Maybe Prelude.Text)
profileSummary_profileVersion = Lens.lens (\ProfileSummary' {profileVersion} -> profileVersion) (\s@ProfileSummary' {} a -> s {profileVersion = a} :: ProfileSummary)

-- | Undocumented member.
profileSummary_updatedAt :: Lens.Lens' ProfileSummary (Prelude.Maybe Prelude.UTCTime)
profileSummary_updatedAt = Lens.lens (\ProfileSummary' {updatedAt} -> updatedAt) (\s@ProfileSummary' {} a -> s {updatedAt = a} :: ProfileSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ProfileSummary where
  parseJSON =
    Data.withObject
      "ProfileSummary"
      ( \x ->
          ProfileSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "ProfileArn")
            Prelude.<*> (x Data..:? "ProfileDescription")
            Prelude.<*> (x Data..:? "ProfileName")
            Prelude.<*> (x Data..:? "ProfileVersion")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable ProfileSummary where
  hashWithSalt _salt ProfileSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` profileDescription
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` profileVersion
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ProfileSummary where
  rnf ProfileSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf profileDescription
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf updatedAt
