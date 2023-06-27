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
-- Module      : Amazonka.WellArchitected.Types.ProfileNotificationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileNotificationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ProfileNotificationType

-- | The profile notification summary.
--
-- /See:/ 'newProfileNotificationSummary' smart constructor.
data ProfileNotificationSummary = ProfileNotificationSummary'
  { -- | The current profile version.
    currentProfileVersion :: Prelude.Maybe Prelude.Text,
    -- | The latest profile version.
    latestProfileVersion :: Prelude.Maybe Prelude.Text,
    -- | The profile ARN.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The profile name.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | Type of notification.
    type' :: Prelude.Maybe ProfileNotificationType,
    workloadId :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileNotificationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentProfileVersion', 'profileNotificationSummary_currentProfileVersion' - The current profile version.
--
-- 'latestProfileVersion', 'profileNotificationSummary_latestProfileVersion' - The latest profile version.
--
-- 'profileArn', 'profileNotificationSummary_profileArn' - The profile ARN.
--
-- 'profileName', 'profileNotificationSummary_profileName' - The profile name.
--
-- 'type'', 'profileNotificationSummary_type' - Type of notification.
--
-- 'workloadId', 'profileNotificationSummary_workloadId' - Undocumented member.
--
-- 'workloadName', 'profileNotificationSummary_workloadName' - Undocumented member.
newProfileNotificationSummary ::
  ProfileNotificationSummary
newProfileNotificationSummary =
  ProfileNotificationSummary'
    { currentProfileVersion =
        Prelude.Nothing,
      latestProfileVersion = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      profileName = Prelude.Nothing,
      type' = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      workloadName = Prelude.Nothing
    }

-- | The current profile version.
profileNotificationSummary_currentProfileVersion :: Lens.Lens' ProfileNotificationSummary (Prelude.Maybe Prelude.Text)
profileNotificationSummary_currentProfileVersion = Lens.lens (\ProfileNotificationSummary' {currentProfileVersion} -> currentProfileVersion) (\s@ProfileNotificationSummary' {} a -> s {currentProfileVersion = a} :: ProfileNotificationSummary)

-- | The latest profile version.
profileNotificationSummary_latestProfileVersion :: Lens.Lens' ProfileNotificationSummary (Prelude.Maybe Prelude.Text)
profileNotificationSummary_latestProfileVersion = Lens.lens (\ProfileNotificationSummary' {latestProfileVersion} -> latestProfileVersion) (\s@ProfileNotificationSummary' {} a -> s {latestProfileVersion = a} :: ProfileNotificationSummary)

-- | The profile ARN.
profileNotificationSummary_profileArn :: Lens.Lens' ProfileNotificationSummary (Prelude.Maybe Prelude.Text)
profileNotificationSummary_profileArn = Lens.lens (\ProfileNotificationSummary' {profileArn} -> profileArn) (\s@ProfileNotificationSummary' {} a -> s {profileArn = a} :: ProfileNotificationSummary)

-- | The profile name.
profileNotificationSummary_profileName :: Lens.Lens' ProfileNotificationSummary (Prelude.Maybe Prelude.Text)
profileNotificationSummary_profileName = Lens.lens (\ProfileNotificationSummary' {profileName} -> profileName) (\s@ProfileNotificationSummary' {} a -> s {profileName = a} :: ProfileNotificationSummary)

-- | Type of notification.
profileNotificationSummary_type :: Lens.Lens' ProfileNotificationSummary (Prelude.Maybe ProfileNotificationType)
profileNotificationSummary_type = Lens.lens (\ProfileNotificationSummary' {type'} -> type') (\s@ProfileNotificationSummary' {} a -> s {type' = a} :: ProfileNotificationSummary)

-- | Undocumented member.
profileNotificationSummary_workloadId :: Lens.Lens' ProfileNotificationSummary (Prelude.Maybe Prelude.Text)
profileNotificationSummary_workloadId = Lens.lens (\ProfileNotificationSummary' {workloadId} -> workloadId) (\s@ProfileNotificationSummary' {} a -> s {workloadId = a} :: ProfileNotificationSummary)

-- | Undocumented member.
profileNotificationSummary_workloadName :: Lens.Lens' ProfileNotificationSummary (Prelude.Maybe Prelude.Text)
profileNotificationSummary_workloadName = Lens.lens (\ProfileNotificationSummary' {workloadName} -> workloadName) (\s@ProfileNotificationSummary' {} a -> s {workloadName = a} :: ProfileNotificationSummary)

instance Data.FromJSON ProfileNotificationSummary where
  parseJSON =
    Data.withObject
      "ProfileNotificationSummary"
      ( \x ->
          ProfileNotificationSummary'
            Prelude.<$> (x Data..:? "CurrentProfileVersion")
            Prelude.<*> (x Data..:? "LatestProfileVersion")
            Prelude.<*> (x Data..:? "ProfileArn")
            Prelude.<*> (x Data..:? "ProfileName")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "WorkloadId")
            Prelude.<*> (x Data..:? "WorkloadName")
      )

instance Prelude.Hashable ProfileNotificationSummary where
  hashWithSalt _salt ProfileNotificationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` currentProfileVersion
      `Prelude.hashWithSalt` latestProfileVersion
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` workloadName

instance Prelude.NFData ProfileNotificationSummary where
  rnf ProfileNotificationSummary' {..} =
    Prelude.rnf currentProfileVersion
      `Prelude.seq` Prelude.rnf latestProfileVersion
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf workloadName
