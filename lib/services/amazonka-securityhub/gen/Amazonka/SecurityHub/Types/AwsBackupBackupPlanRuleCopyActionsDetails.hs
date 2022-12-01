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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleCopyActionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleCopyActionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanLifecycleDetails

-- | An array of @CopyAction@ objects, each of which contains details of the
-- copy operation.
--
-- /See:/ 'newAwsBackupBackupPlanRuleCopyActionsDetails' smart constructor.
data AwsBackupBackupPlanRuleCopyActionsDetails = AwsBackupBackupPlanRuleCopyActionsDetails'
  { -- | Defines when a protected resource is transitioned to cold storage and
    -- when it expires. Backup transitions and expires backups automatically
    -- according to the lifecycle that you define. If you do not specify a
    -- lifecycle, Backup applies the lifecycle policy of the source backup to
    -- the destination backup.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days.
    lifecycle :: Prelude.Maybe AwsBackupBackupPlanLifecycleDetails,
    -- | An Amazon Resource Name (ARN) that uniquely identifies the destination
    -- backup vault for the copied backup.
    destinationBackupVaultArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupPlanRuleCopyActionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'awsBackupBackupPlanRuleCopyActionsDetails_lifecycle' - Defines when a protected resource is transitioned to cold storage and
-- when it expires. Backup transitions and expires backups automatically
-- according to the lifecycle that you define. If you do not specify a
-- lifecycle, Backup applies the lifecycle policy of the source backup to
-- the destination backup.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days.
--
-- 'destinationBackupVaultArn', 'awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies the destination
-- backup vault for the copied backup.
newAwsBackupBackupPlanRuleCopyActionsDetails ::
  AwsBackupBackupPlanRuleCopyActionsDetails
newAwsBackupBackupPlanRuleCopyActionsDetails =
  AwsBackupBackupPlanRuleCopyActionsDetails'
    { lifecycle =
        Prelude.Nothing,
      destinationBackupVaultArn =
        Prelude.Nothing
    }

-- | Defines when a protected resource is transitioned to cold storage and
-- when it expires. Backup transitions and expires backups automatically
-- according to the lifecycle that you define. If you do not specify a
-- lifecycle, Backup applies the lifecycle policy of the source backup to
-- the destination backup.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days.
awsBackupBackupPlanRuleCopyActionsDetails_lifecycle :: Lens.Lens' AwsBackupBackupPlanRuleCopyActionsDetails (Prelude.Maybe AwsBackupBackupPlanLifecycleDetails)
awsBackupBackupPlanRuleCopyActionsDetails_lifecycle = Lens.lens (\AwsBackupBackupPlanRuleCopyActionsDetails' {lifecycle} -> lifecycle) (\s@AwsBackupBackupPlanRuleCopyActionsDetails' {} a -> s {lifecycle = a} :: AwsBackupBackupPlanRuleCopyActionsDetails)

-- | An Amazon Resource Name (ARN) that uniquely identifies the destination
-- backup vault for the copied backup.
awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn :: Lens.Lens' AwsBackupBackupPlanRuleCopyActionsDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn = Lens.lens (\AwsBackupBackupPlanRuleCopyActionsDetails' {destinationBackupVaultArn} -> destinationBackupVaultArn) (\s@AwsBackupBackupPlanRuleCopyActionsDetails' {} a -> s {destinationBackupVaultArn = a} :: AwsBackupBackupPlanRuleCopyActionsDetails)

instance
  Core.FromJSON
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsBackupBackupPlanRuleCopyActionsDetails"
      ( \x ->
          AwsBackupBackupPlanRuleCopyActionsDetails'
            Prelude.<$> (x Core..:? "Lifecycle")
              Prelude.<*> (x Core..:? "DestinationBackupVaultArn")
      )

instance
  Prelude.Hashable
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  hashWithSalt
    _salt
    AwsBackupBackupPlanRuleCopyActionsDetails' {..} =
      _salt `Prelude.hashWithSalt` lifecycle
        `Prelude.hashWithSalt` destinationBackupVaultArn

instance
  Prelude.NFData
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  rnf AwsBackupBackupPlanRuleCopyActionsDetails' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf destinationBackupVaultArn

instance
  Core.ToJSON
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  toJSON AwsBackupBackupPlanRuleCopyActionsDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Lifecycle" Core..=) Prelude.<$> lifecycle,
            ("DestinationBackupVaultArn" Core..=)
              Prelude.<$> destinationBackupVaultArn
          ]
      )
