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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleCopyActionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanLifecycleDetails

-- | An array of @CopyAction@ objects, each of which contains details of the
-- copy operation.
--
-- /See:/ 'newAwsBackupBackupPlanRuleCopyActionsDetails' smart constructor.
data AwsBackupBackupPlanRuleCopyActionsDetails = AwsBackupBackupPlanRuleCopyActionsDetails'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies the destination
    -- backup vault for the copied backup.
    destinationBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | Defines when a protected resource is transitioned to cold storage and
    -- when it expires. Backup transitions and expires backups automatically
    -- according to the lifecycle that you define. If you do not specify a
    -- lifecycle, Backup applies the lifecycle policy of the source backup to
    -- the destination backup.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days.
    lifecycle :: Prelude.Maybe AwsBackupBackupPlanLifecycleDetails
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
-- 'destinationBackupVaultArn', 'awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies the destination
-- backup vault for the copied backup.
--
-- 'lifecycle', 'awsBackupBackupPlanRuleCopyActionsDetails_lifecycle' - Defines when a protected resource is transitioned to cold storage and
-- when it expires. Backup transitions and expires backups automatically
-- according to the lifecycle that you define. If you do not specify a
-- lifecycle, Backup applies the lifecycle policy of the source backup to
-- the destination backup.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days.
newAwsBackupBackupPlanRuleCopyActionsDetails ::
  AwsBackupBackupPlanRuleCopyActionsDetails
newAwsBackupBackupPlanRuleCopyActionsDetails =
  AwsBackupBackupPlanRuleCopyActionsDetails'
    { destinationBackupVaultArn =
        Prelude.Nothing,
      lifecycle = Prelude.Nothing
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies the destination
-- backup vault for the copied backup.
awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn :: Lens.Lens' AwsBackupBackupPlanRuleCopyActionsDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn = Lens.lens (\AwsBackupBackupPlanRuleCopyActionsDetails' {destinationBackupVaultArn} -> destinationBackupVaultArn) (\s@AwsBackupBackupPlanRuleCopyActionsDetails' {} a -> s {destinationBackupVaultArn = a} :: AwsBackupBackupPlanRuleCopyActionsDetails)

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

instance
  Data.FromJSON
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsBackupBackupPlanRuleCopyActionsDetails"
      ( \x ->
          AwsBackupBackupPlanRuleCopyActionsDetails'
            Prelude.<$> (x Data..:? "DestinationBackupVaultArn")
              Prelude.<*> (x Data..:? "Lifecycle")
      )

instance
  Prelude.Hashable
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  hashWithSalt
    _salt
    AwsBackupBackupPlanRuleCopyActionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` destinationBackupVaultArn
        `Prelude.hashWithSalt` lifecycle

instance
  Prelude.NFData
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  rnf AwsBackupBackupPlanRuleCopyActionsDetails' {..} =
    Prelude.rnf destinationBackupVaultArn
      `Prelude.seq` Prelude.rnf lifecycle

instance
  Data.ToJSON
    AwsBackupBackupPlanRuleCopyActionsDetails
  where
  toJSON AwsBackupBackupPlanRuleCopyActionsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationBackupVaultArn" Data..=)
              Prelude.<$> destinationBackupVaultArn,
            ("Lifecycle" Data..=) Prelude.<$> lifecycle
          ]
      )
