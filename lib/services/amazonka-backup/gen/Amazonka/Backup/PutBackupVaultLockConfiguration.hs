{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.PutBackupVaultLockConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies Backup Vault Lock to a backup vault, preventing attempts to
-- delete any recovery point stored in or created in a backup vault. Vault
-- Lock also prevents attempts to update the lifecycle policy that controls
-- the retention period of any recovery point currently stored in a backup
-- vault. If specified, Vault Lock enforces a minimum and maximum retention
-- period for future backup and copy jobs that target a backup vault.
--
-- Backup Vault Lock has been assessed by Cohasset Associates for use in
-- environments that are subject to SEC 17a-4, CFTC, and FINRA regulations.
-- For more information about how Backup Vault Lock relates to these
-- regulations, see the
-- <samples/cohassetreport.zip Cohasset Associates Compliance Assessment.>
module Amazonka.Backup.PutBackupVaultLockConfiguration
  ( -- * Creating a Request
    PutBackupVaultLockConfiguration (..),
    newPutBackupVaultLockConfiguration,

    -- * Request Lenses
    putBackupVaultLockConfiguration_changeableForDays,
    putBackupVaultLockConfiguration_maxRetentionDays,
    putBackupVaultLockConfiguration_minRetentionDays,
    putBackupVaultLockConfiguration_backupVaultName,

    -- * Destructuring the Response
    PutBackupVaultLockConfigurationResponse (..),
    newPutBackupVaultLockConfigurationResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutBackupVaultLockConfiguration' smart constructor.
data PutBackupVaultLockConfiguration = PutBackupVaultLockConfiguration'
  { -- | The Backup Vault Lock configuration that specifies the number of days
    -- before the lock date. For example, setting @ChangeableForDays@ to 30 on
    -- Jan. 1, 2022 at 8pm UTC will set the lock date to Jan. 31, 2022 at 8pm
    -- UTC.
    --
    -- Backup enforces a 72-hour cooling-off period before Vault Lock takes
    -- effect and becomes immutable. Therefore, you must set
    -- @ChangeableForDays@ to 3 or greater.
    --
    -- Before the lock date, you can delete Vault Lock from the vault using
    -- @DeleteBackupVaultLockConfiguration@ or change the Vault Lock
    -- configuration using @PutBackupVaultLockConfiguration@. On and after the
    -- lock date, the Vault Lock becomes immutable and cannot be changed or
    -- deleted.
    --
    -- If this parameter is not specified, you can delete Vault Lock from the
    -- vault using @DeleteBackupVaultLockConfiguration@ or change the Vault
    -- Lock configuration using @PutBackupVaultLockConfiguration@ at any time.
    changeableForDays :: Prelude.Maybe Prelude.Integer,
    -- | The Backup Vault Lock configuration that specifies the maximum retention
    -- period that the vault retains its recovery points. This setting can be
    -- useful if, for example, your organization\'s policies require you to
    -- destroy certain data after retaining it for four years (1460 days).
    --
    -- If this parameter is not included, Vault Lock does not enforce a maximum
    -- retention period on the recovery points in the vault. If this parameter
    -- is included without a value, Vault Lock will not enforce a maximum
    -- retention period.
    --
    -- If this parameter is specified, any backup or copy job to the vault must
    -- have a lifecycle policy with a retention period equal to or shorter than
    -- the maximum retention period. If the job\'s retention period is longer
    -- than that maximum retention period, then the vault fails the backup or
    -- copy job, and you should either modify your lifecycle settings or use a
    -- different vault. The longest maximum retention period you can specify is
    -- 36500 days (approximately 100 years). Recovery points already saved in
    -- the vault prior to Vault Lock are not affected.
    maxRetentionDays :: Prelude.Maybe Prelude.Integer,
    -- | The Backup Vault Lock configuration that specifies the minimum retention
    -- period that the vault retains its recovery points. This setting can be
    -- useful if, for example, your organization\'s policies require you to
    -- retain certain data for at least seven years (2555 days).
    --
    -- If this parameter is not specified, Vault Lock will not enforce a
    -- minimum retention period.
    --
    -- If this parameter is specified, any backup or copy job to the vault must
    -- have a lifecycle policy with a retention period equal to or longer than
    -- the minimum retention period. If the job\'s retention period is shorter
    -- than that minimum retention period, then the vault fails that backup or
    -- copy job, and you should either modify your lifecycle settings or use a
    -- different vault. The shortest minimum retention period you can specify
    -- is 1 day. Recovery points already saved in the vault prior to Vault Lock
    -- are not affected.
    minRetentionDays :: Prelude.Maybe Prelude.Integer,
    -- | The Backup Vault Lock configuration that specifies the name of the
    -- backup vault it protects.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBackupVaultLockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeableForDays', 'putBackupVaultLockConfiguration_changeableForDays' - The Backup Vault Lock configuration that specifies the number of days
-- before the lock date. For example, setting @ChangeableForDays@ to 30 on
-- Jan. 1, 2022 at 8pm UTC will set the lock date to Jan. 31, 2022 at 8pm
-- UTC.
--
-- Backup enforces a 72-hour cooling-off period before Vault Lock takes
-- effect and becomes immutable. Therefore, you must set
-- @ChangeableForDays@ to 3 or greater.
--
-- Before the lock date, you can delete Vault Lock from the vault using
-- @DeleteBackupVaultLockConfiguration@ or change the Vault Lock
-- configuration using @PutBackupVaultLockConfiguration@. On and after the
-- lock date, the Vault Lock becomes immutable and cannot be changed or
-- deleted.
--
-- If this parameter is not specified, you can delete Vault Lock from the
-- vault using @DeleteBackupVaultLockConfiguration@ or change the Vault
-- Lock configuration using @PutBackupVaultLockConfiguration@ at any time.
--
-- 'maxRetentionDays', 'putBackupVaultLockConfiguration_maxRetentionDays' - The Backup Vault Lock configuration that specifies the maximum retention
-- period that the vault retains its recovery points. This setting can be
-- useful if, for example, your organization\'s policies require you to
-- destroy certain data after retaining it for four years (1460 days).
--
-- If this parameter is not included, Vault Lock does not enforce a maximum
-- retention period on the recovery points in the vault. If this parameter
-- is included without a value, Vault Lock will not enforce a maximum
-- retention period.
--
-- If this parameter is specified, any backup or copy job to the vault must
-- have a lifecycle policy with a retention period equal to or shorter than
-- the maximum retention period. If the job\'s retention period is longer
-- than that maximum retention period, then the vault fails the backup or
-- copy job, and you should either modify your lifecycle settings or use a
-- different vault. The longest maximum retention period you can specify is
-- 36500 days (approximately 100 years). Recovery points already saved in
-- the vault prior to Vault Lock are not affected.
--
-- 'minRetentionDays', 'putBackupVaultLockConfiguration_minRetentionDays' - The Backup Vault Lock configuration that specifies the minimum retention
-- period that the vault retains its recovery points. This setting can be
-- useful if, for example, your organization\'s policies require you to
-- retain certain data for at least seven years (2555 days).
--
-- If this parameter is not specified, Vault Lock will not enforce a
-- minimum retention period.
--
-- If this parameter is specified, any backup or copy job to the vault must
-- have a lifecycle policy with a retention period equal to or longer than
-- the minimum retention period. If the job\'s retention period is shorter
-- than that minimum retention period, then the vault fails that backup or
-- copy job, and you should either modify your lifecycle settings or use a
-- different vault. The shortest minimum retention period you can specify
-- is 1 day. Recovery points already saved in the vault prior to Vault Lock
-- are not affected.
--
-- 'backupVaultName', 'putBackupVaultLockConfiguration_backupVaultName' - The Backup Vault Lock configuration that specifies the name of the
-- backup vault it protects.
newPutBackupVaultLockConfiguration ::
  -- | 'backupVaultName'
  Prelude.Text ->
  PutBackupVaultLockConfiguration
newPutBackupVaultLockConfiguration pBackupVaultName_ =
  PutBackupVaultLockConfiguration'
    { changeableForDays =
        Prelude.Nothing,
      maxRetentionDays = Prelude.Nothing,
      minRetentionDays = Prelude.Nothing,
      backupVaultName = pBackupVaultName_
    }

-- | The Backup Vault Lock configuration that specifies the number of days
-- before the lock date. For example, setting @ChangeableForDays@ to 30 on
-- Jan. 1, 2022 at 8pm UTC will set the lock date to Jan. 31, 2022 at 8pm
-- UTC.
--
-- Backup enforces a 72-hour cooling-off period before Vault Lock takes
-- effect and becomes immutable. Therefore, you must set
-- @ChangeableForDays@ to 3 or greater.
--
-- Before the lock date, you can delete Vault Lock from the vault using
-- @DeleteBackupVaultLockConfiguration@ or change the Vault Lock
-- configuration using @PutBackupVaultLockConfiguration@. On and after the
-- lock date, the Vault Lock becomes immutable and cannot be changed or
-- deleted.
--
-- If this parameter is not specified, you can delete Vault Lock from the
-- vault using @DeleteBackupVaultLockConfiguration@ or change the Vault
-- Lock configuration using @PutBackupVaultLockConfiguration@ at any time.
putBackupVaultLockConfiguration_changeableForDays :: Lens.Lens' PutBackupVaultLockConfiguration (Prelude.Maybe Prelude.Integer)
putBackupVaultLockConfiguration_changeableForDays = Lens.lens (\PutBackupVaultLockConfiguration' {changeableForDays} -> changeableForDays) (\s@PutBackupVaultLockConfiguration' {} a -> s {changeableForDays = a} :: PutBackupVaultLockConfiguration)

-- | The Backup Vault Lock configuration that specifies the maximum retention
-- period that the vault retains its recovery points. This setting can be
-- useful if, for example, your organization\'s policies require you to
-- destroy certain data after retaining it for four years (1460 days).
--
-- If this parameter is not included, Vault Lock does not enforce a maximum
-- retention period on the recovery points in the vault. If this parameter
-- is included without a value, Vault Lock will not enforce a maximum
-- retention period.
--
-- If this parameter is specified, any backup or copy job to the vault must
-- have a lifecycle policy with a retention period equal to or shorter than
-- the maximum retention period. If the job\'s retention period is longer
-- than that maximum retention period, then the vault fails the backup or
-- copy job, and you should either modify your lifecycle settings or use a
-- different vault. The longest maximum retention period you can specify is
-- 36500 days (approximately 100 years). Recovery points already saved in
-- the vault prior to Vault Lock are not affected.
putBackupVaultLockConfiguration_maxRetentionDays :: Lens.Lens' PutBackupVaultLockConfiguration (Prelude.Maybe Prelude.Integer)
putBackupVaultLockConfiguration_maxRetentionDays = Lens.lens (\PutBackupVaultLockConfiguration' {maxRetentionDays} -> maxRetentionDays) (\s@PutBackupVaultLockConfiguration' {} a -> s {maxRetentionDays = a} :: PutBackupVaultLockConfiguration)

-- | The Backup Vault Lock configuration that specifies the minimum retention
-- period that the vault retains its recovery points. This setting can be
-- useful if, for example, your organization\'s policies require you to
-- retain certain data for at least seven years (2555 days).
--
-- If this parameter is not specified, Vault Lock will not enforce a
-- minimum retention period.
--
-- If this parameter is specified, any backup or copy job to the vault must
-- have a lifecycle policy with a retention period equal to or longer than
-- the minimum retention period. If the job\'s retention period is shorter
-- than that minimum retention period, then the vault fails that backup or
-- copy job, and you should either modify your lifecycle settings or use a
-- different vault. The shortest minimum retention period you can specify
-- is 1 day. Recovery points already saved in the vault prior to Vault Lock
-- are not affected.
putBackupVaultLockConfiguration_minRetentionDays :: Lens.Lens' PutBackupVaultLockConfiguration (Prelude.Maybe Prelude.Integer)
putBackupVaultLockConfiguration_minRetentionDays = Lens.lens (\PutBackupVaultLockConfiguration' {minRetentionDays} -> minRetentionDays) (\s@PutBackupVaultLockConfiguration' {} a -> s {minRetentionDays = a} :: PutBackupVaultLockConfiguration)

-- | The Backup Vault Lock configuration that specifies the name of the
-- backup vault it protects.
putBackupVaultLockConfiguration_backupVaultName :: Lens.Lens' PutBackupVaultLockConfiguration Prelude.Text
putBackupVaultLockConfiguration_backupVaultName = Lens.lens (\PutBackupVaultLockConfiguration' {backupVaultName} -> backupVaultName) (\s@PutBackupVaultLockConfiguration' {} a -> s {backupVaultName = a} :: PutBackupVaultLockConfiguration)

instance
  Core.AWSRequest
    PutBackupVaultLockConfiguration
  where
  type
    AWSResponse PutBackupVaultLockConfiguration =
      PutBackupVaultLockConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      PutBackupVaultLockConfigurationResponse'

instance
  Prelude.Hashable
    PutBackupVaultLockConfiguration
  where
  hashWithSalt
    _salt
    PutBackupVaultLockConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` changeableForDays
        `Prelude.hashWithSalt` maxRetentionDays
        `Prelude.hashWithSalt` minRetentionDays
        `Prelude.hashWithSalt` backupVaultName

instance
  Prelude.NFData
    PutBackupVaultLockConfiguration
  where
  rnf PutBackupVaultLockConfiguration' {..} =
    Prelude.rnf changeableForDays
      `Prelude.seq` Prelude.rnf maxRetentionDays
      `Prelude.seq` Prelude.rnf minRetentionDays
      `Prelude.seq` Prelude.rnf backupVaultName

instance
  Data.ToHeaders
    PutBackupVaultLockConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutBackupVaultLockConfiguration where
  toJSON PutBackupVaultLockConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChangeableForDays" Data..=)
              Prelude.<$> changeableForDays,
            ("MaxRetentionDays" Data..=)
              Prelude.<$> maxRetentionDays,
            ("MinRetentionDays" Data..=)
              Prelude.<$> minRetentionDays
          ]
      )

instance Data.ToPath PutBackupVaultLockConfiguration where
  toPath PutBackupVaultLockConfiguration' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/vault-lock"
      ]

instance Data.ToQuery PutBackupVaultLockConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutBackupVaultLockConfigurationResponse' smart constructor.
data PutBackupVaultLockConfigurationResponse = PutBackupVaultLockConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBackupVaultLockConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBackupVaultLockConfigurationResponse ::
  PutBackupVaultLockConfigurationResponse
newPutBackupVaultLockConfigurationResponse =
  PutBackupVaultLockConfigurationResponse'

instance
  Prelude.NFData
    PutBackupVaultLockConfigurationResponse
  where
  rnf _ = ()
