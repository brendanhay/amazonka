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
-- Module      : Amazonka.EMRServerless.Types.ManagedPersistenceMonitoringConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.ManagedPersistenceMonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The managed log persistence configuration for a job run.
--
-- /See:/ 'newManagedPersistenceMonitoringConfiguration' smart constructor.
data ManagedPersistenceMonitoringConfiguration = ManagedPersistenceMonitoringConfiguration'
  { -- | The KMS key ARN to encrypt the logs stored in managed log persistence.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Enables managed logging and defaults to true. If set to false, managed
    -- logging will be turned off.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedPersistenceMonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKeyArn', 'managedPersistenceMonitoringConfiguration_encryptionKeyArn' - The KMS key ARN to encrypt the logs stored in managed log persistence.
--
-- 'enabled', 'managedPersistenceMonitoringConfiguration_enabled' - Enables managed logging and defaults to true. If set to false, managed
-- logging will be turned off.
newManagedPersistenceMonitoringConfiguration ::
  ManagedPersistenceMonitoringConfiguration
newManagedPersistenceMonitoringConfiguration =
  ManagedPersistenceMonitoringConfiguration'
    { encryptionKeyArn =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The KMS key ARN to encrypt the logs stored in managed log persistence.
managedPersistenceMonitoringConfiguration_encryptionKeyArn :: Lens.Lens' ManagedPersistenceMonitoringConfiguration (Prelude.Maybe Prelude.Text)
managedPersistenceMonitoringConfiguration_encryptionKeyArn = Lens.lens (\ManagedPersistenceMonitoringConfiguration' {encryptionKeyArn} -> encryptionKeyArn) (\s@ManagedPersistenceMonitoringConfiguration' {} a -> s {encryptionKeyArn = a} :: ManagedPersistenceMonitoringConfiguration)

-- | Enables managed logging and defaults to true. If set to false, managed
-- logging will be turned off.
managedPersistenceMonitoringConfiguration_enabled :: Lens.Lens' ManagedPersistenceMonitoringConfiguration (Prelude.Maybe Prelude.Bool)
managedPersistenceMonitoringConfiguration_enabled = Lens.lens (\ManagedPersistenceMonitoringConfiguration' {enabled} -> enabled) (\s@ManagedPersistenceMonitoringConfiguration' {} a -> s {enabled = a} :: ManagedPersistenceMonitoringConfiguration)

instance
  Core.FromJSON
    ManagedPersistenceMonitoringConfiguration
  where
  parseJSON =
    Core.withObject
      "ManagedPersistenceMonitoringConfiguration"
      ( \x ->
          ManagedPersistenceMonitoringConfiguration'
            Prelude.<$> (x Core..:? "encryptionKeyArn")
              Prelude.<*> (x Core..:? "enabled")
      )

instance
  Prelude.Hashable
    ManagedPersistenceMonitoringConfiguration
  where
  hashWithSalt
    _salt
    ManagedPersistenceMonitoringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` encryptionKeyArn
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    ManagedPersistenceMonitoringConfiguration
  where
  rnf ManagedPersistenceMonitoringConfiguration' {..} =
    Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf enabled

instance
  Core.ToJSON
    ManagedPersistenceMonitoringConfiguration
  where
  toJSON ManagedPersistenceMonitoringConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionKeyArn" Core..=)
              Prelude.<$> encryptionKeyArn,
            ("enabled" Core..=) Prelude.<$> enabled
          ]
      )
