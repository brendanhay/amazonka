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
-- Module      : Amazonka.BackupGateway.Types.HypervisorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.HypervisorDetails where

import Amazonka.BackupGateway.Types.HypervisorState
import Amazonka.BackupGateway.Types.SyncMetadataStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | These are the details of the specified hypervisor. A hypervisor is
-- hardware, software, or firmware that creates and manages virtual
-- machines, and allocates resources to them.
--
-- /See:/ 'newHypervisorDetails' smart constructor.
data HypervisorDetails = HypervisorDetails'
  { -- | The server host of the hypervisor. This can be either an IP address or a
    -- fully-qualified domain name (FQDN).
    host :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the KMS used to encrypt the
    -- hypervisor.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | This is the time when the most recent successful sync of metadata
    -- occurred.
    lastSuccessfulMetadataSyncTime :: Prelude.Maybe Data.POSIX,
    -- | This is the most recent status for the indicated metadata sync.
    latestMetadataSyncStatus :: Prelude.Maybe SyncMetadataStatus,
    -- | This is the most recent status for the indicated metadata sync.
    latestMetadataSyncStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the group of gateways within the
    -- requested log.
    logGroupArn :: Prelude.Maybe Prelude.Text,
    -- | This is the name of the specified hypervisor.
    name :: Prelude.Maybe Prelude.Text,
    -- | This is the current state of the specified hypervisor.
    --
    -- The possible states are @PENDING@, @ONLINE@, @OFFLINE@, or @ERROR@.
    state :: Prelude.Maybe HypervisorState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HypervisorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'hypervisorDetails_host' - The server host of the hypervisor. This can be either an IP address or a
-- fully-qualified domain name (FQDN).
--
-- 'hypervisorArn', 'hypervisorDetails_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
--
-- 'kmsKeyArn', 'hypervisorDetails_kmsKeyArn' - The Amazon Resource Name (ARN) of the KMS used to encrypt the
-- hypervisor.
--
-- 'lastSuccessfulMetadataSyncTime', 'hypervisorDetails_lastSuccessfulMetadataSyncTime' - This is the time when the most recent successful sync of metadata
-- occurred.
--
-- 'latestMetadataSyncStatus', 'hypervisorDetails_latestMetadataSyncStatus' - This is the most recent status for the indicated metadata sync.
--
-- 'latestMetadataSyncStatusMessage', 'hypervisorDetails_latestMetadataSyncStatusMessage' - This is the most recent status for the indicated metadata sync.
--
-- 'logGroupArn', 'hypervisorDetails_logGroupArn' - The Amazon Resource Name (ARN) of the group of gateways within the
-- requested log.
--
-- 'name', 'hypervisorDetails_name' - This is the name of the specified hypervisor.
--
-- 'state', 'hypervisorDetails_state' - This is the current state of the specified hypervisor.
--
-- The possible states are @PENDING@, @ONLINE@, @OFFLINE@, or @ERROR@.
newHypervisorDetails ::
  HypervisorDetails
newHypervisorDetails =
  HypervisorDetails'
    { host = Prelude.Nothing,
      hypervisorArn = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      lastSuccessfulMetadataSyncTime = Prelude.Nothing,
      latestMetadataSyncStatus = Prelude.Nothing,
      latestMetadataSyncStatusMessage = Prelude.Nothing,
      logGroupArn = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The server host of the hypervisor. This can be either an IP address or a
-- fully-qualified domain name (FQDN).
hypervisorDetails_host :: Lens.Lens' HypervisorDetails (Prelude.Maybe Prelude.Text)
hypervisorDetails_host = Lens.lens (\HypervisorDetails' {host} -> host) (\s@HypervisorDetails' {} a -> s {host = a} :: HypervisorDetails)

-- | The Amazon Resource Name (ARN) of the hypervisor.
hypervisorDetails_hypervisorArn :: Lens.Lens' HypervisorDetails (Prelude.Maybe Prelude.Text)
hypervisorDetails_hypervisorArn = Lens.lens (\HypervisorDetails' {hypervisorArn} -> hypervisorArn) (\s@HypervisorDetails' {} a -> s {hypervisorArn = a} :: HypervisorDetails)

-- | The Amazon Resource Name (ARN) of the KMS used to encrypt the
-- hypervisor.
hypervisorDetails_kmsKeyArn :: Lens.Lens' HypervisorDetails (Prelude.Maybe Prelude.Text)
hypervisorDetails_kmsKeyArn = Lens.lens (\HypervisorDetails' {kmsKeyArn} -> kmsKeyArn) (\s@HypervisorDetails' {} a -> s {kmsKeyArn = a} :: HypervisorDetails)

-- | This is the time when the most recent successful sync of metadata
-- occurred.
hypervisorDetails_lastSuccessfulMetadataSyncTime :: Lens.Lens' HypervisorDetails (Prelude.Maybe Prelude.UTCTime)
hypervisorDetails_lastSuccessfulMetadataSyncTime = Lens.lens (\HypervisorDetails' {lastSuccessfulMetadataSyncTime} -> lastSuccessfulMetadataSyncTime) (\s@HypervisorDetails' {} a -> s {lastSuccessfulMetadataSyncTime = a} :: HypervisorDetails) Prelude.. Lens.mapping Data._Time

-- | This is the most recent status for the indicated metadata sync.
hypervisorDetails_latestMetadataSyncStatus :: Lens.Lens' HypervisorDetails (Prelude.Maybe SyncMetadataStatus)
hypervisorDetails_latestMetadataSyncStatus = Lens.lens (\HypervisorDetails' {latestMetadataSyncStatus} -> latestMetadataSyncStatus) (\s@HypervisorDetails' {} a -> s {latestMetadataSyncStatus = a} :: HypervisorDetails)

-- | This is the most recent status for the indicated metadata sync.
hypervisorDetails_latestMetadataSyncStatusMessage :: Lens.Lens' HypervisorDetails (Prelude.Maybe Prelude.Text)
hypervisorDetails_latestMetadataSyncStatusMessage = Lens.lens (\HypervisorDetails' {latestMetadataSyncStatusMessage} -> latestMetadataSyncStatusMessage) (\s@HypervisorDetails' {} a -> s {latestMetadataSyncStatusMessage = a} :: HypervisorDetails)

-- | The Amazon Resource Name (ARN) of the group of gateways within the
-- requested log.
hypervisorDetails_logGroupArn :: Lens.Lens' HypervisorDetails (Prelude.Maybe Prelude.Text)
hypervisorDetails_logGroupArn = Lens.lens (\HypervisorDetails' {logGroupArn} -> logGroupArn) (\s@HypervisorDetails' {} a -> s {logGroupArn = a} :: HypervisorDetails)

-- | This is the name of the specified hypervisor.
hypervisorDetails_name :: Lens.Lens' HypervisorDetails (Prelude.Maybe Prelude.Text)
hypervisorDetails_name = Lens.lens (\HypervisorDetails' {name} -> name) (\s@HypervisorDetails' {} a -> s {name = a} :: HypervisorDetails)

-- | This is the current state of the specified hypervisor.
--
-- The possible states are @PENDING@, @ONLINE@, @OFFLINE@, or @ERROR@.
hypervisorDetails_state :: Lens.Lens' HypervisorDetails (Prelude.Maybe HypervisorState)
hypervisorDetails_state = Lens.lens (\HypervisorDetails' {state} -> state) (\s@HypervisorDetails' {} a -> s {state = a} :: HypervisorDetails)

instance Data.FromJSON HypervisorDetails where
  parseJSON =
    Data.withObject
      "HypervisorDetails"
      ( \x ->
          HypervisorDetails'
            Prelude.<$> (x Data..:? "Host")
            Prelude.<*> (x Data..:? "HypervisorArn")
            Prelude.<*> (x Data..:? "KmsKeyArn")
            Prelude.<*> (x Data..:? "LastSuccessfulMetadataSyncTime")
            Prelude.<*> (x Data..:? "LatestMetadataSyncStatus")
            Prelude.<*> (x Data..:? "LatestMetadataSyncStatusMessage")
            Prelude.<*> (x Data..:? "LogGroupArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable HypervisorDetails where
  hashWithSalt _salt HypervisorDetails' {..} =
    _salt
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` hypervisorArn
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` lastSuccessfulMetadataSyncTime
      `Prelude.hashWithSalt` latestMetadataSyncStatus
      `Prelude.hashWithSalt` latestMetadataSyncStatusMessage
      `Prelude.hashWithSalt` logGroupArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData HypervisorDetails where
  rnf HypervisorDetails' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf hypervisorArn
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf lastSuccessfulMetadataSyncTime
      `Prelude.seq` Prelude.rnf latestMetadataSyncStatus
      `Prelude.seq` Prelude.rnf latestMetadataSyncStatusMessage
      `Prelude.seq` Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
