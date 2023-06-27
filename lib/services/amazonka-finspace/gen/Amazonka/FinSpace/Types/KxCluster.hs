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
-- Module      : Amazonka.FinSpace.Types.KxCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.KxAzMode
import Amazonka.FinSpace.Types.KxClusterStatus
import Amazonka.FinSpace.Types.KxClusterType
import qualified Amazonka.Prelude as Prelude

-- | The details of a kdb cluster.
--
-- /See:/ 'newKxCluster' smart constructor.
data KxCluster = KxCluster'
  { -- | The availability zone identifiers for the requested regions.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The number of availability zones assigned per cluster. This can be one
    -- of the following
    --
    -- -   @SINGLE@ – Assigns one availability zone per cluster.
    --
    -- -   @MULTI@ – Assigns all the availability zones per cluster.
    azMode :: Prelude.Maybe KxAzMode,
    -- | A description of the cluster.
    clusterDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique name for the cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of KDB database that is being created. The following
    -- types are available:
    --
    -- -   HDB – A Historical Database. The data is only accessible with
    --     read-only permissions from one of the FinSpace managed kdb databases
    --     mounted to the cluster.
    --
    -- -   RDB – A Realtime Database. This type of database captures all the
    --     data from a ticker plant and stores it in memory until the end of
    --     day, after which it writes all of its data to a disk and reloads the
    --     HDB. This cluster type requires local storage for temporary storage
    --     of data during the savedown process. If you specify this field in
    --     your request, you must provide the @savedownStorageConfiguration@
    --     parameter.
    --
    -- -   GATEWAY – A gateway cluster allows you to access data across
    --     processes in kdb systems. It allows you to create your own routing
    --     logic using the initialization scripts and custom code. This type of
    --     cluster does not require a writable local storage.
    clusterType :: Prelude.Maybe KxClusterType,
    -- | The timestamp at which the cluster was created in FinSpace. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | An IAM role that defines a set of permissions associated with a cluster.
    -- These permissions are assumed when a cluster attempts to access another
    -- cluster.
    executionRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies a Q program that will be run at launch of a cluster. It is a
    -- relative path within /.zip/ file that contains the custom code, which
    -- will be loaded on the cluster. It must include the file name itself. For
    -- example, @somedir\/init.q@.
    initializationScript :: Prelude.Maybe Prelude.Text,
    -- | The last time that the cluster was modified. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A version of the FinSpace managed kdb to run.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The status of a cluster.
    --
    -- -   PENDING – The cluster is pending creation.
    --
    -- -   CREATING –The cluster creation process is in progress.
    --
    -- -   CREATE_FAILED– The cluster creation process has failed.
    --
    -- -   RUNNING – The cluster creation process is running.
    --
    -- -   UPDATING – The cluster is in the process of being updated.
    --
    -- -   DELETING – The cluster is in the process of being deleted.
    --
    -- -   DELETED – The cluster has been deleted.
    --
    -- -   DELETE_FAILED – The cluster failed to delete.
    status :: Prelude.Maybe KxClusterStatus,
    -- | The error message when a failed state occurs.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneId', 'kxCluster_availabilityZoneId' - The availability zone identifiers for the requested regions.
--
-- 'azMode', 'kxCluster_azMode' - The number of availability zones assigned per cluster. This can be one
-- of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
--
-- 'clusterDescription', 'kxCluster_clusterDescription' - A description of the cluster.
--
-- 'clusterName', 'kxCluster_clusterName' - A unique name for the cluster.
--
-- 'clusterType', 'kxCluster_clusterType' - Specifies the type of KDB database that is being created. The following
-- types are available:
--
-- -   HDB – A Historical Database. The data is only accessible with
--     read-only permissions from one of the FinSpace managed kdb databases
--     mounted to the cluster.
--
-- -   RDB – A Realtime Database. This type of database captures all the
--     data from a ticker plant and stores it in memory until the end of
--     day, after which it writes all of its data to a disk and reloads the
--     HDB. This cluster type requires local storage for temporary storage
--     of data during the savedown process. If you specify this field in
--     your request, you must provide the @savedownStorageConfiguration@
--     parameter.
--
-- -   GATEWAY – A gateway cluster allows you to access data across
--     processes in kdb systems. It allows you to create your own routing
--     logic using the initialization scripts and custom code. This type of
--     cluster does not require a writable local storage.
--
-- 'createdTimestamp', 'kxCluster_createdTimestamp' - The timestamp at which the cluster was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'executionRole', 'kxCluster_executionRole' - An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
--
-- 'initializationScript', 'kxCluster_initializationScript' - Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
--
-- 'lastModifiedTimestamp', 'kxCluster_lastModifiedTimestamp' - The last time that the cluster was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'releaseLabel', 'kxCluster_releaseLabel' - A version of the FinSpace managed kdb to run.
--
-- 'status', 'kxCluster_status' - The status of a cluster.
--
-- -   PENDING – The cluster is pending creation.
--
-- -   CREATING –The cluster creation process is in progress.
--
-- -   CREATE_FAILED– The cluster creation process has failed.
--
-- -   RUNNING – The cluster creation process is running.
--
-- -   UPDATING – The cluster is in the process of being updated.
--
-- -   DELETING – The cluster is in the process of being deleted.
--
-- -   DELETED – The cluster has been deleted.
--
-- -   DELETE_FAILED – The cluster failed to delete.
--
-- 'statusReason', 'kxCluster_statusReason' - The error message when a failed state occurs.
newKxCluster ::
  KxCluster
newKxCluster =
  KxCluster'
    { availabilityZoneId = Prelude.Nothing,
      azMode = Prelude.Nothing,
      clusterDescription = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      executionRole = Prelude.Nothing,
      initializationScript = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The availability zone identifiers for the requested regions.
kxCluster_availabilityZoneId :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.Text)
kxCluster_availabilityZoneId = Lens.lens (\KxCluster' {availabilityZoneId} -> availabilityZoneId) (\s@KxCluster' {} a -> s {availabilityZoneId = a} :: KxCluster)

-- | The number of availability zones assigned per cluster. This can be one
-- of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
kxCluster_azMode :: Lens.Lens' KxCluster (Prelude.Maybe KxAzMode)
kxCluster_azMode = Lens.lens (\KxCluster' {azMode} -> azMode) (\s@KxCluster' {} a -> s {azMode = a} :: KxCluster)

-- | A description of the cluster.
kxCluster_clusterDescription :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.Text)
kxCluster_clusterDescription = Lens.lens (\KxCluster' {clusterDescription} -> clusterDescription) (\s@KxCluster' {} a -> s {clusterDescription = a} :: KxCluster)

-- | A unique name for the cluster.
kxCluster_clusterName :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.Text)
kxCluster_clusterName = Lens.lens (\KxCluster' {clusterName} -> clusterName) (\s@KxCluster' {} a -> s {clusterName = a} :: KxCluster)

-- | Specifies the type of KDB database that is being created. The following
-- types are available:
--
-- -   HDB – A Historical Database. The data is only accessible with
--     read-only permissions from one of the FinSpace managed kdb databases
--     mounted to the cluster.
--
-- -   RDB – A Realtime Database. This type of database captures all the
--     data from a ticker plant and stores it in memory until the end of
--     day, after which it writes all of its data to a disk and reloads the
--     HDB. This cluster type requires local storage for temporary storage
--     of data during the savedown process. If you specify this field in
--     your request, you must provide the @savedownStorageConfiguration@
--     parameter.
--
-- -   GATEWAY – A gateway cluster allows you to access data across
--     processes in kdb systems. It allows you to create your own routing
--     logic using the initialization scripts and custom code. This type of
--     cluster does not require a writable local storage.
kxCluster_clusterType :: Lens.Lens' KxCluster (Prelude.Maybe KxClusterType)
kxCluster_clusterType = Lens.lens (\KxCluster' {clusterType} -> clusterType) (\s@KxCluster' {} a -> s {clusterType = a} :: KxCluster)

-- | The timestamp at which the cluster was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxCluster_createdTimestamp :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.UTCTime)
kxCluster_createdTimestamp = Lens.lens (\KxCluster' {createdTimestamp} -> createdTimestamp) (\s@KxCluster' {} a -> s {createdTimestamp = a} :: KxCluster) Prelude.. Lens.mapping Data._Time

-- | An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
kxCluster_executionRole :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.Text)
kxCluster_executionRole = Lens.lens (\KxCluster' {executionRole} -> executionRole) (\s@KxCluster' {} a -> s {executionRole = a} :: KxCluster)

-- | Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
kxCluster_initializationScript :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.Text)
kxCluster_initializationScript = Lens.lens (\KxCluster' {initializationScript} -> initializationScript) (\s@KxCluster' {} a -> s {initializationScript = a} :: KxCluster)

-- | The last time that the cluster was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxCluster_lastModifiedTimestamp :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.UTCTime)
kxCluster_lastModifiedTimestamp = Lens.lens (\KxCluster' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@KxCluster' {} a -> s {lastModifiedTimestamp = a} :: KxCluster) Prelude.. Lens.mapping Data._Time

-- | A version of the FinSpace managed kdb to run.
kxCluster_releaseLabel :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.Text)
kxCluster_releaseLabel = Lens.lens (\KxCluster' {releaseLabel} -> releaseLabel) (\s@KxCluster' {} a -> s {releaseLabel = a} :: KxCluster)

-- | The status of a cluster.
--
-- -   PENDING – The cluster is pending creation.
--
-- -   CREATING –The cluster creation process is in progress.
--
-- -   CREATE_FAILED– The cluster creation process has failed.
--
-- -   RUNNING – The cluster creation process is running.
--
-- -   UPDATING – The cluster is in the process of being updated.
--
-- -   DELETING – The cluster is in the process of being deleted.
--
-- -   DELETED – The cluster has been deleted.
--
-- -   DELETE_FAILED – The cluster failed to delete.
kxCluster_status :: Lens.Lens' KxCluster (Prelude.Maybe KxClusterStatus)
kxCluster_status = Lens.lens (\KxCluster' {status} -> status) (\s@KxCluster' {} a -> s {status = a} :: KxCluster)

-- | The error message when a failed state occurs.
kxCluster_statusReason :: Lens.Lens' KxCluster (Prelude.Maybe Prelude.Text)
kxCluster_statusReason = Lens.lens (\KxCluster' {statusReason} -> statusReason) (\s@KxCluster' {} a -> s {statusReason = a} :: KxCluster)

instance Data.FromJSON KxCluster where
  parseJSON =
    Data.withObject
      "KxCluster"
      ( \x ->
          KxCluster'
            Prelude.<$> (x Data..:? "availabilityZoneId")
            Prelude.<*> (x Data..:? "azMode")
            Prelude.<*> (x Data..:? "clusterDescription")
            Prelude.<*> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "clusterType")
            Prelude.<*> (x Data..:? "createdTimestamp")
            Prelude.<*> (x Data..:? "executionRole")
            Prelude.<*> (x Data..:? "initializationScript")
            Prelude.<*> (x Data..:? "lastModifiedTimestamp")
            Prelude.<*> (x Data..:? "releaseLabel")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusReason")
      )

instance Prelude.Hashable KxCluster where
  hashWithSalt _salt KxCluster' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` azMode
      `Prelude.hashWithSalt` clusterDescription
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` clusterType
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` initializationScript
      `Prelude.hashWithSalt` lastModifiedTimestamp
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData KxCluster where
  rnf KxCluster' {..} =
    Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf azMode
      `Prelude.seq` Prelude.rnf clusterDescription
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf initializationScript
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
