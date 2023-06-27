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
-- Module      : Amazonka.FinSpace.GetKxCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a kdb cluster.
module Amazonka.FinSpace.GetKxCluster
  ( -- * Creating a Request
    GetKxCluster (..),
    newGetKxCluster,

    -- * Request Lenses
    getKxCluster_environmentId,
    getKxCluster_clusterName,

    -- * Destructuring the Response
    GetKxClusterResponse (..),
    newGetKxClusterResponse,

    -- * Response Lenses
    getKxClusterResponse_autoScalingConfiguration,
    getKxClusterResponse_availabilityZoneId,
    getKxClusterResponse_azMode,
    getKxClusterResponse_cacheStorageConfigurations,
    getKxClusterResponse_capacityConfiguration,
    getKxClusterResponse_clusterDescription,
    getKxClusterResponse_clusterName,
    getKxClusterResponse_clusterType,
    getKxClusterResponse_code,
    getKxClusterResponse_commandLineArguments,
    getKxClusterResponse_createdTimestamp,
    getKxClusterResponse_databases,
    getKxClusterResponse_executionRole,
    getKxClusterResponse_initializationScript,
    getKxClusterResponse_lastModifiedTimestamp,
    getKxClusterResponse_releaseLabel,
    getKxClusterResponse_savedownStorageConfiguration,
    getKxClusterResponse_status,
    getKxClusterResponse_statusReason,
    getKxClusterResponse_vpcConfiguration,
    getKxClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKxCluster' smart constructor.
data GetKxCluster = GetKxCluster'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the cluster that you want to retrieve.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'getKxCluster_environmentId' - A unique identifier for the kdb environment.
--
-- 'clusterName', 'getKxCluster_clusterName' - The name of the cluster that you want to retrieve.
newGetKxCluster ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  GetKxCluster
newGetKxCluster pEnvironmentId_ pClusterName_ =
  GetKxCluster'
    { environmentId = pEnvironmentId_,
      clusterName = pClusterName_
    }

-- | A unique identifier for the kdb environment.
getKxCluster_environmentId :: Lens.Lens' GetKxCluster Prelude.Text
getKxCluster_environmentId = Lens.lens (\GetKxCluster' {environmentId} -> environmentId) (\s@GetKxCluster' {} a -> s {environmentId = a} :: GetKxCluster)

-- | The name of the cluster that you want to retrieve.
getKxCluster_clusterName :: Lens.Lens' GetKxCluster Prelude.Text
getKxCluster_clusterName = Lens.lens (\GetKxCluster' {clusterName} -> clusterName) (\s@GetKxCluster' {} a -> s {clusterName = a} :: GetKxCluster)

instance Core.AWSRequest GetKxCluster where
  type AWSResponse GetKxCluster = GetKxClusterResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKxClusterResponse'
            Prelude.<$> (x Data..?> "autoScalingConfiguration")
            Prelude.<*> (x Data..?> "availabilityZoneId")
            Prelude.<*> (x Data..?> "azMode")
            Prelude.<*> ( x
                            Data..?> "cacheStorageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "capacityConfiguration")
            Prelude.<*> (x Data..?> "clusterDescription")
            Prelude.<*> (x Data..?> "clusterName")
            Prelude.<*> (x Data..?> "clusterType")
            Prelude.<*> (x Data..?> "code")
            Prelude.<*> ( x
                            Data..?> "commandLineArguments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "createdTimestamp")
            Prelude.<*> (x Data..?> "databases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "executionRole")
            Prelude.<*> (x Data..?> "initializationScript")
            Prelude.<*> (x Data..?> "lastModifiedTimestamp")
            Prelude.<*> (x Data..?> "releaseLabel")
            Prelude.<*> (x Data..?> "savedownStorageConfiguration")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusReason")
            Prelude.<*> (x Data..?> "vpcConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKxCluster where
  hashWithSalt _salt GetKxCluster' {..} =
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData GetKxCluster where
  rnf GetKxCluster' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders GetKxCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetKxCluster where
  toPath GetKxCluster' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/clusters/",
        Data.toBS clusterName
      ]

instance Data.ToQuery GetKxCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKxClusterResponse' smart constructor.
data GetKxClusterResponse = GetKxClusterResponse'
  { -- | The configuration based on which FinSpace will scale in or scale out
    -- nodes in your cluster.
    autoScalingConfiguration :: Prelude.Maybe AutoScalingConfiguration,
    -- | The availability zone identifiers for the requested regions.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The number of availability zones you want to assign per cluster. This
    -- can be one of the following
    --
    -- -   @SINGLE@ – Assigns one availability zone per cluster.
    --
    -- -   @MULTI@ – Assigns all the availability zones per cluster.
    azMode :: Prelude.Maybe KxAzMode,
    -- | The configurations for a read only cache storage associated with a
    -- cluster. This cache will be stored as an FSx Lustre that reads from the
    -- S3 store.
    cacheStorageConfigurations :: Prelude.Maybe [KxCacheStorageConfiguration],
    -- | A structure for the metadata of a cluster. It includes information like
    -- the CPUs needed, memory of instances, number of instances, and the port
    -- used while establishing a connection.
    capacityConfiguration :: Prelude.Maybe CapacityConfiguration,
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
    -- | The details of the custom code that you want to use inside a cluster
    -- when analyzing a data. It consists of the S3 source bucket, location, S3
    -- object version, and the relative path from where the custom code is
    -- loaded into the cluster.
    code :: Prelude.Maybe CodeConfiguration,
    -- | Defines key-value pairs to make them available inside the cluster.
    commandLineArguments :: Prelude.Maybe [KxCommandLineArgument],
    -- | The timestamp at which the cluster was created in FinSpace. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of databases mounted on the cluster.
    databases :: Prelude.Maybe [KxDatabaseConfiguration],
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
    -- | The version of FinSpace managed kdb to run.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The size and type of the temporary storage that is used to hold data
    -- during the savedown process. This parameter is required when you choose
    -- @clusterType@ as RDB. All the data written to this storage space is lost
    -- when the cluster node is restarted.
    savedownStorageConfiguration :: Prelude.Maybe KxSavedownStorageConfiguration,
    -- | The status of cluster creation.
    --
    -- -   PENDING – The cluster is pending creation.
    --
    -- -   CREATING – The cluster creation process is in progress.
    --
    -- -   CREATE_FAILED – The cluster creation process has failed.
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
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Configuration details about the network where the Privatelink endpoint
    -- of the cluster resides.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingConfiguration', 'getKxClusterResponse_autoScalingConfiguration' - The configuration based on which FinSpace will scale in or scale out
-- nodes in your cluster.
--
-- 'availabilityZoneId', 'getKxClusterResponse_availabilityZoneId' - The availability zone identifiers for the requested regions.
--
-- 'azMode', 'getKxClusterResponse_azMode' - The number of availability zones you want to assign per cluster. This
-- can be one of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
--
-- 'cacheStorageConfigurations', 'getKxClusterResponse_cacheStorageConfigurations' - The configurations for a read only cache storage associated with a
-- cluster. This cache will be stored as an FSx Lustre that reads from the
-- S3 store.
--
-- 'capacityConfiguration', 'getKxClusterResponse_capacityConfiguration' - A structure for the metadata of a cluster. It includes information like
-- the CPUs needed, memory of instances, number of instances, and the port
-- used while establishing a connection.
--
-- 'clusterDescription', 'getKxClusterResponse_clusterDescription' - A description of the cluster.
--
-- 'clusterName', 'getKxClusterResponse_clusterName' - A unique name for the cluster.
--
-- 'clusterType', 'getKxClusterResponse_clusterType' - Specifies the type of KDB database that is being created. The following
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
-- 'code', 'getKxClusterResponse_code' - The details of the custom code that you want to use inside a cluster
-- when analyzing a data. It consists of the S3 source bucket, location, S3
-- object version, and the relative path from where the custom code is
-- loaded into the cluster.
--
-- 'commandLineArguments', 'getKxClusterResponse_commandLineArguments' - Defines key-value pairs to make them available inside the cluster.
--
-- 'createdTimestamp', 'getKxClusterResponse_createdTimestamp' - The timestamp at which the cluster was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'databases', 'getKxClusterResponse_databases' - A list of databases mounted on the cluster.
--
-- 'executionRole', 'getKxClusterResponse_executionRole' - An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
--
-- 'initializationScript', 'getKxClusterResponse_initializationScript' - Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
--
-- 'lastModifiedTimestamp', 'getKxClusterResponse_lastModifiedTimestamp' - The last time that the cluster was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'releaseLabel', 'getKxClusterResponse_releaseLabel' - The version of FinSpace managed kdb to run.
--
-- 'savedownStorageConfiguration', 'getKxClusterResponse_savedownStorageConfiguration' - The size and type of the temporary storage that is used to hold data
-- during the savedown process. This parameter is required when you choose
-- @clusterType@ as RDB. All the data written to this storage space is lost
-- when the cluster node is restarted.
--
-- 'status', 'getKxClusterResponse_status' - The status of cluster creation.
--
-- -   PENDING – The cluster is pending creation.
--
-- -   CREATING – The cluster creation process is in progress.
--
-- -   CREATE_FAILED – The cluster creation process has failed.
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
-- 'statusReason', 'getKxClusterResponse_statusReason' - The error message when a failed state occurs.
--
-- 'vpcConfiguration', 'getKxClusterResponse_vpcConfiguration' - Configuration details about the network where the Privatelink endpoint
-- of the cluster resides.
--
-- 'httpStatus', 'getKxClusterResponse_httpStatus' - The response's http status code.
newGetKxClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKxClusterResponse
newGetKxClusterResponse pHttpStatus_ =
  GetKxClusterResponse'
    { autoScalingConfiguration =
        Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      azMode = Prelude.Nothing,
      cacheStorageConfigurations = Prelude.Nothing,
      capacityConfiguration = Prelude.Nothing,
      clusterDescription = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      code = Prelude.Nothing,
      commandLineArguments = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      databases = Prelude.Nothing,
      executionRole = Prelude.Nothing,
      initializationScript = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      savedownStorageConfiguration = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration based on which FinSpace will scale in or scale out
-- nodes in your cluster.
getKxClusterResponse_autoScalingConfiguration :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe AutoScalingConfiguration)
getKxClusterResponse_autoScalingConfiguration = Lens.lens (\GetKxClusterResponse' {autoScalingConfiguration} -> autoScalingConfiguration) (\s@GetKxClusterResponse' {} a -> s {autoScalingConfiguration = a} :: GetKxClusterResponse)

-- | The availability zone identifiers for the requested regions.
getKxClusterResponse_availabilityZoneId :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.Text)
getKxClusterResponse_availabilityZoneId = Lens.lens (\GetKxClusterResponse' {availabilityZoneId} -> availabilityZoneId) (\s@GetKxClusterResponse' {} a -> s {availabilityZoneId = a} :: GetKxClusterResponse)

-- | The number of availability zones you want to assign per cluster. This
-- can be one of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
getKxClusterResponse_azMode :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe KxAzMode)
getKxClusterResponse_azMode = Lens.lens (\GetKxClusterResponse' {azMode} -> azMode) (\s@GetKxClusterResponse' {} a -> s {azMode = a} :: GetKxClusterResponse)

-- | The configurations for a read only cache storage associated with a
-- cluster. This cache will be stored as an FSx Lustre that reads from the
-- S3 store.
getKxClusterResponse_cacheStorageConfigurations :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe [KxCacheStorageConfiguration])
getKxClusterResponse_cacheStorageConfigurations = Lens.lens (\GetKxClusterResponse' {cacheStorageConfigurations} -> cacheStorageConfigurations) (\s@GetKxClusterResponse' {} a -> s {cacheStorageConfigurations = a} :: GetKxClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | A structure for the metadata of a cluster. It includes information like
-- the CPUs needed, memory of instances, number of instances, and the port
-- used while establishing a connection.
getKxClusterResponse_capacityConfiguration :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe CapacityConfiguration)
getKxClusterResponse_capacityConfiguration = Lens.lens (\GetKxClusterResponse' {capacityConfiguration} -> capacityConfiguration) (\s@GetKxClusterResponse' {} a -> s {capacityConfiguration = a} :: GetKxClusterResponse)

-- | A description of the cluster.
getKxClusterResponse_clusterDescription :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.Text)
getKxClusterResponse_clusterDescription = Lens.lens (\GetKxClusterResponse' {clusterDescription} -> clusterDescription) (\s@GetKxClusterResponse' {} a -> s {clusterDescription = a} :: GetKxClusterResponse)

-- | A unique name for the cluster.
getKxClusterResponse_clusterName :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.Text)
getKxClusterResponse_clusterName = Lens.lens (\GetKxClusterResponse' {clusterName} -> clusterName) (\s@GetKxClusterResponse' {} a -> s {clusterName = a} :: GetKxClusterResponse)

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
getKxClusterResponse_clusterType :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe KxClusterType)
getKxClusterResponse_clusterType = Lens.lens (\GetKxClusterResponse' {clusterType} -> clusterType) (\s@GetKxClusterResponse' {} a -> s {clusterType = a} :: GetKxClusterResponse)

-- | The details of the custom code that you want to use inside a cluster
-- when analyzing a data. It consists of the S3 source bucket, location, S3
-- object version, and the relative path from where the custom code is
-- loaded into the cluster.
getKxClusterResponse_code :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe CodeConfiguration)
getKxClusterResponse_code = Lens.lens (\GetKxClusterResponse' {code} -> code) (\s@GetKxClusterResponse' {} a -> s {code = a} :: GetKxClusterResponse)

-- | Defines key-value pairs to make them available inside the cluster.
getKxClusterResponse_commandLineArguments :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe [KxCommandLineArgument])
getKxClusterResponse_commandLineArguments = Lens.lens (\GetKxClusterResponse' {commandLineArguments} -> commandLineArguments) (\s@GetKxClusterResponse' {} a -> s {commandLineArguments = a} :: GetKxClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp at which the cluster was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getKxClusterResponse_createdTimestamp :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.UTCTime)
getKxClusterResponse_createdTimestamp = Lens.lens (\GetKxClusterResponse' {createdTimestamp} -> createdTimestamp) (\s@GetKxClusterResponse' {} a -> s {createdTimestamp = a} :: GetKxClusterResponse) Prelude.. Lens.mapping Data._Time

-- | A list of databases mounted on the cluster.
getKxClusterResponse_databases :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe [KxDatabaseConfiguration])
getKxClusterResponse_databases = Lens.lens (\GetKxClusterResponse' {databases} -> databases) (\s@GetKxClusterResponse' {} a -> s {databases = a} :: GetKxClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
getKxClusterResponse_executionRole :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.Text)
getKxClusterResponse_executionRole = Lens.lens (\GetKxClusterResponse' {executionRole} -> executionRole) (\s@GetKxClusterResponse' {} a -> s {executionRole = a} :: GetKxClusterResponse)

-- | Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
getKxClusterResponse_initializationScript :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.Text)
getKxClusterResponse_initializationScript = Lens.lens (\GetKxClusterResponse' {initializationScript} -> initializationScript) (\s@GetKxClusterResponse' {} a -> s {initializationScript = a} :: GetKxClusterResponse)

-- | The last time that the cluster was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getKxClusterResponse_lastModifiedTimestamp :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.UTCTime)
getKxClusterResponse_lastModifiedTimestamp = Lens.lens (\GetKxClusterResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@GetKxClusterResponse' {} a -> s {lastModifiedTimestamp = a} :: GetKxClusterResponse) Prelude.. Lens.mapping Data._Time

-- | The version of FinSpace managed kdb to run.
getKxClusterResponse_releaseLabel :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.Text)
getKxClusterResponse_releaseLabel = Lens.lens (\GetKxClusterResponse' {releaseLabel} -> releaseLabel) (\s@GetKxClusterResponse' {} a -> s {releaseLabel = a} :: GetKxClusterResponse)

-- | The size and type of the temporary storage that is used to hold data
-- during the savedown process. This parameter is required when you choose
-- @clusterType@ as RDB. All the data written to this storage space is lost
-- when the cluster node is restarted.
getKxClusterResponse_savedownStorageConfiguration :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe KxSavedownStorageConfiguration)
getKxClusterResponse_savedownStorageConfiguration = Lens.lens (\GetKxClusterResponse' {savedownStorageConfiguration} -> savedownStorageConfiguration) (\s@GetKxClusterResponse' {} a -> s {savedownStorageConfiguration = a} :: GetKxClusterResponse)

-- | The status of cluster creation.
--
-- -   PENDING – The cluster is pending creation.
--
-- -   CREATING – The cluster creation process is in progress.
--
-- -   CREATE_FAILED – The cluster creation process has failed.
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
getKxClusterResponse_status :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe KxClusterStatus)
getKxClusterResponse_status = Lens.lens (\GetKxClusterResponse' {status} -> status) (\s@GetKxClusterResponse' {} a -> s {status = a} :: GetKxClusterResponse)

-- | The error message when a failed state occurs.
getKxClusterResponse_statusReason :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe Prelude.Text)
getKxClusterResponse_statusReason = Lens.lens (\GetKxClusterResponse' {statusReason} -> statusReason) (\s@GetKxClusterResponse' {} a -> s {statusReason = a} :: GetKxClusterResponse)

-- | Configuration details about the network where the Privatelink endpoint
-- of the cluster resides.
getKxClusterResponse_vpcConfiguration :: Lens.Lens' GetKxClusterResponse (Prelude.Maybe VpcConfiguration)
getKxClusterResponse_vpcConfiguration = Lens.lens (\GetKxClusterResponse' {vpcConfiguration} -> vpcConfiguration) (\s@GetKxClusterResponse' {} a -> s {vpcConfiguration = a} :: GetKxClusterResponse)

-- | The response's http status code.
getKxClusterResponse_httpStatus :: Lens.Lens' GetKxClusterResponse Prelude.Int
getKxClusterResponse_httpStatus = Lens.lens (\GetKxClusterResponse' {httpStatus} -> httpStatus) (\s@GetKxClusterResponse' {} a -> s {httpStatus = a} :: GetKxClusterResponse)

instance Prelude.NFData GetKxClusterResponse where
  rnf GetKxClusterResponse' {..} =
    Prelude.rnf autoScalingConfiguration
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf azMode
      `Prelude.seq` Prelude.rnf cacheStorageConfigurations
      `Prelude.seq` Prelude.rnf capacityConfiguration
      `Prelude.seq` Prelude.rnf clusterDescription
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf commandLineArguments
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf databases
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf initializationScript
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf
        savedownStorageConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
