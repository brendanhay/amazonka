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
-- Module      : Amazonka.FinSpace.CreateKxCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new kdb cluster.
module Amazonka.FinSpace.CreateKxCluster
  ( -- * Creating a Request
    CreateKxCluster (..),
    newCreateKxCluster,

    -- * Request Lenses
    createKxCluster_autoScalingConfiguration,
    createKxCluster_availabilityZoneId,
    createKxCluster_cacheStorageConfigurations,
    createKxCluster_clientToken,
    createKxCluster_clusterDescription,
    createKxCluster_code,
    createKxCluster_commandLineArguments,
    createKxCluster_databases,
    createKxCluster_executionRole,
    createKxCluster_initializationScript,
    createKxCluster_savedownStorageConfiguration,
    createKxCluster_tags,
    createKxCluster_vpcConfiguration,
    createKxCluster_environmentId,
    createKxCluster_clusterName,
    createKxCluster_clusterType,
    createKxCluster_capacityConfiguration,
    createKxCluster_releaseLabel,
    createKxCluster_azMode,

    -- * Destructuring the Response
    CreateKxClusterResponse (..),
    newCreateKxClusterResponse,

    -- * Response Lenses
    createKxClusterResponse_autoScalingConfiguration,
    createKxClusterResponse_availabilityZoneId,
    createKxClusterResponse_azMode,
    createKxClusterResponse_cacheStorageConfigurations,
    createKxClusterResponse_capacityConfiguration,
    createKxClusterResponse_clusterDescription,
    createKxClusterResponse_clusterName,
    createKxClusterResponse_clusterType,
    createKxClusterResponse_code,
    createKxClusterResponse_commandLineArguments,
    createKxClusterResponse_createdTimestamp,
    createKxClusterResponse_databases,
    createKxClusterResponse_environmentId,
    createKxClusterResponse_executionRole,
    createKxClusterResponse_initializationScript,
    createKxClusterResponse_lastModifiedTimestamp,
    createKxClusterResponse_releaseLabel,
    createKxClusterResponse_savedownStorageConfiguration,
    createKxClusterResponse_status,
    createKxClusterResponse_statusReason,
    createKxClusterResponse_vpcConfiguration,
    createKxClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKxCluster' smart constructor.
data CreateKxCluster = CreateKxCluster'
  { -- | The configuration based on which FinSpace will scale in or scale out
    -- nodes in your cluster.
    autoScalingConfiguration :: Prelude.Maybe AutoScalingConfiguration,
    -- | The availability zone identifiers for the requested regions.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The configurations for a read only cache storage associated with a
    -- cluster. This cache will be stored as an FSx Lustre that reads from the
    -- S3 store.
    cacheStorageConfigurations :: Prelude.Maybe [KxCacheStorageConfiguration],
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the cluster.
    clusterDescription :: Prelude.Maybe Prelude.Text,
    -- | The details of the custom code that you want to use inside a cluster
    -- when analyzing a data. It consists of the S3 source bucket, location, S3
    -- object version, and the relative path from where the custom code is
    -- loaded into the cluster.
    code :: Prelude.Maybe CodeConfiguration,
    -- | Defines the key-value pairs to make them available inside the cluster.
    commandLineArguments :: Prelude.Maybe [KxCommandLineArgument],
    -- | A list of databases that will be available for querying.
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
    -- | The size and type of the temporary storage that is used to hold data
    -- during the savedown process. This parameter is required when you choose
    -- @clusterType@ as RDB. All the data written to this storage space is lost
    -- when the cluster node is restarted.
    savedownStorageConfiguration :: Prelude.Maybe KxSavedownStorageConfiguration,
    -- | A list of key-value pairs to label the cluster. You can add up to 50
    -- tags to a cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration details about the network where the Privatelink endpoint
    -- of the cluster resides.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | A unique name for the cluster that you want to create.
    clusterName :: Prelude.Text,
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
    clusterType :: KxClusterType,
    -- | A structure for the metadata of a cluster. It includes information about
    -- like the CPUs needed, memory of instances, number of instances, and the
    -- port used while establishing a connection.
    capacityConfiguration :: CapacityConfiguration,
    -- | The version of FinSpace managed kdb to run.
    releaseLabel :: Prelude.Text,
    -- | The number of availability zones you want to assign per cluster. This
    -- can be one of the following
    --
    -- -   @SINGLE@ – Assigns one availability zone per cluster.
    --
    -- -   @MULTI@ – Assigns all the availability zones per cluster.
    azMode :: KxAzMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingConfiguration', 'createKxCluster_autoScalingConfiguration' - The configuration based on which FinSpace will scale in or scale out
-- nodes in your cluster.
--
-- 'availabilityZoneId', 'createKxCluster_availabilityZoneId' - The availability zone identifiers for the requested regions.
--
-- 'cacheStorageConfigurations', 'createKxCluster_cacheStorageConfigurations' - The configurations for a read only cache storage associated with a
-- cluster. This cache will be stored as an FSx Lustre that reads from the
-- S3 store.
--
-- 'clientToken', 'createKxCluster_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'clusterDescription', 'createKxCluster_clusterDescription' - A description of the cluster.
--
-- 'code', 'createKxCluster_code' - The details of the custom code that you want to use inside a cluster
-- when analyzing a data. It consists of the S3 source bucket, location, S3
-- object version, and the relative path from where the custom code is
-- loaded into the cluster.
--
-- 'commandLineArguments', 'createKxCluster_commandLineArguments' - Defines the key-value pairs to make them available inside the cluster.
--
-- 'databases', 'createKxCluster_databases' - A list of databases that will be available for querying.
--
-- 'executionRole', 'createKxCluster_executionRole' - An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
--
-- 'initializationScript', 'createKxCluster_initializationScript' - Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
--
-- 'savedownStorageConfiguration', 'createKxCluster_savedownStorageConfiguration' - The size and type of the temporary storage that is used to hold data
-- during the savedown process. This parameter is required when you choose
-- @clusterType@ as RDB. All the data written to this storage space is lost
-- when the cluster node is restarted.
--
-- 'tags', 'createKxCluster_tags' - A list of key-value pairs to label the cluster. You can add up to 50
-- tags to a cluster.
--
-- 'vpcConfiguration', 'createKxCluster_vpcConfiguration' - Configuration details about the network where the Privatelink endpoint
-- of the cluster resides.
--
-- 'environmentId', 'createKxCluster_environmentId' - A unique identifier for the kdb environment.
--
-- 'clusterName', 'createKxCluster_clusterName' - A unique name for the cluster that you want to create.
--
-- 'clusterType', 'createKxCluster_clusterType' - Specifies the type of KDB database that is being created. The following
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
-- 'capacityConfiguration', 'createKxCluster_capacityConfiguration' - A structure for the metadata of a cluster. It includes information about
-- like the CPUs needed, memory of instances, number of instances, and the
-- port used while establishing a connection.
--
-- 'releaseLabel', 'createKxCluster_releaseLabel' - The version of FinSpace managed kdb to run.
--
-- 'azMode', 'createKxCluster_azMode' - The number of availability zones you want to assign per cluster. This
-- can be one of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
newCreateKxCluster ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'clusterType'
  KxClusterType ->
  -- | 'capacityConfiguration'
  CapacityConfiguration ->
  -- | 'releaseLabel'
  Prelude.Text ->
  -- | 'azMode'
  KxAzMode ->
  CreateKxCluster
newCreateKxCluster
  pEnvironmentId_
  pClusterName_
  pClusterType_
  pCapacityConfiguration_
  pReleaseLabel_
  pAzMode_ =
    CreateKxCluster'
      { autoScalingConfiguration =
          Prelude.Nothing,
        availabilityZoneId = Prelude.Nothing,
        cacheStorageConfigurations = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        clusterDescription = Prelude.Nothing,
        code = Prelude.Nothing,
        commandLineArguments = Prelude.Nothing,
        databases = Prelude.Nothing,
        executionRole = Prelude.Nothing,
        initializationScript = Prelude.Nothing,
        savedownStorageConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        environmentId = pEnvironmentId_,
        clusterName = pClusterName_,
        clusterType = pClusterType_,
        capacityConfiguration = pCapacityConfiguration_,
        releaseLabel = pReleaseLabel_,
        azMode = pAzMode_
      }

-- | The configuration based on which FinSpace will scale in or scale out
-- nodes in your cluster.
createKxCluster_autoScalingConfiguration :: Lens.Lens' CreateKxCluster (Prelude.Maybe AutoScalingConfiguration)
createKxCluster_autoScalingConfiguration = Lens.lens (\CreateKxCluster' {autoScalingConfiguration} -> autoScalingConfiguration) (\s@CreateKxCluster' {} a -> s {autoScalingConfiguration = a} :: CreateKxCluster)

-- | The availability zone identifiers for the requested regions.
createKxCluster_availabilityZoneId :: Lens.Lens' CreateKxCluster (Prelude.Maybe Prelude.Text)
createKxCluster_availabilityZoneId = Lens.lens (\CreateKxCluster' {availabilityZoneId} -> availabilityZoneId) (\s@CreateKxCluster' {} a -> s {availabilityZoneId = a} :: CreateKxCluster)

-- | The configurations for a read only cache storage associated with a
-- cluster. This cache will be stored as an FSx Lustre that reads from the
-- S3 store.
createKxCluster_cacheStorageConfigurations :: Lens.Lens' CreateKxCluster (Prelude.Maybe [KxCacheStorageConfiguration])
createKxCluster_cacheStorageConfigurations = Lens.lens (\CreateKxCluster' {cacheStorageConfigurations} -> cacheStorageConfigurations) (\s@CreateKxCluster' {} a -> s {cacheStorageConfigurations = a} :: CreateKxCluster) Prelude.. Lens.mapping Lens.coerced

-- | A token that ensures idempotency. This token expires in 10 minutes.
createKxCluster_clientToken :: Lens.Lens' CreateKxCluster (Prelude.Maybe Prelude.Text)
createKxCluster_clientToken = Lens.lens (\CreateKxCluster' {clientToken} -> clientToken) (\s@CreateKxCluster' {} a -> s {clientToken = a} :: CreateKxCluster)

-- | A description of the cluster.
createKxCluster_clusterDescription :: Lens.Lens' CreateKxCluster (Prelude.Maybe Prelude.Text)
createKxCluster_clusterDescription = Lens.lens (\CreateKxCluster' {clusterDescription} -> clusterDescription) (\s@CreateKxCluster' {} a -> s {clusterDescription = a} :: CreateKxCluster)

-- | The details of the custom code that you want to use inside a cluster
-- when analyzing a data. It consists of the S3 source bucket, location, S3
-- object version, and the relative path from where the custom code is
-- loaded into the cluster.
createKxCluster_code :: Lens.Lens' CreateKxCluster (Prelude.Maybe CodeConfiguration)
createKxCluster_code = Lens.lens (\CreateKxCluster' {code} -> code) (\s@CreateKxCluster' {} a -> s {code = a} :: CreateKxCluster)

-- | Defines the key-value pairs to make them available inside the cluster.
createKxCluster_commandLineArguments :: Lens.Lens' CreateKxCluster (Prelude.Maybe [KxCommandLineArgument])
createKxCluster_commandLineArguments = Lens.lens (\CreateKxCluster' {commandLineArguments} -> commandLineArguments) (\s@CreateKxCluster' {} a -> s {commandLineArguments = a} :: CreateKxCluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of databases that will be available for querying.
createKxCluster_databases :: Lens.Lens' CreateKxCluster (Prelude.Maybe [KxDatabaseConfiguration])
createKxCluster_databases = Lens.lens (\CreateKxCluster' {databases} -> databases) (\s@CreateKxCluster' {} a -> s {databases = a} :: CreateKxCluster) Prelude.. Lens.mapping Lens.coerced

-- | An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
createKxCluster_executionRole :: Lens.Lens' CreateKxCluster (Prelude.Maybe Prelude.Text)
createKxCluster_executionRole = Lens.lens (\CreateKxCluster' {executionRole} -> executionRole) (\s@CreateKxCluster' {} a -> s {executionRole = a} :: CreateKxCluster)

-- | Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
createKxCluster_initializationScript :: Lens.Lens' CreateKxCluster (Prelude.Maybe Prelude.Text)
createKxCluster_initializationScript = Lens.lens (\CreateKxCluster' {initializationScript} -> initializationScript) (\s@CreateKxCluster' {} a -> s {initializationScript = a} :: CreateKxCluster)

-- | The size and type of the temporary storage that is used to hold data
-- during the savedown process. This parameter is required when you choose
-- @clusterType@ as RDB. All the data written to this storage space is lost
-- when the cluster node is restarted.
createKxCluster_savedownStorageConfiguration :: Lens.Lens' CreateKxCluster (Prelude.Maybe KxSavedownStorageConfiguration)
createKxCluster_savedownStorageConfiguration = Lens.lens (\CreateKxCluster' {savedownStorageConfiguration} -> savedownStorageConfiguration) (\s@CreateKxCluster' {} a -> s {savedownStorageConfiguration = a} :: CreateKxCluster)

-- | A list of key-value pairs to label the cluster. You can add up to 50
-- tags to a cluster.
createKxCluster_tags :: Lens.Lens' CreateKxCluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createKxCluster_tags = Lens.lens (\CreateKxCluster' {tags} -> tags) (\s@CreateKxCluster' {} a -> s {tags = a} :: CreateKxCluster) Prelude.. Lens.mapping Lens.coerced

-- | Configuration details about the network where the Privatelink endpoint
-- of the cluster resides.
createKxCluster_vpcConfiguration :: Lens.Lens' CreateKxCluster (Prelude.Maybe VpcConfiguration)
createKxCluster_vpcConfiguration = Lens.lens (\CreateKxCluster' {vpcConfiguration} -> vpcConfiguration) (\s@CreateKxCluster' {} a -> s {vpcConfiguration = a} :: CreateKxCluster)

-- | A unique identifier for the kdb environment.
createKxCluster_environmentId :: Lens.Lens' CreateKxCluster Prelude.Text
createKxCluster_environmentId = Lens.lens (\CreateKxCluster' {environmentId} -> environmentId) (\s@CreateKxCluster' {} a -> s {environmentId = a} :: CreateKxCluster)

-- | A unique name for the cluster that you want to create.
createKxCluster_clusterName :: Lens.Lens' CreateKxCluster Prelude.Text
createKxCluster_clusterName = Lens.lens (\CreateKxCluster' {clusterName} -> clusterName) (\s@CreateKxCluster' {} a -> s {clusterName = a} :: CreateKxCluster)

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
createKxCluster_clusterType :: Lens.Lens' CreateKxCluster KxClusterType
createKxCluster_clusterType = Lens.lens (\CreateKxCluster' {clusterType} -> clusterType) (\s@CreateKxCluster' {} a -> s {clusterType = a} :: CreateKxCluster)

-- | A structure for the metadata of a cluster. It includes information about
-- like the CPUs needed, memory of instances, number of instances, and the
-- port used while establishing a connection.
createKxCluster_capacityConfiguration :: Lens.Lens' CreateKxCluster CapacityConfiguration
createKxCluster_capacityConfiguration = Lens.lens (\CreateKxCluster' {capacityConfiguration} -> capacityConfiguration) (\s@CreateKxCluster' {} a -> s {capacityConfiguration = a} :: CreateKxCluster)

-- | The version of FinSpace managed kdb to run.
createKxCluster_releaseLabel :: Lens.Lens' CreateKxCluster Prelude.Text
createKxCluster_releaseLabel = Lens.lens (\CreateKxCluster' {releaseLabel} -> releaseLabel) (\s@CreateKxCluster' {} a -> s {releaseLabel = a} :: CreateKxCluster)

-- | The number of availability zones you want to assign per cluster. This
-- can be one of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
createKxCluster_azMode :: Lens.Lens' CreateKxCluster KxAzMode
createKxCluster_azMode = Lens.lens (\CreateKxCluster' {azMode} -> azMode) (\s@CreateKxCluster' {} a -> s {azMode = a} :: CreateKxCluster)

instance Core.AWSRequest CreateKxCluster where
  type
    AWSResponse CreateKxCluster =
      CreateKxClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKxClusterResponse'
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
            Prelude.<*> (x Data..?> "environmentId")
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

instance Prelude.Hashable CreateKxCluster where
  hashWithSalt _salt CreateKxCluster' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingConfiguration
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` cacheStorageConfigurations
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` clusterDescription
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` commandLineArguments
      `Prelude.hashWithSalt` databases
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` initializationScript
      `Prelude.hashWithSalt` savedownStorageConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` clusterType
      `Prelude.hashWithSalt` capacityConfiguration
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` azMode

instance Prelude.NFData CreateKxCluster where
  rnf CreateKxCluster' {..} =
    Prelude.rnf autoScalingConfiguration
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf cacheStorageConfigurations
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf clusterDescription
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf commandLineArguments
      `Prelude.seq` Prelude.rnf databases
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf initializationScript
      `Prelude.seq` Prelude.rnf savedownStorageConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf capacityConfiguration
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf azMode

instance Data.ToHeaders CreateKxCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKxCluster where
  toJSON CreateKxCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoScalingConfiguration" Data..=)
              Prelude.<$> autoScalingConfiguration,
            ("availabilityZoneId" Data..=)
              Prelude.<$> availabilityZoneId,
            ("cacheStorageConfigurations" Data..=)
              Prelude.<$> cacheStorageConfigurations,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("clusterDescription" Data..=)
              Prelude.<$> clusterDescription,
            ("code" Data..=) Prelude.<$> code,
            ("commandLineArguments" Data..=)
              Prelude.<$> commandLineArguments,
            ("databases" Data..=) Prelude.<$> databases,
            ("executionRole" Data..=) Prelude.<$> executionRole,
            ("initializationScript" Data..=)
              Prelude.<$> initializationScript,
            ("savedownStorageConfiguration" Data..=)
              Prelude.<$> savedownStorageConfiguration,
            ("tags" Data..=) Prelude.<$> tags,
            ("vpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            Prelude.Just ("clusterName" Data..= clusterName),
            Prelude.Just ("clusterType" Data..= clusterType),
            Prelude.Just
              ( "capacityConfiguration"
                  Data..= capacityConfiguration
              ),
            Prelude.Just ("releaseLabel" Data..= releaseLabel),
            Prelude.Just ("azMode" Data..= azMode)
          ]
      )

instance Data.ToPath CreateKxCluster where
  toPath CreateKxCluster' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/clusters"
      ]

instance Data.ToQuery CreateKxCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKxClusterResponse' smart constructor.
data CreateKxClusterResponse = CreateKxClusterResponse'
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
    -- | Defines the key-value pairs to make them available inside the cluster.
    commandLineArguments :: Prelude.Maybe [KxCommandLineArgument],
    -- | The timestamp at which the cluster was created in FinSpace. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of databases that will be available for querying.
    databases :: Prelude.Maybe [KxDatabaseConfiguration],
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'CreateKxClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingConfiguration', 'createKxClusterResponse_autoScalingConfiguration' - The configuration based on which FinSpace will scale in or scale out
-- nodes in your cluster.
--
-- 'availabilityZoneId', 'createKxClusterResponse_availabilityZoneId' - The availability zone identifiers for the requested regions.
--
-- 'azMode', 'createKxClusterResponse_azMode' - The number of availability zones you want to assign per cluster. This
-- can be one of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
--
-- 'cacheStorageConfigurations', 'createKxClusterResponse_cacheStorageConfigurations' - The configurations for a read only cache storage associated with a
-- cluster. This cache will be stored as an FSx Lustre that reads from the
-- S3 store.
--
-- 'capacityConfiguration', 'createKxClusterResponse_capacityConfiguration' - A structure for the metadata of a cluster. It includes information like
-- the CPUs needed, memory of instances, number of instances, and the port
-- used while establishing a connection.
--
-- 'clusterDescription', 'createKxClusterResponse_clusterDescription' - A description of the cluster.
--
-- 'clusterName', 'createKxClusterResponse_clusterName' - A unique name for the cluster.
--
-- 'clusterType', 'createKxClusterResponse_clusterType' - Specifies the type of KDB database that is being created. The following
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
-- 'code', 'createKxClusterResponse_code' - The details of the custom code that you want to use inside a cluster
-- when analyzing a data. It consists of the S3 source bucket, location, S3
-- object version, and the relative path from where the custom code is
-- loaded into the cluster.
--
-- 'commandLineArguments', 'createKxClusterResponse_commandLineArguments' - Defines the key-value pairs to make them available inside the cluster.
--
-- 'createdTimestamp', 'createKxClusterResponse_createdTimestamp' - The timestamp at which the cluster was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'databases', 'createKxClusterResponse_databases' - A list of databases that will be available for querying.
--
-- 'environmentId', 'createKxClusterResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'executionRole', 'createKxClusterResponse_executionRole' - An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
--
-- 'initializationScript', 'createKxClusterResponse_initializationScript' - Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
--
-- 'lastModifiedTimestamp', 'createKxClusterResponse_lastModifiedTimestamp' - The last time that the cluster was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'releaseLabel', 'createKxClusterResponse_releaseLabel' - A version of the FinSpace managed kdb to run.
--
-- 'savedownStorageConfiguration', 'createKxClusterResponse_savedownStorageConfiguration' - The size and type of the temporary storage that is used to hold data
-- during the savedown process. This parameter is required when you choose
-- @clusterType@ as RDB. All the data written to this storage space is lost
-- when the cluster node is restarted.
--
-- 'status', 'createKxClusterResponse_status' - The status of cluster creation.
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
-- 'statusReason', 'createKxClusterResponse_statusReason' - The error message when a failed state occurs.
--
-- 'vpcConfiguration', 'createKxClusterResponse_vpcConfiguration' - Configuration details about the network where the Privatelink endpoint
-- of the cluster resides.
--
-- 'httpStatus', 'createKxClusterResponse_httpStatus' - The response's http status code.
newCreateKxClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKxClusterResponse
newCreateKxClusterResponse pHttpStatus_ =
  CreateKxClusterResponse'
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
      environmentId = Prelude.Nothing,
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
createKxClusterResponse_autoScalingConfiguration :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe AutoScalingConfiguration)
createKxClusterResponse_autoScalingConfiguration = Lens.lens (\CreateKxClusterResponse' {autoScalingConfiguration} -> autoScalingConfiguration) (\s@CreateKxClusterResponse' {} a -> s {autoScalingConfiguration = a} :: CreateKxClusterResponse)

-- | The availability zone identifiers for the requested regions.
createKxClusterResponse_availabilityZoneId :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_availabilityZoneId = Lens.lens (\CreateKxClusterResponse' {availabilityZoneId} -> availabilityZoneId) (\s@CreateKxClusterResponse' {} a -> s {availabilityZoneId = a} :: CreateKxClusterResponse)

-- | The number of availability zones you want to assign per cluster. This
-- can be one of the following
--
-- -   @SINGLE@ – Assigns one availability zone per cluster.
--
-- -   @MULTI@ – Assigns all the availability zones per cluster.
createKxClusterResponse_azMode :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe KxAzMode)
createKxClusterResponse_azMode = Lens.lens (\CreateKxClusterResponse' {azMode} -> azMode) (\s@CreateKxClusterResponse' {} a -> s {azMode = a} :: CreateKxClusterResponse)

-- | The configurations for a read only cache storage associated with a
-- cluster. This cache will be stored as an FSx Lustre that reads from the
-- S3 store.
createKxClusterResponse_cacheStorageConfigurations :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe [KxCacheStorageConfiguration])
createKxClusterResponse_cacheStorageConfigurations = Lens.lens (\CreateKxClusterResponse' {cacheStorageConfigurations} -> cacheStorageConfigurations) (\s@CreateKxClusterResponse' {} a -> s {cacheStorageConfigurations = a} :: CreateKxClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | A structure for the metadata of a cluster. It includes information like
-- the CPUs needed, memory of instances, number of instances, and the port
-- used while establishing a connection.
createKxClusterResponse_capacityConfiguration :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe CapacityConfiguration)
createKxClusterResponse_capacityConfiguration = Lens.lens (\CreateKxClusterResponse' {capacityConfiguration} -> capacityConfiguration) (\s@CreateKxClusterResponse' {} a -> s {capacityConfiguration = a} :: CreateKxClusterResponse)

-- | A description of the cluster.
createKxClusterResponse_clusterDescription :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_clusterDescription = Lens.lens (\CreateKxClusterResponse' {clusterDescription} -> clusterDescription) (\s@CreateKxClusterResponse' {} a -> s {clusterDescription = a} :: CreateKxClusterResponse)

-- | A unique name for the cluster.
createKxClusterResponse_clusterName :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_clusterName = Lens.lens (\CreateKxClusterResponse' {clusterName} -> clusterName) (\s@CreateKxClusterResponse' {} a -> s {clusterName = a} :: CreateKxClusterResponse)

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
createKxClusterResponse_clusterType :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe KxClusterType)
createKxClusterResponse_clusterType = Lens.lens (\CreateKxClusterResponse' {clusterType} -> clusterType) (\s@CreateKxClusterResponse' {} a -> s {clusterType = a} :: CreateKxClusterResponse)

-- | The details of the custom code that you want to use inside a cluster
-- when analyzing a data. It consists of the S3 source bucket, location, S3
-- object version, and the relative path from where the custom code is
-- loaded into the cluster.
createKxClusterResponse_code :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe CodeConfiguration)
createKxClusterResponse_code = Lens.lens (\CreateKxClusterResponse' {code} -> code) (\s@CreateKxClusterResponse' {} a -> s {code = a} :: CreateKxClusterResponse)

-- | Defines the key-value pairs to make them available inside the cluster.
createKxClusterResponse_commandLineArguments :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe [KxCommandLineArgument])
createKxClusterResponse_commandLineArguments = Lens.lens (\CreateKxClusterResponse' {commandLineArguments} -> commandLineArguments) (\s@CreateKxClusterResponse' {} a -> s {commandLineArguments = a} :: CreateKxClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp at which the cluster was created in FinSpace. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
createKxClusterResponse_createdTimestamp :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.UTCTime)
createKxClusterResponse_createdTimestamp = Lens.lens (\CreateKxClusterResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateKxClusterResponse' {} a -> s {createdTimestamp = a} :: CreateKxClusterResponse) Prelude.. Lens.mapping Data._Time

-- | A list of databases that will be available for querying.
createKxClusterResponse_databases :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe [KxDatabaseConfiguration])
createKxClusterResponse_databases = Lens.lens (\CreateKxClusterResponse' {databases} -> databases) (\s@CreateKxClusterResponse' {} a -> s {databases = a} :: CreateKxClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the kdb environment.
createKxClusterResponse_environmentId :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_environmentId = Lens.lens (\CreateKxClusterResponse' {environmentId} -> environmentId) (\s@CreateKxClusterResponse' {} a -> s {environmentId = a} :: CreateKxClusterResponse)

-- | An IAM role that defines a set of permissions associated with a cluster.
-- These permissions are assumed when a cluster attempts to access another
-- cluster.
createKxClusterResponse_executionRole :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_executionRole = Lens.lens (\CreateKxClusterResponse' {executionRole} -> executionRole) (\s@CreateKxClusterResponse' {} a -> s {executionRole = a} :: CreateKxClusterResponse)

-- | Specifies a Q program that will be run at launch of a cluster. It is a
-- relative path within /.zip/ file that contains the custom code, which
-- will be loaded on the cluster. It must include the file name itself. For
-- example, @somedir\/init.q@.
createKxClusterResponse_initializationScript :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_initializationScript = Lens.lens (\CreateKxClusterResponse' {initializationScript} -> initializationScript) (\s@CreateKxClusterResponse' {} a -> s {initializationScript = a} :: CreateKxClusterResponse)

-- | The last time that the cluster was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
createKxClusterResponse_lastModifiedTimestamp :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.UTCTime)
createKxClusterResponse_lastModifiedTimestamp = Lens.lens (\CreateKxClusterResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@CreateKxClusterResponse' {} a -> s {lastModifiedTimestamp = a} :: CreateKxClusterResponse) Prelude.. Lens.mapping Data._Time

-- | A version of the FinSpace managed kdb to run.
createKxClusterResponse_releaseLabel :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_releaseLabel = Lens.lens (\CreateKxClusterResponse' {releaseLabel} -> releaseLabel) (\s@CreateKxClusterResponse' {} a -> s {releaseLabel = a} :: CreateKxClusterResponse)

-- | The size and type of the temporary storage that is used to hold data
-- during the savedown process. This parameter is required when you choose
-- @clusterType@ as RDB. All the data written to this storage space is lost
-- when the cluster node is restarted.
createKxClusterResponse_savedownStorageConfiguration :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe KxSavedownStorageConfiguration)
createKxClusterResponse_savedownStorageConfiguration = Lens.lens (\CreateKxClusterResponse' {savedownStorageConfiguration} -> savedownStorageConfiguration) (\s@CreateKxClusterResponse' {} a -> s {savedownStorageConfiguration = a} :: CreateKxClusterResponse)

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
createKxClusterResponse_status :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe KxClusterStatus)
createKxClusterResponse_status = Lens.lens (\CreateKxClusterResponse' {status} -> status) (\s@CreateKxClusterResponse' {} a -> s {status = a} :: CreateKxClusterResponse)

-- | The error message when a failed state occurs.
createKxClusterResponse_statusReason :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe Prelude.Text)
createKxClusterResponse_statusReason = Lens.lens (\CreateKxClusterResponse' {statusReason} -> statusReason) (\s@CreateKxClusterResponse' {} a -> s {statusReason = a} :: CreateKxClusterResponse)

-- | Configuration details about the network where the Privatelink endpoint
-- of the cluster resides.
createKxClusterResponse_vpcConfiguration :: Lens.Lens' CreateKxClusterResponse (Prelude.Maybe VpcConfiguration)
createKxClusterResponse_vpcConfiguration = Lens.lens (\CreateKxClusterResponse' {vpcConfiguration} -> vpcConfiguration) (\s@CreateKxClusterResponse' {} a -> s {vpcConfiguration = a} :: CreateKxClusterResponse)

-- | The response's http status code.
createKxClusterResponse_httpStatus :: Lens.Lens' CreateKxClusterResponse Prelude.Int
createKxClusterResponse_httpStatus = Lens.lens (\CreateKxClusterResponse' {httpStatus} -> httpStatus) (\s@CreateKxClusterResponse' {} a -> s {httpStatus = a} :: CreateKxClusterResponse)

instance Prelude.NFData CreateKxClusterResponse where
  rnf CreateKxClusterResponse' {..} =
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
      `Prelude.seq` Prelude.rnf environmentId
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
