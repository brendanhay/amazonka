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
-- Module      : Amazonka.Glue.Types.DevEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DevEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.WorkerType
import qualified Amazonka.Prelude as Prelude

-- | A development endpoint where a developer can remotely debug extract,
-- transform, and load (ETL) scripts.
--
-- /See:/ 'newDevEndpoint' smart constructor.
data DevEndpoint = DevEndpoint'
  { -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- @DevEndpoint@.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The Apache Zeppelin port for the remote Apache Spark interpreter.
    zeppelinRemoteSparkInterpreterPort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the IAM role used in this
    -- @DevEndpoint@.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The public key to be used by this @DevEndpoint@ for authentication. This
    -- attribute is provided for backward compatibility because the recommended
    -- attribute to use is public keys.
    publicKey :: Prelude.Maybe Prelude.Text,
    -- | The number of workers of a defined @workerType@ that are allocated to
    -- the development endpoint.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | A list of security group identifiers used in this @DevEndpoint@.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the @DevEndpoint@.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The Python version indicates the version supported for
    -- running your ETL scripts on development endpoints.
    --
    -- For more information about the available Glue versions and corresponding
    -- Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    --
    -- Development endpoints that are created without specifying a Glue version
    -- default to Glue 0.9.
    --
    -- You can specify a version of Python support for development endpoints by
    -- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
    -- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
    -- defaults to Python 2.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The point in time at which this DevEndpoint was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The point in time at which this @DevEndpoint@ was last modified.
    lastModifiedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The subnet ID for this @DevEndpoint@.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The type of predefined worker that is allocated to the development
    -- endpoint. Accepts a value of Standard, G.1X, or G.2X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
    --     of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
    --     of memory, 128 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- Known issue: when a development endpoint is created with the @G.2X@
    -- @WorkerType@ configuration, the Spark drivers for the development
    -- endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
    workerType :: Prelude.Maybe WorkerType,
    -- | The public IP address used by this @DevEndpoint@. The @PublicAddress@
    -- field is present only when you create a non-virtual private cloud (VPC)
    -- @DevEndpoint@.
    publicAddress :: Prelude.Maybe Prelude.Text,
    -- | The current status of this @DevEndpoint@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Availability Zone where this @DevEndpoint@ is
    -- located.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The path to one or more Java @.jar@ files in an S3 bucket that should be
    -- loaded in your @DevEndpoint@.
    --
    -- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
    extraJarsS3Path :: Prelude.Maybe Prelude.Text,
    -- | A private IP address to access the @DevEndpoint@ within a VPC if the
    -- @DevEndpoint@ is created within one. The @PrivateAddress@ field is
    -- present only when you create the @DevEndpoint@ within your VPC.
    privateAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of Glue Data Processing Units (DPUs) allocated to this
    -- @DevEndpoint@.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | A map of arguments used to configure the @DevEndpoint@.
    --
    -- Valid arguments are:
    --
    -- -   @\"--enable-glue-datacatalog\": \"\"@
    --
    -- You can specify a version of Python support for development endpoints by
    -- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
    -- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
    -- defaults to Python 2.
    arguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The status of the last update.
    lastUpdateStatus :: Prelude.Maybe Prelude.Text,
    -- | The YARN endpoint address used by this @DevEndpoint@.
    yarnEndpointAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The paths to one or more Python libraries in an Amazon S3 bucket that
    -- should be loaded in your @DevEndpoint@. Multiple values must be complete
    -- paths separated by a comma.
    --
    -- You can only use pure Python libraries with a @DevEndpoint@. Libraries
    -- that rely on C extensions, such as the
    -- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
    -- currently supported.
    extraPythonLibsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The reason for a current failure in this @DevEndpoint@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A list of public keys to be used by the @DevEndpoints@ for
    -- authentication. Using this attribute is preferred over a single public
    -- key because the public keys allow you to have a different private key
    -- per client.
    --
    -- If you previously created an endpoint with a public key, you must remove
    -- that key to be able to set a list of public keys. Call the
    -- @UpdateDevEndpoint@ API operation with the public key content in the
    -- @deletePublicKeys@ attribute, and the list of new keys in the
    -- @addPublicKeys@ attribute.
    publicKeys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DevEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'devEndpoint_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- @DevEndpoint@.
--
-- 'zeppelinRemoteSparkInterpreterPort', 'devEndpoint_zeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- 'roleArn', 'devEndpoint_roleArn' - The Amazon Resource Name (ARN) of the IAM role used in this
-- @DevEndpoint@.
--
-- 'publicKey', 'devEndpoint_publicKey' - The public key to be used by this @DevEndpoint@ for authentication. This
-- attribute is provided for backward compatibility because the recommended
-- attribute to use is public keys.
--
-- 'numberOfWorkers', 'devEndpoint_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'securityGroupIds', 'devEndpoint_securityGroupIds' - A list of security group identifiers used in this @DevEndpoint@.
--
-- 'endpointName', 'devEndpoint_endpointName' - The name of the @DevEndpoint@.
--
-- 'glueVersion', 'devEndpoint_glueVersion' - Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Development endpoints that are created without specifying a Glue version
-- default to Glue 0.9.
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
--
-- 'createdTimestamp', 'devEndpoint_createdTimestamp' - The point in time at which this DevEndpoint was created.
--
-- 'lastModifiedTimestamp', 'devEndpoint_lastModifiedTimestamp' - The point in time at which this @DevEndpoint@ was last modified.
--
-- 'subnetId', 'devEndpoint_subnetId' - The subnet ID for this @DevEndpoint@.
--
-- 'workerType', 'devEndpoint_workerType' - The type of predefined worker that is allocated to the development
-- endpoint. Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- Known issue: when a development endpoint is created with the @G.2X@
-- @WorkerType@ configuration, the Spark drivers for the development
-- endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
--
-- 'publicAddress', 'devEndpoint_publicAddress' - The public IP address used by this @DevEndpoint@. The @PublicAddress@
-- field is present only when you create a non-virtual private cloud (VPC)
-- @DevEndpoint@.
--
-- 'status', 'devEndpoint_status' - The current status of this @DevEndpoint@.
--
-- 'availabilityZone', 'devEndpoint_availabilityZone' - The Amazon Web Services Availability Zone where this @DevEndpoint@ is
-- located.
--
-- 'extraJarsS3Path', 'devEndpoint_extraJarsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
--
-- 'privateAddress', 'devEndpoint_privateAddress' - A private IP address to access the @DevEndpoint@ within a VPC if the
-- @DevEndpoint@ is created within one. The @PrivateAddress@ field is
-- present only when you create the @DevEndpoint@ within your VPC.
--
-- 'numberOfNodes', 'devEndpoint_numberOfNodes' - The number of Glue Data Processing Units (DPUs) allocated to this
-- @DevEndpoint@.
--
-- 'arguments', 'devEndpoint_arguments' - A map of arguments used to configure the @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
--
-- 'lastUpdateStatus', 'devEndpoint_lastUpdateStatus' - The status of the last update.
--
-- 'yarnEndpointAddress', 'devEndpoint_yarnEndpointAddress' - The YARN endpoint address used by this @DevEndpoint@.
--
-- 'vpcId', 'devEndpoint_vpcId' - The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
--
-- 'extraPythonLibsS3Path', 'devEndpoint_extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon S3 bucket that
-- should be loaded in your @DevEndpoint@. Multiple values must be complete
-- paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- currently supported.
--
-- 'failureReason', 'devEndpoint_failureReason' - The reason for a current failure in this @DevEndpoint@.
--
-- 'publicKeys', 'devEndpoint_publicKeys' - A list of public keys to be used by the @DevEndpoints@ for
-- authentication. Using this attribute is preferred over a single public
-- key because the public keys allow you to have a different private key
-- per client.
--
-- If you previously created an endpoint with a public key, you must remove
-- that key to be able to set a list of public keys. Call the
-- @UpdateDevEndpoint@ API operation with the public key content in the
-- @deletePublicKeys@ attribute, and the list of new keys in the
-- @addPublicKeys@ attribute.
newDevEndpoint ::
  DevEndpoint
newDevEndpoint =
  DevEndpoint'
    { securityConfiguration =
        Prelude.Nothing,
      zeppelinRemoteSparkInterpreterPort = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      workerType = Prelude.Nothing,
      publicAddress = Prelude.Nothing,
      status = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      extraJarsS3Path = Prelude.Nothing,
      privateAddress = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      arguments = Prelude.Nothing,
      lastUpdateStatus = Prelude.Nothing,
      yarnEndpointAddress = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      extraPythonLibsS3Path = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      publicKeys = Prelude.Nothing
    }

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- @DevEndpoint@.
devEndpoint_securityConfiguration :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_securityConfiguration = Lens.lens (\DevEndpoint' {securityConfiguration} -> securityConfiguration) (\s@DevEndpoint' {} a -> s {securityConfiguration = a} :: DevEndpoint)

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
devEndpoint_zeppelinRemoteSparkInterpreterPort :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Int)
devEndpoint_zeppelinRemoteSparkInterpreterPort = Lens.lens (\DevEndpoint' {zeppelinRemoteSparkInterpreterPort} -> zeppelinRemoteSparkInterpreterPort) (\s@DevEndpoint' {} a -> s {zeppelinRemoteSparkInterpreterPort = a} :: DevEndpoint)

-- | The Amazon Resource Name (ARN) of the IAM role used in this
-- @DevEndpoint@.
devEndpoint_roleArn :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_roleArn = Lens.lens (\DevEndpoint' {roleArn} -> roleArn) (\s@DevEndpoint' {} a -> s {roleArn = a} :: DevEndpoint)

-- | The public key to be used by this @DevEndpoint@ for authentication. This
-- attribute is provided for backward compatibility because the recommended
-- attribute to use is public keys.
devEndpoint_publicKey :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_publicKey = Lens.lens (\DevEndpoint' {publicKey} -> publicKey) (\s@DevEndpoint' {} a -> s {publicKey = a} :: DevEndpoint)

-- | The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
devEndpoint_numberOfWorkers :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Int)
devEndpoint_numberOfWorkers = Lens.lens (\DevEndpoint' {numberOfWorkers} -> numberOfWorkers) (\s@DevEndpoint' {} a -> s {numberOfWorkers = a} :: DevEndpoint)

-- | A list of security group identifiers used in this @DevEndpoint@.
devEndpoint_securityGroupIds :: Lens.Lens' DevEndpoint (Prelude.Maybe [Prelude.Text])
devEndpoint_securityGroupIds = Lens.lens (\DevEndpoint' {securityGroupIds} -> securityGroupIds) (\s@DevEndpoint' {} a -> s {securityGroupIds = a} :: DevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The name of the @DevEndpoint@.
devEndpoint_endpointName :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_endpointName = Lens.lens (\DevEndpoint' {endpointName} -> endpointName) (\s@DevEndpoint' {} a -> s {endpointName = a} :: DevEndpoint)

-- | Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Development endpoints that are created without specifying a Glue version
-- default to Glue 0.9.
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
devEndpoint_glueVersion :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_glueVersion = Lens.lens (\DevEndpoint' {glueVersion} -> glueVersion) (\s@DevEndpoint' {} a -> s {glueVersion = a} :: DevEndpoint)

-- | The point in time at which this DevEndpoint was created.
devEndpoint_createdTimestamp :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.UTCTime)
devEndpoint_createdTimestamp = Lens.lens (\DevEndpoint' {createdTimestamp} -> createdTimestamp) (\s@DevEndpoint' {} a -> s {createdTimestamp = a} :: DevEndpoint) Prelude.. Lens.mapping Core._Time

-- | The point in time at which this @DevEndpoint@ was last modified.
devEndpoint_lastModifiedTimestamp :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.UTCTime)
devEndpoint_lastModifiedTimestamp = Lens.lens (\DevEndpoint' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DevEndpoint' {} a -> s {lastModifiedTimestamp = a} :: DevEndpoint) Prelude.. Lens.mapping Core._Time

-- | The subnet ID for this @DevEndpoint@.
devEndpoint_subnetId :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_subnetId = Lens.lens (\DevEndpoint' {subnetId} -> subnetId) (\s@DevEndpoint' {} a -> s {subnetId = a} :: DevEndpoint)

-- | The type of predefined worker that is allocated to the development
-- endpoint. Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- Known issue: when a development endpoint is created with the @G.2X@
-- @WorkerType@ configuration, the Spark drivers for the development
-- endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
devEndpoint_workerType :: Lens.Lens' DevEndpoint (Prelude.Maybe WorkerType)
devEndpoint_workerType = Lens.lens (\DevEndpoint' {workerType} -> workerType) (\s@DevEndpoint' {} a -> s {workerType = a} :: DevEndpoint)

-- | The public IP address used by this @DevEndpoint@. The @PublicAddress@
-- field is present only when you create a non-virtual private cloud (VPC)
-- @DevEndpoint@.
devEndpoint_publicAddress :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_publicAddress = Lens.lens (\DevEndpoint' {publicAddress} -> publicAddress) (\s@DevEndpoint' {} a -> s {publicAddress = a} :: DevEndpoint)

-- | The current status of this @DevEndpoint@.
devEndpoint_status :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_status = Lens.lens (\DevEndpoint' {status} -> status) (\s@DevEndpoint' {} a -> s {status = a} :: DevEndpoint)

-- | The Amazon Web Services Availability Zone where this @DevEndpoint@ is
-- located.
devEndpoint_availabilityZone :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_availabilityZone = Lens.lens (\DevEndpoint' {availabilityZone} -> availabilityZone) (\s@DevEndpoint' {} a -> s {availabilityZone = a} :: DevEndpoint)

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
devEndpoint_extraJarsS3Path :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_extraJarsS3Path = Lens.lens (\DevEndpoint' {extraJarsS3Path} -> extraJarsS3Path) (\s@DevEndpoint' {} a -> s {extraJarsS3Path = a} :: DevEndpoint)

-- | A private IP address to access the @DevEndpoint@ within a VPC if the
-- @DevEndpoint@ is created within one. The @PrivateAddress@ field is
-- present only when you create the @DevEndpoint@ within your VPC.
devEndpoint_privateAddress :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_privateAddress = Lens.lens (\DevEndpoint' {privateAddress} -> privateAddress) (\s@DevEndpoint' {} a -> s {privateAddress = a} :: DevEndpoint)

-- | The number of Glue Data Processing Units (DPUs) allocated to this
-- @DevEndpoint@.
devEndpoint_numberOfNodes :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Int)
devEndpoint_numberOfNodes = Lens.lens (\DevEndpoint' {numberOfNodes} -> numberOfNodes) (\s@DevEndpoint' {} a -> s {numberOfNodes = a} :: DevEndpoint)

-- | A map of arguments used to configure the @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
devEndpoint_arguments :: Lens.Lens' DevEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
devEndpoint_arguments = Lens.lens (\DevEndpoint' {arguments} -> arguments) (\s@DevEndpoint' {} a -> s {arguments = a} :: DevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The status of the last update.
devEndpoint_lastUpdateStatus :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_lastUpdateStatus = Lens.lens (\DevEndpoint' {lastUpdateStatus} -> lastUpdateStatus) (\s@DevEndpoint' {} a -> s {lastUpdateStatus = a} :: DevEndpoint)

-- | The YARN endpoint address used by this @DevEndpoint@.
devEndpoint_yarnEndpointAddress :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_yarnEndpointAddress = Lens.lens (\DevEndpoint' {yarnEndpointAddress} -> yarnEndpointAddress) (\s@DevEndpoint' {} a -> s {yarnEndpointAddress = a} :: DevEndpoint)

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
devEndpoint_vpcId :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_vpcId = Lens.lens (\DevEndpoint' {vpcId} -> vpcId) (\s@DevEndpoint' {} a -> s {vpcId = a} :: DevEndpoint)

-- | The paths to one or more Python libraries in an Amazon S3 bucket that
-- should be loaded in your @DevEndpoint@. Multiple values must be complete
-- paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- currently supported.
devEndpoint_extraPythonLibsS3Path :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_extraPythonLibsS3Path = Lens.lens (\DevEndpoint' {extraPythonLibsS3Path} -> extraPythonLibsS3Path) (\s@DevEndpoint' {} a -> s {extraPythonLibsS3Path = a} :: DevEndpoint)

-- | The reason for a current failure in this @DevEndpoint@.
devEndpoint_failureReason :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_failureReason = Lens.lens (\DevEndpoint' {failureReason} -> failureReason) (\s@DevEndpoint' {} a -> s {failureReason = a} :: DevEndpoint)

-- | A list of public keys to be used by the @DevEndpoints@ for
-- authentication. Using this attribute is preferred over a single public
-- key because the public keys allow you to have a different private key
-- per client.
--
-- If you previously created an endpoint with a public key, you must remove
-- that key to be able to set a list of public keys. Call the
-- @UpdateDevEndpoint@ API operation with the public key content in the
-- @deletePublicKeys@ attribute, and the list of new keys in the
-- @addPublicKeys@ attribute.
devEndpoint_publicKeys :: Lens.Lens' DevEndpoint (Prelude.Maybe [Prelude.Text])
devEndpoint_publicKeys = Lens.lens (\DevEndpoint' {publicKeys} -> publicKeys) (\s@DevEndpoint' {} a -> s {publicKeys = a} :: DevEndpoint) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DevEndpoint where
  parseJSON =
    Core.withObject
      "DevEndpoint"
      ( \x ->
          DevEndpoint'
            Prelude.<$> (x Core..:? "SecurityConfiguration")
            Prelude.<*> (x Core..:? "ZeppelinRemoteSparkInterpreterPort")
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "PublicKey")
            Prelude.<*> (x Core..:? "NumberOfWorkers")
            Prelude.<*> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EndpointName")
            Prelude.<*> (x Core..:? "GlueVersion")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "LastModifiedTimestamp")
            Prelude.<*> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "WorkerType")
            Prelude.<*> (x Core..:? "PublicAddress")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "ExtraJarsS3Path")
            Prelude.<*> (x Core..:? "PrivateAddress")
            Prelude.<*> (x Core..:? "NumberOfNodes")
            Prelude.<*> (x Core..:? "Arguments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastUpdateStatus")
            Prelude.<*> (x Core..:? "YarnEndpointAddress")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "ExtraPythonLibsS3Path")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "PublicKeys" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable DevEndpoint where
  hashWithSalt _salt DevEndpoint' {..} =
    _salt `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` zeppelinRemoteSparkInterpreterPort
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` publicKey
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` lastModifiedTimestamp
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` publicAddress
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` extraJarsS3Path
      `Prelude.hashWithSalt` privateAddress
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` arguments
      `Prelude.hashWithSalt` lastUpdateStatus
      `Prelude.hashWithSalt` yarnEndpointAddress
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` extraPythonLibsS3Path
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` publicKeys

instance Prelude.NFData DevEndpoint where
  rnf DevEndpoint' {..} =
    Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf zeppelinRemoteSparkInterpreterPort
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf publicAddress
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf extraJarsS3Path
      `Prelude.seq` Prelude.rnf privateAddress
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf arguments
      `Prelude.seq` Prelude.rnf lastUpdateStatus
      `Prelude.seq` Prelude.rnf
        yarnEndpointAddress
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf
        extraPythonLibsS3Path
      `Prelude.seq` Prelude.rnf
        failureReason
      `Prelude.seq` Prelude.rnf
        publicKeys
