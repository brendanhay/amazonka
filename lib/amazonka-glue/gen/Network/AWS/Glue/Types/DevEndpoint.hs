{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DevEndpoint
  ( DevEndpoint (..),

    -- * Smart constructor
    mkDevEndpoint,

    -- * Lenses
    deStatus,
    deFailureReason,
    deEndpointName,
    deNumberOfWorkers,
    deExtraPythonLibsS3Path,
    deLastUpdateStatus,
    deSecurityGroupIds,
    deLastModifiedTimestamp,
    dePublicKeys,
    deVPCId,
    deArguments,
    dePrivateAddress,
    deWorkerType,
    deSecurityConfiguration,
    dePublicKey,
    deSubnetId,
    deGlueVersion,
    deNumberOfNodes,
    dePublicAddress,
    deAvailabilityZone,
    deZeppelinRemoteSparkInterpreterPort,
    deExtraJARsS3Path,
    deCreatedTimestamp,
    deYarnEndpointAddress,
    deRoleARN,
  )
where

import Network.AWS.Glue.Types.WorkerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A development endpoint where a developer can remotely debug extract, transform, and load (ETL) scripts.
--
-- /See:/ 'mkDevEndpoint' smart constructor.
data DevEndpoint = DevEndpoint'
  { -- | The current status of this @DevEndpoint@ .
    status :: Lude.Maybe Lude.Text,
    -- | The reason for a current failure in this @DevEndpoint@ .
    failureReason :: Lude.Maybe Lude.Text,
    -- | The name of the @DevEndpoint@ .
    endpointName :: Lude.Maybe Lude.Text,
    -- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
    numberOfWorkers :: Lude.Maybe Lude.Int,
    -- | The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
    extraPythonLibsS3Path :: Lude.Maybe Lude.Text,
    -- | The status of the last update.
    lastUpdateStatus :: Lude.Maybe Lude.Text,
    -- | A list of security group identifiers used in this @DevEndpoint@ .
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | The point in time at which this @DevEndpoint@ was last modified.
    lastModifiedTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | A list of public keys to be used by the @DevEndpoints@ for authentication. Using this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
    publicKeys :: Lude.Maybe [Lude.Text],
    -- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
    vpcId :: Lude.Maybe Lude.Text,
    -- | A map of arguments used to configure the @DevEndpoint@ .
    --
    -- Valid arguments are:
    --
    --     * @"--enable-glue-datacatalog": ""@
    --
    --
    --     * @"GLUE_PYTHON_VERSION": "3"@
    --
    --
    --     * @"GLUE_PYTHON_VERSION": "2"@
    --
    --
    -- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
    arguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A private IP address to access the @DevEndpoint@ within a VPC if the @DevEndpoint@ is created within one. The @PrivateAddress@ field is present only when you create the @DevEndpoint@ within your VPC.
    privateAddress :: Lude.Maybe Lude.Text,
    -- | The type of predefined worker that is allocated to the development endpoint. Accepts a value of Standard, G.1X, or G.2X.
    --
    --
    --     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
    --
    --
    --     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
    --
    --
    --     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
    --
    --
    -- Known issue: when a development endpoint is created with the @G.2X@ @WorkerType@ configuration, the Spark drivers for the development endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
    workerType :: Lude.Maybe WorkerType,
    -- | The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
    securityConfiguration :: Lude.Maybe Lude.Text,
    -- | The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
    publicKey :: Lude.Maybe Lude.Text,
    -- | The subnet ID for this @DevEndpoint@ .
    subnetId :: Lude.Maybe Lude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
    --
    -- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
    -- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
    -- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
    glueVersion :: Lude.Maybe Lude.Text,
    -- | The number of AWS Glue Data Processing Units (DPUs) allocated to this @DevEndpoint@ .
    numberOfNodes :: Lude.Maybe Lude.Int,
    -- | The public IP address used by this @DevEndpoint@ . The @PublicAddress@ field is present only when you create a non-virtual private cloud (VPC) @DevEndpoint@ .
    publicAddress :: Lude.Maybe Lude.Text,
    -- | The AWS Availability Zone where this @DevEndpoint@ is located.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The Apache Zeppelin port for the remote Apache Spark interpreter.
    zeppelinRemoteSparkInterpreterPort :: Lude.Maybe Lude.Int,
    -- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
    extraJARsS3Path :: Lude.Maybe Lude.Text,
    -- | The point in time at which this DevEndpoint was created.
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The YARN endpoint address used by this @DevEndpoint@ .
    yarnEndpointAddress :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used in this @DevEndpoint@ .
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DevEndpoint' with the minimum fields required to make a request.
--
-- * 'status' - The current status of this @DevEndpoint@ .
-- * 'failureReason' - The reason for a current failure in this @DevEndpoint@ .
-- * 'endpointName' - The name of the @DevEndpoint@ .
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
-- * 'extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
-- * 'lastUpdateStatus' - The status of the last update.
-- * 'securityGroupIds' - A list of security group identifiers used in this @DevEndpoint@ .
-- * 'lastModifiedTimestamp' - The point in time at which this @DevEndpoint@ was last modified.
-- * 'publicKeys' - A list of public keys to be used by the @DevEndpoints@ for authentication. Using this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
-- * 'vpcId' - The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
-- * 'arguments' - A map of arguments used to configure the @DevEndpoint@ .
--
-- Valid arguments are:
--
--     * @"--enable-glue-datacatalog": ""@
--
--
--     * @"GLUE_PYTHON_VERSION": "3"@
--
--
--     * @"GLUE_PYTHON_VERSION": "2"@
--
--
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
-- * 'privateAddress' - A private IP address to access the @DevEndpoint@ within a VPC if the @DevEndpoint@ is created within one. The @PrivateAddress@ field is present only when you create the @DevEndpoint@ within your VPC.
-- * 'workerType' - The type of predefined worker that is allocated to the development endpoint. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
--     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
-- Known issue: when a development endpoint is created with the @G.2X@ @WorkerType@ configuration, the Spark drivers for the development endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
-- * 'publicKey' - The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
-- * 'subnetId' - The subnet ID for this @DevEndpoint@ .
-- * 'glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
-- * 'numberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) allocated to this @DevEndpoint@ .
-- * 'publicAddress' - The public IP address used by this @DevEndpoint@ . The @PublicAddress@ field is present only when you create a non-virtual private cloud (VPC) @DevEndpoint@ .
-- * 'availabilityZone' - The AWS Availability Zone where this @DevEndpoint@ is located.
-- * 'zeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
-- * 'extraJARsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
-- * 'createdTimestamp' - The point in time at which this DevEndpoint was created.
-- * 'yarnEndpointAddress' - The YARN endpoint address used by this @DevEndpoint@ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role used in this @DevEndpoint@ .
mkDevEndpoint ::
  DevEndpoint
mkDevEndpoint =
  DevEndpoint'
    { status = Lude.Nothing,
      failureReason = Lude.Nothing,
      endpointName = Lude.Nothing,
      numberOfWorkers = Lude.Nothing,
      extraPythonLibsS3Path = Lude.Nothing,
      lastUpdateStatus = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      lastModifiedTimestamp = Lude.Nothing,
      publicKeys = Lude.Nothing,
      vpcId = Lude.Nothing,
      arguments = Lude.Nothing,
      privateAddress = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      publicKey = Lude.Nothing,
      subnetId = Lude.Nothing,
      glueVersion = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      publicAddress = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      zeppelinRemoteSparkInterpreterPort = Lude.Nothing,
      extraJARsS3Path = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      yarnEndpointAddress = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The current status of this @DevEndpoint@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStatus :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deStatus = Lens.lens (status :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DevEndpoint)
{-# DEPRECATED deStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The reason for a current failure in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deFailureReason :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deFailureReason = Lens.lens (failureReason :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DevEndpoint)
{-# DEPRECATED deFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointName :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deEndpointName = Lens.lens (endpointName :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointName = a} :: DevEndpoint)
{-# DEPRECATED deEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNumberOfWorkers :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Int)
deNumberOfWorkers = Lens.lens (numberOfWorkers :: DevEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: DevEndpoint)
{-# DEPRECATED deNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExtraPythonLibsS3Path :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deExtraPythonLibsS3Path = Lens.lens (extraPythonLibsS3Path :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {extraPythonLibsS3Path = a} :: DevEndpoint)
{-# DEPRECATED deExtraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead." #-}

-- | The status of the last update.
--
-- /Note:/ Consider using 'lastUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLastUpdateStatus :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deLastUpdateStatus = Lens.lens (lastUpdateStatus :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdateStatus = a} :: DevEndpoint)
{-# DEPRECATED deLastUpdateStatus "Use generic-lens or generic-optics with 'lastUpdateStatus' instead." #-}

-- | A list of security group identifiers used in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSecurityGroupIds :: Lens.Lens' DevEndpoint (Lude.Maybe [Lude.Text])
deSecurityGroupIds = Lens.lens (securityGroupIds :: DevEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: DevEndpoint)
{-# DEPRECATED deSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The point in time at which this @DevEndpoint@ was last modified.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLastModifiedTimestamp :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Timestamp)
deLastModifiedTimestamp = Lens.lens (lastModifiedTimestamp :: DevEndpoint -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimestamp = a} :: DevEndpoint)
{-# DEPRECATED deLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | A list of public keys to be used by the @DevEndpoints@ for authentication. Using this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
--
-- /Note:/ Consider using 'publicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePublicKeys :: Lens.Lens' DevEndpoint (Lude.Maybe [Lude.Text])
dePublicKeys = Lens.lens (publicKeys :: DevEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {publicKeys = a} :: DevEndpoint)
{-# DEPRECATED dePublicKeys "Use generic-lens or generic-optics with 'publicKeys' instead." #-}

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deVPCId :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deVPCId = Lens.lens (vpcId :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DevEndpoint)
{-# DEPRECATED deVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A map of arguments used to configure the @DevEndpoint@ .
--
-- Valid arguments are:
--
--     * @"--enable-glue-datacatalog": ""@
--
--
--     * @"GLUE_PYTHON_VERSION": "3"@
--
--
--     * @"GLUE_PYTHON_VERSION": "2"@
--
--
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deArguments :: Lens.Lens' DevEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
deArguments = Lens.lens (arguments :: DevEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {arguments = a} :: DevEndpoint)
{-# DEPRECATED deArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

-- | A private IP address to access the @DevEndpoint@ within a VPC if the @DevEndpoint@ is created within one. The @PrivateAddress@ field is present only when you create the @DevEndpoint@ within your VPC.
--
-- /Note:/ Consider using 'privateAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePrivateAddress :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
dePrivateAddress = Lens.lens (privateAddress :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {privateAddress = a} :: DevEndpoint)
{-# DEPRECATED dePrivateAddress "Use generic-lens or generic-optics with 'privateAddress' instead." #-}

-- | The type of predefined worker that is allocated to the development endpoint. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
--     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.
--
--
-- Known issue: when a development endpoint is created with the @G.2X@ @WorkerType@ configuration, the Spark drivers for the development endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deWorkerType :: Lens.Lens' DevEndpoint (Lude.Maybe WorkerType)
deWorkerType = Lens.lens (workerType :: DevEndpoint -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: DevEndpoint)
{-# DEPRECATED deWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSecurityConfiguration :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deSecurityConfiguration = Lens.lens (securityConfiguration :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: DevEndpoint)
{-# DEPRECATED deSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePublicKey :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
dePublicKey = Lens.lens (publicKey :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {publicKey = a} :: DevEndpoint)
{-# DEPRECATED dePublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The subnet ID for this @DevEndpoint@ .
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSubnetId :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deSubnetId = Lens.lens (subnetId :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: DevEndpoint)
{-# DEPRECATED deSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deGlueVersion :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deGlueVersion = Lens.lens (glueVersion :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: DevEndpoint)
{-# DEPRECATED deGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this @DevEndpoint@ .
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNumberOfNodes :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Int)
deNumberOfNodes = Lens.lens (numberOfNodes :: DevEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: DevEndpoint)
{-# DEPRECATED deNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The public IP address used by this @DevEndpoint@ . The @PublicAddress@ field is present only when you create a non-virtual private cloud (VPC) @DevEndpoint@ .
--
-- /Note:/ Consider using 'publicAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePublicAddress :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
dePublicAddress = Lens.lens (publicAddress :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {publicAddress = a} :: DevEndpoint)
{-# DEPRECATED dePublicAddress "Use generic-lens or generic-optics with 'publicAddress' instead." #-}

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deAvailabilityZone :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deAvailabilityZone = Lens.lens (availabilityZone :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DevEndpoint)
{-# DEPRECATED deAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- /Note:/ Consider using 'zeppelinRemoteSparkInterpreterPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deZeppelinRemoteSparkInterpreterPort :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Int)
deZeppelinRemoteSparkInterpreterPort = Lens.lens (zeppelinRemoteSparkInterpreterPort :: DevEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {zeppelinRemoteSparkInterpreterPort = a} :: DevEndpoint)
{-# DEPRECATED deZeppelinRemoteSparkInterpreterPort "Use generic-lens or generic-optics with 'zeppelinRemoteSparkInterpreterPort' instead." #-}

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJARsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExtraJARsS3Path :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deExtraJARsS3Path = Lens.lens (extraJARsS3Path :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {extraJARsS3Path = a} :: DevEndpoint)
{-# DEPRECATED deExtraJARsS3Path "Use generic-lens or generic-optics with 'extraJARsS3Path' instead." #-}

-- | The point in time at which this DevEndpoint was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deCreatedTimestamp :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Timestamp)
deCreatedTimestamp = Lens.lens (createdTimestamp :: DevEndpoint -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: DevEndpoint)
{-# DEPRECATED deCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The YARN endpoint address used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'yarnEndpointAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deYarnEndpointAddress :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deYarnEndpointAddress = Lens.lens (yarnEndpointAddress :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {yarnEndpointAddress = a} :: DevEndpoint)
{-# DEPRECATED deYarnEndpointAddress "Use generic-lens or generic-optics with 'yarnEndpointAddress' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role used in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deRoleARN :: Lens.Lens' DevEndpoint (Lude.Maybe Lude.Text)
deRoleARN = Lens.lens (roleARN :: DevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DevEndpoint)
{-# DEPRECATED deRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON DevEndpoint where
  parseJSON =
    Lude.withObject
      "DevEndpoint"
      ( \x ->
          DevEndpoint'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "EndpointName")
            Lude.<*> (x Lude..:? "NumberOfWorkers")
            Lude.<*> (x Lude..:? "ExtraPythonLibsS3Path")
            Lude.<*> (x Lude..:? "LastUpdateStatus")
            Lude.<*> (x Lude..:? "SecurityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastModifiedTimestamp")
            Lude.<*> (x Lude..:? "PublicKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "Arguments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PrivateAddress")
            Lude.<*> (x Lude..:? "WorkerType")
            Lude.<*> (x Lude..:? "SecurityConfiguration")
            Lude.<*> (x Lude..:? "PublicKey")
            Lude.<*> (x Lude..:? "SubnetId")
            Lude.<*> (x Lude..:? "GlueVersion")
            Lude.<*> (x Lude..:? "NumberOfNodes")
            Lude.<*> (x Lude..:? "PublicAddress")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "ZeppelinRemoteSparkInterpreterPort")
            Lude.<*> (x Lude..:? "ExtraJarsS3Path")
            Lude.<*> (x Lude..:? "CreatedTimestamp")
            Lude.<*> (x Lude..:? "YarnEndpointAddress")
            Lude.<*> (x Lude..:? "RoleArn")
      )
