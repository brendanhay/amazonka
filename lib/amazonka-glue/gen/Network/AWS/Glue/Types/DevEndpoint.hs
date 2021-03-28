{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DevEndpoint
  ( DevEndpoint (..)
  -- * Smart constructor
  , mkDevEndpoint
  -- * Lenses
  , deArguments
  , deAvailabilityZone
  , deCreatedTimestamp
  , deEndpointName
  , deExtraJarsS3Path
  , deExtraPythonLibsS3Path
  , deFailureReason
  , deGlueVersion
  , deLastModifiedTimestamp
  , deLastUpdateStatus
  , deNumberOfNodes
  , deNumberOfWorkers
  , dePrivateAddress
  , dePublicAddress
  , dePublicKey
  , dePublicKeys
  , deRoleArn
  , deSecurityConfiguration
  , deSecurityGroupIds
  , deStatus
  , deSubnetId
  , deVpcId
  , deWorkerType
  , deYarnEndpointAddress
  , deZeppelinRemoteSparkInterpreterPort
  ) where

import qualified Network.AWS.Glue.Types.AvailabilityZone as Types
import qualified Network.AWS.Glue.Types.EndpointName as Types
import qualified Network.AWS.Glue.Types.ExtraJarsS3Path as Types
import qualified Network.AWS.Glue.Types.ExtraPythonLibsS3Path as Types
import qualified Network.AWS.Glue.Types.FailureReason as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.GlueVersionString as Types
import qualified Network.AWS.Glue.Types.LastUpdateStatus as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.PrivateAddress as Types
import qualified Network.AWS.Glue.Types.PublicAddress as Types
import qualified Network.AWS.Glue.Types.PublicKey as Types
import qualified Network.AWS.Glue.Types.RoleArn as Types
import qualified Network.AWS.Glue.Types.Status as Types
import qualified Network.AWS.Glue.Types.SubnetId as Types
import qualified Network.AWS.Glue.Types.VpcId as Types
import qualified Network.AWS.Glue.Types.WorkerType as Types
import qualified Network.AWS.Glue.Types.YarnEndpointAddress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A development endpoint where a developer can remotely debug extract, transform, and load (ETL) scripts.
--
-- /See:/ 'mkDevEndpoint' smart constructor.
data DevEndpoint = DevEndpoint'
  { arguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString)
    -- ^ A map of arguments used to configure the @DevEndpoint@ .
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
  , availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The AWS Availability Zone where this @DevEndpoint@ is located.
  , createdTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The point in time at which this DevEndpoint was created.
  , endpointName :: Core.Maybe Types.EndpointName
    -- ^ The name of the @DevEndpoint@ .
  , extraJarsS3Path :: Core.Maybe Types.ExtraJarsS3Path
    -- ^ The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
  , extraPythonLibsS3Path :: Core.Maybe Types.ExtraPythonLibsS3Path
    -- ^ The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ The reason for a current failure in this @DevEndpoint@ .
  , glueVersion :: Core.Maybe Types.GlueVersionString
    -- ^ Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints. 
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
  , lastModifiedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The point in time at which this @DevEndpoint@ was last modified.
  , lastUpdateStatus :: Core.Maybe Types.LastUpdateStatus
    -- ^ The status of the last update.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of AWS Glue Data Processing Units (DPUs) allocated to this @DevEndpoint@ .
  , numberOfWorkers :: Core.Maybe Core.Int
    -- ^ The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ . 
  , privateAddress :: Core.Maybe Types.PrivateAddress
    -- ^ A private IP address to access the @DevEndpoint@ within a VPC if the @DevEndpoint@ is created within one. The @PrivateAddress@ field is present only when you create the @DevEndpoint@ within your VPC.
  , publicAddress :: Core.Maybe Types.PublicAddress
    -- ^ The public IP address used by this @DevEndpoint@ . The @PublicAddress@ field is present only when you create a non-virtual private cloud (VPC) @DevEndpoint@ .
  , publicKey :: Core.Maybe Types.PublicKey
    -- ^ The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
  , publicKeys :: Core.Maybe [Types.GenericString]
    -- ^ A list of public keys to be used by the @DevEndpoints@ for authentication. Using this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role used in this @DevEndpoint@ .
  , securityConfiguration :: Core.Maybe Types.NameString
    -- ^ The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
  , securityGroupIds :: Core.Maybe [Types.GenericString]
    -- ^ A list of security group identifiers used in this @DevEndpoint@ .
  , status :: Core.Maybe Types.Status
    -- ^ The current status of this @DevEndpoint@ .
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The subnet ID for this @DevEndpoint@ .
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
  , workerType :: Core.Maybe Types.WorkerType
    -- ^ The type of predefined worker that is allocated to the development endpoint. Accepts a value of Standard, G.1X, or G.2X.
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
  , yarnEndpointAddress :: Core.Maybe Types.YarnEndpointAddress
    -- ^ The YARN endpoint address used by this @DevEndpoint@ .
  , zeppelinRemoteSparkInterpreterPort :: Core.Maybe Core.Int
    -- ^ The Apache Zeppelin port for the remote Apache Spark interpreter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DevEndpoint' value with any optional fields omitted.
mkDevEndpoint
    :: DevEndpoint
mkDevEndpoint
  = DevEndpoint'{arguments = Core.Nothing,
                 availabilityZone = Core.Nothing, createdTimestamp = Core.Nothing,
                 endpointName = Core.Nothing, extraJarsS3Path = Core.Nothing,
                 extraPythonLibsS3Path = Core.Nothing, failureReason = Core.Nothing,
                 glueVersion = Core.Nothing, lastModifiedTimestamp = Core.Nothing,
                 lastUpdateStatus = Core.Nothing, numberOfNodes = Core.Nothing,
                 numberOfWorkers = Core.Nothing, privateAddress = Core.Nothing,
                 publicAddress = Core.Nothing, publicKey = Core.Nothing,
                 publicKeys = Core.Nothing, roleArn = Core.Nothing,
                 securityConfiguration = Core.Nothing,
                 securityGroupIds = Core.Nothing, status = Core.Nothing,
                 subnetId = Core.Nothing, vpcId = Core.Nothing,
                 workerType = Core.Nothing, yarnEndpointAddress = Core.Nothing,
                 zeppelinRemoteSparkInterpreterPort = Core.Nothing}

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
deArguments :: Lens.Lens' DevEndpoint (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
deArguments = Lens.field @"arguments"
{-# INLINEABLE deArguments #-}
{-# DEPRECATED arguments "Use generic-lens or generic-optics with 'arguments' instead"  #-}

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deAvailabilityZone :: Lens.Lens' DevEndpoint (Core.Maybe Types.AvailabilityZone)
deAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE deAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The point in time at which this DevEndpoint was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deCreatedTimestamp :: Lens.Lens' DevEndpoint (Core.Maybe Core.NominalDiffTime)
deCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE deCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The name of the @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointName :: Lens.Lens' DevEndpoint (Core.Maybe Types.EndpointName)
deEndpointName = Lens.field @"endpointName"
{-# INLINEABLE deEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJarsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExtraJarsS3Path :: Lens.Lens' DevEndpoint (Core.Maybe Types.ExtraJarsS3Path)
deExtraJarsS3Path = Lens.field @"extraJarsS3Path"
{-# INLINEABLE deExtraJarsS3Path #-}
{-# DEPRECATED extraJarsS3Path "Use generic-lens or generic-optics with 'extraJarsS3Path' instead"  #-}

-- | The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExtraPythonLibsS3Path :: Lens.Lens' DevEndpoint (Core.Maybe Types.ExtraPythonLibsS3Path)
deExtraPythonLibsS3Path = Lens.field @"extraPythonLibsS3Path"
{-# INLINEABLE deExtraPythonLibsS3Path #-}
{-# DEPRECATED extraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead"  #-}

-- | The reason for a current failure in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deFailureReason :: Lens.Lens' DevEndpoint (Core.Maybe Types.FailureReason)
deFailureReason = Lens.field @"failureReason"
{-# INLINEABLE deFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints. 
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deGlueVersion :: Lens.Lens' DevEndpoint (Core.Maybe Types.GlueVersionString)
deGlueVersion = Lens.field @"glueVersion"
{-# INLINEABLE deGlueVersion #-}
{-# DEPRECATED glueVersion "Use generic-lens or generic-optics with 'glueVersion' instead"  #-}

-- | The point in time at which this @DevEndpoint@ was last modified.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLastModifiedTimestamp :: Lens.Lens' DevEndpoint (Core.Maybe Core.NominalDiffTime)
deLastModifiedTimestamp = Lens.field @"lastModifiedTimestamp"
{-# INLINEABLE deLastModifiedTimestamp #-}
{-# DEPRECATED lastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead"  #-}

-- | The status of the last update.
--
-- /Note:/ Consider using 'lastUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLastUpdateStatus :: Lens.Lens' DevEndpoint (Core.Maybe Types.LastUpdateStatus)
deLastUpdateStatus = Lens.field @"lastUpdateStatus"
{-# INLINEABLE deLastUpdateStatus #-}
{-# DEPRECATED lastUpdateStatus "Use generic-lens or generic-optics with 'lastUpdateStatus' instead"  #-}

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this @DevEndpoint@ .
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNumberOfNodes :: Lens.Lens' DevEndpoint (Core.Maybe Core.Int)
deNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE deNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ . 
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNumberOfWorkers :: Lens.Lens' DevEndpoint (Core.Maybe Core.Int)
deNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# INLINEABLE deNumberOfWorkers #-}
{-# DEPRECATED numberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead"  #-}

-- | A private IP address to access the @DevEndpoint@ within a VPC if the @DevEndpoint@ is created within one. The @PrivateAddress@ field is present only when you create the @DevEndpoint@ within your VPC.
--
-- /Note:/ Consider using 'privateAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePrivateAddress :: Lens.Lens' DevEndpoint (Core.Maybe Types.PrivateAddress)
dePrivateAddress = Lens.field @"privateAddress"
{-# INLINEABLE dePrivateAddress #-}
{-# DEPRECATED privateAddress "Use generic-lens or generic-optics with 'privateAddress' instead"  #-}

-- | The public IP address used by this @DevEndpoint@ . The @PublicAddress@ field is present only when you create a non-virtual private cloud (VPC) @DevEndpoint@ .
--
-- /Note:/ Consider using 'publicAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePublicAddress :: Lens.Lens' DevEndpoint (Core.Maybe Types.PublicAddress)
dePublicAddress = Lens.field @"publicAddress"
{-# INLINEABLE dePublicAddress #-}
{-# DEPRECATED publicAddress "Use generic-lens or generic-optics with 'publicAddress' instead"  #-}

-- | The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePublicKey :: Lens.Lens' DevEndpoint (Core.Maybe Types.PublicKey)
dePublicKey = Lens.field @"publicKey"
{-# INLINEABLE dePublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | A list of public keys to be used by the @DevEndpoints@ for authentication. Using this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
--
-- /Note:/ Consider using 'publicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePublicKeys :: Lens.Lens' DevEndpoint (Core.Maybe [Types.GenericString])
dePublicKeys = Lens.field @"publicKeys"
{-# INLINEABLE dePublicKeys #-}
{-# DEPRECATED publicKeys "Use generic-lens or generic-optics with 'publicKeys' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role used in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deRoleArn :: Lens.Lens' DevEndpoint (Core.Maybe Types.RoleArn)
deRoleArn = Lens.field @"roleArn"
{-# INLINEABLE deRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSecurityConfiguration :: Lens.Lens' DevEndpoint (Core.Maybe Types.NameString)
deSecurityConfiguration = Lens.field @"securityConfiguration"
{-# INLINEABLE deSecurityConfiguration #-}
{-# DEPRECATED securityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead"  #-}

-- | A list of security group identifiers used in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSecurityGroupIds :: Lens.Lens' DevEndpoint (Core.Maybe [Types.GenericString])
deSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE deSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The current status of this @DevEndpoint@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStatus :: Lens.Lens' DevEndpoint (Core.Maybe Types.Status)
deStatus = Lens.field @"status"
{-# INLINEABLE deStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The subnet ID for this @DevEndpoint@ .
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSubnetId :: Lens.Lens' DevEndpoint (Core.Maybe Types.SubnetId)
deSubnetId = Lens.field @"subnetId"
{-# INLINEABLE deSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deVpcId :: Lens.Lens' DevEndpoint (Core.Maybe Types.VpcId)
deVpcId = Lens.field @"vpcId"
{-# INLINEABLE deVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

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
deWorkerType :: Lens.Lens' DevEndpoint (Core.Maybe Types.WorkerType)
deWorkerType = Lens.field @"workerType"
{-# INLINEABLE deWorkerType #-}
{-# DEPRECATED workerType "Use generic-lens or generic-optics with 'workerType' instead"  #-}

-- | The YARN endpoint address used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'yarnEndpointAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deYarnEndpointAddress :: Lens.Lens' DevEndpoint (Core.Maybe Types.YarnEndpointAddress)
deYarnEndpointAddress = Lens.field @"yarnEndpointAddress"
{-# INLINEABLE deYarnEndpointAddress #-}
{-# DEPRECATED yarnEndpointAddress "Use generic-lens or generic-optics with 'yarnEndpointAddress' instead"  #-}

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- /Note:/ Consider using 'zeppelinRemoteSparkInterpreterPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deZeppelinRemoteSparkInterpreterPort :: Lens.Lens' DevEndpoint (Core.Maybe Core.Int)
deZeppelinRemoteSparkInterpreterPort = Lens.field @"zeppelinRemoteSparkInterpreterPort"
{-# INLINEABLE deZeppelinRemoteSparkInterpreterPort #-}
{-# DEPRECATED zeppelinRemoteSparkInterpreterPort "Use generic-lens or generic-optics with 'zeppelinRemoteSparkInterpreterPort' instead"  #-}

instance Core.FromJSON DevEndpoint where
        parseJSON
          = Core.withObject "DevEndpoint" Core.$
              \ x ->
                DevEndpoint' Core.<$>
                  (x Core..:? "Arguments") Core.<*> x Core..:? "AvailabilityZone"
                    Core.<*> x Core..:? "CreatedTimestamp"
                    Core.<*> x Core..:? "EndpointName"
                    Core.<*> x Core..:? "ExtraJarsS3Path"
                    Core.<*> x Core..:? "ExtraPythonLibsS3Path"
                    Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "GlueVersion"
                    Core.<*> x Core..:? "LastModifiedTimestamp"
                    Core.<*> x Core..:? "LastUpdateStatus"
                    Core.<*> x Core..:? "NumberOfNodes"
                    Core.<*> x Core..:? "NumberOfWorkers"
                    Core.<*> x Core..:? "PrivateAddress"
                    Core.<*> x Core..:? "PublicAddress"
                    Core.<*> x Core..:? "PublicKey"
                    Core.<*> x Core..:? "PublicKeys"
                    Core.<*> x Core..:? "RoleArn"
                    Core.<*> x Core..:? "SecurityConfiguration"
                    Core.<*> x Core..:? "SecurityGroupIds"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "SubnetId"
                    Core.<*> x Core..:? "VpcId"
                    Core.<*> x Core..:? "WorkerType"
                    Core.<*> x Core..:? "YarnEndpointAddress"
                    Core.<*> x Core..:? "ZeppelinRemoteSparkInterpreterPort"
