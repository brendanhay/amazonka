{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new development endpoint.
module Network.AWS.Glue.CreateDevEndpoint
    (
    -- * Creating a request
      CreateDevEndpoint (..)
    , mkCreateDevEndpoint
    -- ** Request lenses
    , cdeEndpointName
    , cdeRoleArn
    , cdeArguments
    , cdeExtraJarsS3Path
    , cdeExtraPythonLibsS3Path
    , cdeGlueVersion
    , cdeNumberOfNodes
    , cdeNumberOfWorkers
    , cdePublicKey
    , cdePublicKeys
    , cdeSecurityConfiguration
    , cdeSecurityGroupIds
    , cdeSubnetId
    , cdeTags
    , cdeWorkerType

    -- * Destructuring the response
    , CreateDevEndpointResponse (..)
    , mkCreateDevEndpointResponse
    -- ** Response lenses
    , cderrsArguments
    , cderrsAvailabilityZone
    , cderrsCreatedTimestamp
    , cderrsEndpointName
    , cderrsExtraJarsS3Path
    , cderrsExtraPythonLibsS3Path
    , cderrsFailureReason
    , cderrsGlueVersion
    , cderrsNumberOfNodes
    , cderrsNumberOfWorkers
    , cderrsRoleArn
    , cderrsSecurityConfiguration
    , cderrsSecurityGroupIds
    , cderrsStatus
    , cderrsSubnetId
    , cderrsVpcId
    , cderrsWorkerType
    , cderrsYarnEndpointAddress
    , cderrsZeppelinRemoteSparkInterpreterPort
    , cderrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDevEndpoint' smart constructor.
data CreateDevEndpoint = CreateDevEndpoint'
  { endpointName :: Types.EndpointName
    -- ^ The name to be assigned to the new @DevEndpoint@ .
  , roleArn :: Types.RoleArn
    -- ^ The IAM role for the @DevEndpoint@ .
  , arguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString)
    -- ^ A map of arguments used to configure the @DevEndpoint@ .
  , extraJarsS3Path :: Core.Maybe Types.ExtraJarsS3Path
    -- ^ The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
  , extraPythonLibsS3Path :: Core.Maybe Types.ExtraPythonLibsS3Path
    -- ^ The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
  , glueVersion :: Core.Maybe Types.GlueVersionString
    -- ^ Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints. 
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of AWS Glue Data Processing Units (DPUs) to allocate to this @DevEndpoint@ .
  , numberOfWorkers :: Core.Maybe Core.Int
    -- ^ The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ . 
  , publicKey :: Core.Maybe Types.PublicKey
    -- ^ The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
  , publicKeys :: Core.Maybe [Types.GenericString]
    -- ^ A list of public keys to be used by the development endpoints for authentication. The use of this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
  , securityConfiguration :: Core.Maybe Types.NameString
    -- ^ The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
  , securityGroupIds :: Core.Maybe [Types.GenericString]
    -- ^ Security group IDs for the security groups to be used by the new @DevEndpoint@ .
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The subnet ID for the new @DevEndpoint@ to use.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags to use with this DevEndpoint. You may use tags to limit access to the DevEndpoint. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDevEndpoint' value with any optional fields omitted.
mkCreateDevEndpoint
    :: Types.EndpointName -- ^ 'endpointName'
    -> Types.RoleArn -- ^ 'roleArn'
    -> CreateDevEndpoint
mkCreateDevEndpoint endpointName roleArn
  = CreateDevEndpoint'{endpointName, roleArn,
                       arguments = Core.Nothing, extraJarsS3Path = Core.Nothing,
                       extraPythonLibsS3Path = Core.Nothing, glueVersion = Core.Nothing,
                       numberOfNodes = Core.Nothing, numberOfWorkers = Core.Nothing,
                       publicKey = Core.Nothing, publicKeys = Core.Nothing,
                       securityConfiguration = Core.Nothing,
                       securityGroupIds = Core.Nothing, subnetId = Core.Nothing,
                       tags = Core.Nothing, workerType = Core.Nothing}

-- | The name to be assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeEndpointName :: Lens.Lens' CreateDevEndpoint Types.EndpointName
cdeEndpointName = Lens.field @"endpointName"
{-# INLINEABLE cdeEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | The IAM role for the @DevEndpoint@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeRoleArn :: Lens.Lens' CreateDevEndpoint Types.RoleArn
cdeRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cdeRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | A map of arguments used to configure the @DevEndpoint@ .
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeArguments :: Lens.Lens' CreateDevEndpoint (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
cdeArguments = Lens.field @"arguments"
{-# INLINEABLE cdeArguments #-}
{-# DEPRECATED arguments "Use generic-lens or generic-optics with 'arguments' instead"  #-}

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJarsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeExtraJarsS3Path :: Lens.Lens' CreateDevEndpoint (Core.Maybe Types.ExtraJarsS3Path)
cdeExtraJarsS3Path = Lens.field @"extraJarsS3Path"
{-# INLINEABLE cdeExtraJarsS3Path #-}
{-# DEPRECATED extraJarsS3Path "Use generic-lens or generic-optics with 'extraJarsS3Path' instead"  #-}

-- | The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeExtraPythonLibsS3Path :: Lens.Lens' CreateDevEndpoint (Core.Maybe Types.ExtraPythonLibsS3Path)
cdeExtraPythonLibsS3Path = Lens.field @"extraPythonLibsS3Path"
{-# INLINEABLE cdeExtraPythonLibsS3Path #-}
{-# DEPRECATED extraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead"  #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints. 
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeGlueVersion :: Lens.Lens' CreateDevEndpoint (Core.Maybe Types.GlueVersionString)
cdeGlueVersion = Lens.field @"glueVersion"
{-# INLINEABLE cdeGlueVersion #-}
{-# DEPRECATED glueVersion "Use generic-lens or generic-optics with 'glueVersion' instead"  #-}

-- | The number of AWS Glue Data Processing Units (DPUs) to allocate to this @DevEndpoint@ .
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeNumberOfNodes :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Int)
cdeNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE cdeNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ . 
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeNumberOfWorkers :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Int)
cdeNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# INLINEABLE cdeNumberOfWorkers #-}
{-# DEPRECATED numberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead"  #-}

-- | The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdePublicKey :: Lens.Lens' CreateDevEndpoint (Core.Maybe Types.PublicKey)
cdePublicKey = Lens.field @"publicKey"
{-# INLINEABLE cdePublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | A list of public keys to be used by the development endpoints for authentication. The use of this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
--
-- /Note:/ Consider using 'publicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdePublicKeys :: Lens.Lens' CreateDevEndpoint (Core.Maybe [Types.GenericString])
cdePublicKeys = Lens.field @"publicKeys"
{-# INLINEABLE cdePublicKeys #-}
{-# DEPRECATED publicKeys "Use generic-lens or generic-optics with 'publicKeys' instead"  #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeSecurityConfiguration :: Lens.Lens' CreateDevEndpoint (Core.Maybe Types.NameString)
cdeSecurityConfiguration = Lens.field @"securityConfiguration"
{-# INLINEABLE cdeSecurityConfiguration #-}
{-# DEPRECATED securityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead"  #-}

-- | Security group IDs for the security groups to be used by the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeSecurityGroupIds :: Lens.Lens' CreateDevEndpoint (Core.Maybe [Types.GenericString])
cdeSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE cdeSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The subnet ID for the new @DevEndpoint@ to use.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeSubnetId :: Lens.Lens' CreateDevEndpoint (Core.Maybe Types.SubnetId)
cdeSubnetId = Lens.field @"subnetId"
{-# INLINEABLE cdeSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The tags to use with this DevEndpoint. You may use tags to limit access to the DevEndpoint. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeTags :: Lens.Lens' CreateDevEndpoint (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cdeTags = Lens.field @"tags"
{-# INLINEABLE cdeTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

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
cdeWorkerType :: Lens.Lens' CreateDevEndpoint (Core.Maybe Types.WorkerType)
cdeWorkerType = Lens.field @"workerType"
{-# INLINEABLE cdeWorkerType #-}
{-# DEPRECATED workerType "Use generic-lens or generic-optics with 'workerType' instead"  #-}

instance Core.ToQuery CreateDevEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDevEndpoint where
        toHeaders CreateDevEndpoint{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreateDevEndpoint") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDevEndpoint where
        toJSON CreateDevEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointName" Core..= endpointName),
                  Core.Just ("RoleArn" Core..= roleArn),
                  ("Arguments" Core..=) Core.<$> arguments,
                  ("ExtraJarsS3Path" Core..=) Core.<$> extraJarsS3Path,
                  ("ExtraPythonLibsS3Path" Core..=) Core.<$> extraPythonLibsS3Path,
                  ("GlueVersion" Core..=) Core.<$> glueVersion,
                  ("NumberOfNodes" Core..=) Core.<$> numberOfNodes,
                  ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
                  ("PublicKey" Core..=) Core.<$> publicKey,
                  ("PublicKeys" Core..=) Core.<$> publicKeys,
                  ("SecurityConfiguration" Core..=) Core.<$> securityConfiguration,
                  ("SecurityGroupIds" Core..=) Core.<$> securityGroupIds,
                  ("SubnetId" Core..=) Core.<$> subnetId,
                  ("Tags" Core..=) Core.<$> tags,
                  ("WorkerType" Core..=) Core.<$> workerType])

instance Core.AWSRequest CreateDevEndpoint where
        type Rs CreateDevEndpoint = CreateDevEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDevEndpointResponse' Core.<$>
                   (x Core..:? "Arguments") Core.<*> x Core..:? "AvailabilityZone"
                     Core.<*> x Core..:? "CreatedTimestamp"
                     Core.<*> x Core..:? "EndpointName"
                     Core.<*> x Core..:? "ExtraJarsS3Path"
                     Core.<*> x Core..:? "ExtraPythonLibsS3Path"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "GlueVersion"
                     Core.<*> x Core..:? "NumberOfNodes"
                     Core.<*> x Core..:? "NumberOfWorkers"
                     Core.<*> x Core..:? "RoleArn"
                     Core.<*> x Core..:? "SecurityConfiguration"
                     Core.<*> x Core..:? "SecurityGroupIds"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "SubnetId"
                     Core.<*> x Core..:? "VpcId"
                     Core.<*> x Core..:? "WorkerType"
                     Core.<*> x Core..:? "YarnEndpointAddress"
                     Core.<*> x Core..:? "ZeppelinRemoteSparkInterpreterPort"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDevEndpointResponse' smart constructor.
data CreateDevEndpointResponse = CreateDevEndpointResponse'
  { arguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString)
    -- ^ The map of arguments used to configure this @DevEndpoint@ .
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
  , availabilityZone :: Core.Maybe Types.GenericString
    -- ^ The AWS Availability Zone where this @DevEndpoint@ is located.
  , createdTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The point in time at which this @DevEndpoint@ was created.
  , endpointName :: Core.Maybe Types.GenericString
    -- ^ The name assigned to the new @DevEndpoint@ .
  , extraJarsS3Path :: Core.Maybe Types.GenericString
    -- ^ Path to one or more Java @.jar@ files in an S3 bucket that will be loaded in your @DevEndpoint@ .
  , extraPythonLibsS3Path :: Core.Maybe Types.GenericString
    -- ^ The paths to one or more Python libraries in an S3 bucket that will be loaded in your @DevEndpoint@ .
  , failureReason :: Core.Maybe Types.GenericString
    -- ^ The reason for a current failure in this @DevEndpoint@ .
  , glueVersion :: Core.Maybe Types.GlueVersionString
    -- ^ Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints. 
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
  , numberOfWorkers :: Core.Maybe Core.Int
    -- ^ The number of workers of a defined @workerType@ that are allocated to the development endpoint.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the role assigned to the new @DevEndpoint@ .
  , securityConfiguration :: Core.Maybe Types.NameString
    -- ^ The name of the @SecurityConfiguration@ structure being used with this @DevEndpoint@ .
  , securityGroupIds :: Core.Maybe [Types.GenericString]
    -- ^ The security groups assigned to the new @DevEndpoint@ .
  , status :: Core.Maybe Types.GenericString
    -- ^ The current status of the new @DevEndpoint@ .
  , subnetId :: Core.Maybe Types.GenericString
    -- ^ The subnet ID assigned to the new @DevEndpoint@ .
  , vpcId :: Core.Maybe Types.GenericString
    -- ^ The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
  , workerType :: Core.Maybe Types.WorkerType
    -- ^ The type of predefined worker that is allocated to the development endpoint. May be a value of Standard, G.1X, or G.2X.
  , yarnEndpointAddress :: Core.Maybe Types.GenericString
    -- ^ The address of the YARN endpoint used by this @DevEndpoint@ .
  , zeppelinRemoteSparkInterpreterPort :: Core.Maybe Core.Int
    -- ^ The Apache Zeppelin port for the remote Apache Spark interpreter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDevEndpointResponse' value with any optional fields omitted.
mkCreateDevEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDevEndpointResponse
mkCreateDevEndpointResponse responseStatus
  = CreateDevEndpointResponse'{arguments = Core.Nothing,
                               availabilityZone = Core.Nothing, createdTimestamp = Core.Nothing,
                               endpointName = Core.Nothing, extraJarsS3Path = Core.Nothing,
                               extraPythonLibsS3Path = Core.Nothing, failureReason = Core.Nothing,
                               glueVersion = Core.Nothing, numberOfNodes = Core.Nothing,
                               numberOfWorkers = Core.Nothing, roleArn = Core.Nothing,
                               securityConfiguration = Core.Nothing,
                               securityGroupIds = Core.Nothing, status = Core.Nothing,
                               subnetId = Core.Nothing, vpcId = Core.Nothing,
                               workerType = Core.Nothing, yarnEndpointAddress = Core.Nothing,
                               zeppelinRemoteSparkInterpreterPort = Core.Nothing, responseStatus}

-- | The map of arguments used to configure this @DevEndpoint@ .
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
cderrsArguments :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
cderrsArguments = Lens.field @"arguments"
{-# INLINEABLE cderrsArguments #-}
{-# DEPRECATED arguments "Use generic-lens or generic-optics with 'arguments' instead"  #-}

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsAvailabilityZone :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE cderrsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The point in time at which this @DevEndpoint@ was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsCreatedTimestamp :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.NominalDiffTime)
cderrsCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE cderrsCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The name assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsEndpointName :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsEndpointName = Lens.field @"endpointName"
{-# INLINEABLE cderrsEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | Path to one or more Java @.jar@ files in an S3 bucket that will be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJarsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsExtraJarsS3Path :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsExtraJarsS3Path = Lens.field @"extraJarsS3Path"
{-# INLINEABLE cderrsExtraJarsS3Path #-}
{-# DEPRECATED extraJarsS3Path "Use generic-lens or generic-optics with 'extraJarsS3Path' instead"  #-}

-- | The paths to one or more Python libraries in an S3 bucket that will be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsExtraPythonLibsS3Path :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsExtraPythonLibsS3Path = Lens.field @"extraPythonLibsS3Path"
{-# INLINEABLE cderrsExtraPythonLibsS3Path #-}
{-# DEPRECATED extraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead"  #-}

-- | The reason for a current failure in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsFailureReason :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE cderrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints. 
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsGlueVersion :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GlueVersionString)
cderrsGlueVersion = Lens.field @"glueVersion"
{-# INLINEABLE cderrsGlueVersion #-}
{-# DEPRECATED glueVersion "Use generic-lens or generic-optics with 'glueVersion' instead"  #-}

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsNumberOfNodes :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Int)
cderrsNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE cderrsNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsNumberOfWorkers :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Int)
cderrsNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# INLINEABLE cderrsNumberOfWorkers #-}
{-# DEPRECATED numberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead"  #-}

-- | The Amazon Resource Name (ARN) of the role assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsRoleArn :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.RoleArn)
cderrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cderrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of the @SecurityConfiguration@ structure being used with this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsSecurityConfiguration :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.NameString)
cderrsSecurityConfiguration = Lens.field @"securityConfiguration"
{-# INLINEABLE cderrsSecurityConfiguration #-}
{-# DEPRECATED securityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead"  #-}

-- | The security groups assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsSecurityGroupIds :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe [Types.GenericString])
cderrsSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE cderrsSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The current status of the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsStatus :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsStatus = Lens.field @"status"
{-# INLINEABLE cderrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The subnet ID assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsSubnetId :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE cderrsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsVpcId :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsVpcId = Lens.field @"vpcId"
{-# INLINEABLE cderrsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The type of predefined worker that is allocated to the development endpoint. May be a value of Standard, G.1X, or G.2X.
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsWorkerType :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.WorkerType)
cderrsWorkerType = Lens.field @"workerType"
{-# INLINEABLE cderrsWorkerType #-}
{-# DEPRECATED workerType "Use generic-lens or generic-optics with 'workerType' instead"  #-}

-- | The address of the YARN endpoint used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'yarnEndpointAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsYarnEndpointAddress :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Types.GenericString)
cderrsYarnEndpointAddress = Lens.field @"yarnEndpointAddress"
{-# INLINEABLE cderrsYarnEndpointAddress #-}
{-# DEPRECATED yarnEndpointAddress "Use generic-lens or generic-optics with 'yarnEndpointAddress' instead"  #-}

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- /Note:/ Consider using 'zeppelinRemoteSparkInterpreterPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsZeppelinRemoteSparkInterpreterPort :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Int)
cderrsZeppelinRemoteSparkInterpreterPort = Lens.field @"zeppelinRemoteSparkInterpreterPort"
{-# INLINEABLE cderrsZeppelinRemoteSparkInterpreterPort #-}
{-# DEPRECATED zeppelinRemoteSparkInterpreterPort "Use generic-lens or generic-optics with 'zeppelinRemoteSparkInterpreterPort' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsResponseStatus :: Lens.Lens' CreateDevEndpointResponse Core.Int
cderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
