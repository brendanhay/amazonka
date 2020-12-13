{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDevEndpoint (..),
    mkCreateDevEndpoint,

    -- ** Request lenses
    cdeEndpointName,
    cdeNumberOfWorkers,
    cdeExtraPythonLibsS3Path,
    cdeSecurityGroupIds,
    cdePublicKeys,
    cdeArguments,
    cdeWorkerType,
    cdeSecurityConfiguration,
    cdePublicKey,
    cdeSubnetId,
    cdeGlueVersion,
    cdeNumberOfNodes,
    cdeExtraJARsS3Path,
    cdeTags,
    cdeRoleARN,

    -- * Destructuring the response
    CreateDevEndpointResponse (..),
    mkCreateDevEndpointResponse,

    -- ** Response lenses
    cdersStatus,
    cdersFailureReason,
    cdersEndpointName,
    cdersNumberOfWorkers,
    cdersExtraPythonLibsS3Path,
    cdersSecurityGroupIds,
    cdersVPCId,
    cdersArguments,
    cdersWorkerType,
    cdersSecurityConfiguration,
    cdersSubnetId,
    cdersGlueVersion,
    cdersNumberOfNodes,
    cdersAvailabilityZone,
    cdersZeppelinRemoteSparkInterpreterPort,
    cdersExtraJARsS3Path,
    cdersCreatedTimestamp,
    cdersYarnEndpointAddress,
    cdersRoleARN,
    cdersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDevEndpoint' smart constructor.
data CreateDevEndpoint = CreateDevEndpoint'
  { -- | The name to be assigned to the new @DevEndpoint@ .
    endpointName :: Lude.Text,
    -- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
    numberOfWorkers :: Lude.Maybe Lude.Int,
    -- | The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
    extraPythonLibsS3Path :: Lude.Maybe Lude.Text,
    -- | Security group IDs for the security groups to be used by the new @DevEndpoint@ .
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | A list of public keys to be used by the development endpoints for authentication. The use of this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
    publicKeys :: Lude.Maybe [Lude.Text],
    -- | A map of arguments used to configure the @DevEndpoint@ .
    arguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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
    -- | The subnet ID for the new @DevEndpoint@ to use.
    subnetId :: Lude.Maybe Lude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
    --
    -- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
    -- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
    -- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
    glueVersion :: Lude.Maybe Lude.Text,
    -- | The number of AWS Glue Data Processing Units (DPUs) to allocate to this @DevEndpoint@ .
    numberOfNodes :: Lude.Maybe Lude.Int,
    -- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
    extraJARsS3Path :: Lude.Maybe Lude.Text,
    -- | The tags to use with this DevEndpoint. You may use tags to limit access to the DevEndpoint. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The IAM role for the @DevEndpoint@ .
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDevEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - The name to be assigned to the new @DevEndpoint@ .
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
-- * 'extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
-- * 'securityGroupIds' - Security group IDs for the security groups to be used by the new @DevEndpoint@ .
-- * 'publicKeys' - A list of public keys to be used by the development endpoints for authentication. The use of this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
-- * 'arguments' - A map of arguments used to configure the @DevEndpoint@ .
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
-- * 'subnetId' - The subnet ID for the new @DevEndpoint@ to use.
-- * 'glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
-- * 'numberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) to allocate to this @DevEndpoint@ .
-- * 'extraJARsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
-- * 'tags' - The tags to use with this DevEndpoint. You may use tags to limit access to the DevEndpoint. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
-- * 'roleARN' - The IAM role for the @DevEndpoint@ .
mkCreateDevEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateDevEndpoint
mkCreateDevEndpoint pEndpointName_ pRoleARN_ =
  CreateDevEndpoint'
    { endpointName = pEndpointName_,
      numberOfWorkers = Lude.Nothing,
      extraPythonLibsS3Path = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      publicKeys = Lude.Nothing,
      arguments = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      publicKey = Lude.Nothing,
      subnetId = Lude.Nothing,
      glueVersion = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      extraJARsS3Path = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The name to be assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeEndpointName :: Lens.Lens' CreateDevEndpoint Lude.Text
cdeEndpointName = Lens.lens (endpointName :: CreateDevEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeNumberOfWorkers :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Int)
cdeNumberOfWorkers = Lens.lens (numberOfWorkers :: CreateDevEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeExtraPythonLibsS3Path :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Text)
cdeExtraPythonLibsS3Path = Lens.lens (extraPythonLibsS3Path :: CreateDevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {extraPythonLibsS3Path = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeExtraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead." #-}

-- | Security group IDs for the security groups to be used by the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeSecurityGroupIds :: Lens.Lens' CreateDevEndpoint (Lude.Maybe [Lude.Text])
cdeSecurityGroupIds = Lens.lens (securityGroupIds :: CreateDevEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A list of public keys to be used by the development endpoints for authentication. The use of this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
--
-- /Note:/ Consider using 'publicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdePublicKeys :: Lens.Lens' CreateDevEndpoint (Lude.Maybe [Lude.Text])
cdePublicKeys = Lens.lens (publicKeys :: CreateDevEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {publicKeys = a} :: CreateDevEndpoint)
{-# DEPRECATED cdePublicKeys "Use generic-lens or generic-optics with 'publicKeys' instead." #-}

-- | A map of arguments used to configure the @DevEndpoint@ .
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeArguments :: Lens.Lens' CreateDevEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdeArguments = Lens.lens (arguments :: CreateDevEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {arguments = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

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
cdeWorkerType :: Lens.Lens' CreateDevEndpoint (Lude.Maybe WorkerType)
cdeWorkerType = Lens.lens (workerType :: CreateDevEndpoint -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeSecurityConfiguration :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Text)
cdeSecurityConfiguration = Lens.lens (securityConfiguration :: CreateDevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdePublicKey :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Text)
cdePublicKey = Lens.lens (publicKey :: CreateDevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {publicKey = a} :: CreateDevEndpoint)
{-# DEPRECATED cdePublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The subnet ID for the new @DevEndpoint@ to use.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeSubnetId :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Text)
cdeSubnetId = Lens.lens (subnetId :: CreateDevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
--
-- For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide.
-- Development endpoints that are created without specifying a Glue version default to Glue 0.9.
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeGlueVersion :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Text)
cdeGlueVersion = Lens.lens (glueVersion :: CreateDevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The number of AWS Glue Data Processing Units (DPUs) to allocate to this @DevEndpoint@ .
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeNumberOfNodes :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Int)
cdeNumberOfNodes = Lens.lens (numberOfNodes :: CreateDevEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJARsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeExtraJARsS3Path :: Lens.Lens' CreateDevEndpoint (Lude.Maybe Lude.Text)
cdeExtraJARsS3Path = Lens.lens (extraJARsS3Path :: CreateDevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {extraJARsS3Path = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeExtraJARsS3Path "Use generic-lens or generic-optics with 'extraJARsS3Path' instead." #-}

-- | The tags to use with this DevEndpoint. You may use tags to limit access to the DevEndpoint. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeTags :: Lens.Lens' CreateDevEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdeTags = Lens.lens (tags :: CreateDevEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IAM role for the @DevEndpoint@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeRoleARN :: Lens.Lens' CreateDevEndpoint Lude.Text
cdeRoleARN = Lens.lens (roleARN :: CreateDevEndpoint -> Lude.Text) (\s a -> s {roleARN = a} :: CreateDevEndpoint)
{-# DEPRECATED cdeRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateDevEndpoint where
  type Rs CreateDevEndpoint = CreateDevEndpointResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDevEndpointResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "EndpointName")
            Lude.<*> (x Lude..?> "NumberOfWorkers")
            Lude.<*> (x Lude..?> "ExtraPythonLibsS3Path")
            Lude.<*> (x Lude..?> "SecurityGroupIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "VpcId")
            Lude.<*> (x Lude..?> "Arguments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "WorkerType")
            Lude.<*> (x Lude..?> "SecurityConfiguration")
            Lude.<*> (x Lude..?> "SubnetId")
            Lude.<*> (x Lude..?> "GlueVersion")
            Lude.<*> (x Lude..?> "NumberOfNodes")
            Lude.<*> (x Lude..?> "AvailabilityZone")
            Lude.<*> (x Lude..?> "ZeppelinRemoteSparkInterpreterPort")
            Lude.<*> (x Lude..?> "ExtraJarsS3Path")
            Lude.<*> (x Lude..?> "CreatedTimestamp")
            Lude.<*> (x Lude..?> "YarnEndpointAddress")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDevEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateDevEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDevEndpoint where
  toJSON CreateDevEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EndpointName" Lude..= endpointName),
            ("NumberOfWorkers" Lude..=) Lude.<$> numberOfWorkers,
            ("ExtraPythonLibsS3Path" Lude..=) Lude.<$> extraPythonLibsS3Path,
            ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("PublicKeys" Lude..=) Lude.<$> publicKeys,
            ("Arguments" Lude..=) Lude.<$> arguments,
            ("WorkerType" Lude..=) Lude.<$> workerType,
            ("SecurityConfiguration" Lude..=) Lude.<$> securityConfiguration,
            ("PublicKey" Lude..=) Lude.<$> publicKey,
            ("SubnetId" Lude..=) Lude.<$> subnetId,
            ("GlueVersion" Lude..=) Lude.<$> glueVersion,
            ("NumberOfNodes" Lude..=) Lude.<$> numberOfNodes,
            ("ExtraJarsS3Path" Lude..=) Lude.<$> extraJARsS3Path,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateDevEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDevEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDevEndpointResponse' smart constructor.
data CreateDevEndpointResponse = CreateDevEndpointResponse'
  { -- | The current status of the new @DevEndpoint@ .
    status :: Lude.Maybe Lude.Text,
    -- | The reason for a current failure in this @DevEndpoint@ .
    failureReason :: Lude.Maybe Lude.Text,
    -- | The name assigned to the new @DevEndpoint@ .
    endpointName :: Lude.Maybe Lude.Text,
    -- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
    numberOfWorkers :: Lude.Maybe Lude.Int,
    -- | The paths to one or more Python libraries in an S3 bucket that will be loaded in your @DevEndpoint@ .
    extraPythonLibsS3Path :: Lude.Maybe Lude.Text,
    -- | The security groups assigned to the new @DevEndpoint@ .
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
    vpcId :: Lude.Maybe Lude.Text,
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
    arguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The type of predefined worker that is allocated to the development endpoint. May be a value of Standard, G.1X, or G.2X.
    workerType :: Lude.Maybe WorkerType,
    -- | The name of the @SecurityConfiguration@ structure being used with this @DevEndpoint@ .
    securityConfiguration :: Lude.Maybe Lude.Text,
    -- | The subnet ID assigned to the new @DevEndpoint@ .
    subnetId :: Lude.Maybe Lude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
    glueVersion :: Lude.Maybe Lude.Text,
    -- | The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
    numberOfNodes :: Lude.Maybe Lude.Int,
    -- | The AWS Availability Zone where this @DevEndpoint@ is located.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The Apache Zeppelin port for the remote Apache Spark interpreter.
    zeppelinRemoteSparkInterpreterPort :: Lude.Maybe Lude.Int,
    -- | Path to one or more Java @.jar@ files in an S3 bucket that will be loaded in your @DevEndpoint@ .
    extraJARsS3Path :: Lude.Maybe Lude.Text,
    -- | The point in time at which this @DevEndpoint@ was created.
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The address of the YARN endpoint used by this @DevEndpoint@ .
    yarnEndpointAddress :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the role assigned to the new @DevEndpoint@ .
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDevEndpointResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the new @DevEndpoint@ .
-- * 'failureReason' - The reason for a current failure in this @DevEndpoint@ .
-- * 'endpointName' - The name assigned to the new @DevEndpoint@ .
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to the development endpoint.
-- * 'extraPythonLibsS3Path' - The paths to one or more Python libraries in an S3 bucket that will be loaded in your @DevEndpoint@ .
-- * 'securityGroupIds' - The security groups assigned to the new @DevEndpoint@ .
-- * 'vpcId' - The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
-- * 'arguments' - The map of arguments used to configure this @DevEndpoint@ .
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
-- * 'workerType' - The type of predefined worker that is allocated to the development endpoint. May be a value of Standard, G.1X, or G.2X.
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure being used with this @DevEndpoint@ .
-- * 'subnetId' - The subnet ID assigned to the new @DevEndpoint@ .
-- * 'glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
-- * 'numberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
-- * 'availabilityZone' - The AWS Availability Zone where this @DevEndpoint@ is located.
-- * 'zeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
-- * 'extraJARsS3Path' - Path to one or more Java @.jar@ files in an S3 bucket that will be loaded in your @DevEndpoint@ .
-- * 'createdTimestamp' - The point in time at which this @DevEndpoint@ was created.
-- * 'yarnEndpointAddress' - The address of the YARN endpoint used by this @DevEndpoint@ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role assigned to the new @DevEndpoint@ .
-- * 'responseStatus' - The response status code.
mkCreateDevEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDevEndpointResponse
mkCreateDevEndpointResponse pResponseStatus_ =
  CreateDevEndpointResponse'
    { status = Lude.Nothing,
      failureReason = Lude.Nothing,
      endpointName = Lude.Nothing,
      numberOfWorkers = Lude.Nothing,
      extraPythonLibsS3Path = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      arguments = Lude.Nothing,
      workerType = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      subnetId = Lude.Nothing,
      glueVersion = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      zeppelinRemoteSparkInterpreterPort = Lude.Nothing,
      extraJARsS3Path = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      yarnEndpointAddress = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersStatus :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersStatus = Lens.lens (status :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The reason for a current failure in this @DevEndpoint@ .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersFailureReason :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersFailureReason = Lens.lens (failureReason :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersEndpointName :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersEndpointName = Lens.lens (endpointName :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpointName = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersNumberOfWorkers :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Int)
cdersNumberOfWorkers = Lens.lens (numberOfWorkers :: CreateDevEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The paths to one or more Python libraries in an S3 bucket that will be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersExtraPythonLibsS3Path :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersExtraPythonLibsS3Path = Lens.lens (extraPythonLibsS3Path :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {extraPythonLibsS3Path = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersExtraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead." #-}

-- | The security groups assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersSecurityGroupIds :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe [Lude.Text])
cdersSecurityGroupIds = Lens.lens (securityGroupIds :: CreateDevEndpointResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersVPCId :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersVPCId = Lens.lens (vpcId :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

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
cdersArguments :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdersArguments = Lens.lens (arguments :: CreateDevEndpointResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {arguments = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

-- | The type of predefined worker that is allocated to the development endpoint. May be a value of Standard, G.1X, or G.2X.
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersWorkerType :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe WorkerType)
cdersWorkerType = Lens.lens (workerType :: CreateDevEndpointResponse -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | The name of the @SecurityConfiguration@ structure being used with this @DevEndpoint@ .
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersSecurityConfiguration :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersSecurityConfiguration = Lens.lens (securityConfiguration :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The subnet ID assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersSubnetId :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersSubnetId = Lens.lens (subnetId :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersGlueVersion :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersGlueVersion = Lens.lens (glueVersion :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersNumberOfNodes :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Int)
cdersNumberOfNodes = Lens.lens (numberOfNodes :: CreateDevEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersAvailabilityZone :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersAvailabilityZone = Lens.lens (availabilityZone :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- /Note:/ Consider using 'zeppelinRemoteSparkInterpreterPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersZeppelinRemoteSparkInterpreterPort :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Int)
cdersZeppelinRemoteSparkInterpreterPort = Lens.lens (zeppelinRemoteSparkInterpreterPort :: CreateDevEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {zeppelinRemoteSparkInterpreterPort = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersZeppelinRemoteSparkInterpreterPort "Use generic-lens or generic-optics with 'zeppelinRemoteSparkInterpreterPort' instead." #-}

-- | Path to one or more Java @.jar@ files in an S3 bucket that will be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJARsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersExtraJARsS3Path :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersExtraJARsS3Path = Lens.lens (extraJARsS3Path :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {extraJARsS3Path = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersExtraJARsS3Path "Use generic-lens or generic-optics with 'extraJARsS3Path' instead." #-}

-- | The point in time at which this @DevEndpoint@ was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersCreatedTimestamp :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Timestamp)
cdersCreatedTimestamp = Lens.lens (createdTimestamp :: CreateDevEndpointResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The address of the YARN endpoint used by this @DevEndpoint@ .
--
-- /Note:/ Consider using 'yarnEndpointAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersYarnEndpointAddress :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersYarnEndpointAddress = Lens.lens (yarnEndpointAddress :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {yarnEndpointAddress = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersYarnEndpointAddress "Use generic-lens or generic-optics with 'yarnEndpointAddress' instead." #-}

-- | The Amazon Resource Name (ARN) of the role assigned to the new @DevEndpoint@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersRoleARN :: Lens.Lens' CreateDevEndpointResponse (Lude.Maybe Lude.Text)
cdersRoleARN = Lens.lens (roleARN :: CreateDevEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersResponseStatus :: Lens.Lens' CreateDevEndpointResponse Lude.Int
cdersResponseStatus = Lens.lens (responseStatus :: CreateDevEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDevEndpointResponse)
{-# DEPRECATED cdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
