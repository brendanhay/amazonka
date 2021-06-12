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
-- Module      : Network.AWS.Glue.CreateDevEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new development endpoint.
module Network.AWS.Glue.CreateDevEndpoint
  ( -- * Creating a Request
    CreateDevEndpoint (..),
    newCreateDevEndpoint,

    -- * Request Lenses
    createDevEndpoint_securityGroupIds,
    createDevEndpoint_securityConfiguration,
    createDevEndpoint_publicKey,
    createDevEndpoint_extraPythonLibsS3Path,
    createDevEndpoint_numberOfWorkers,
    createDevEndpoint_glueVersion,
    createDevEndpoint_tags,
    createDevEndpoint_numberOfNodes,
    createDevEndpoint_workerType,
    createDevEndpoint_subnetId,
    createDevEndpoint_arguments,
    createDevEndpoint_publicKeys,
    createDevEndpoint_extraJarsS3Path,
    createDevEndpoint_endpointName,
    createDevEndpoint_roleArn,

    -- * Destructuring the Response
    CreateDevEndpointResponse (..),
    newCreateDevEndpointResponse,

    -- * Response Lenses
    createDevEndpointResponse_securityGroupIds,
    createDevEndpointResponse_status,
    createDevEndpointResponse_endpointName,
    createDevEndpointResponse_roleArn,
    createDevEndpointResponse_yarnEndpointAddress,
    createDevEndpointResponse_securityConfiguration,
    createDevEndpointResponse_createdTimestamp,
    createDevEndpointResponse_extraPythonLibsS3Path,
    createDevEndpointResponse_numberOfWorkers,
    createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort,
    createDevEndpointResponse_availabilityZone,
    createDevEndpointResponse_failureReason,
    createDevEndpointResponse_glueVersion,
    createDevEndpointResponse_numberOfNodes,
    createDevEndpointResponse_workerType,
    createDevEndpointResponse_subnetId,
    createDevEndpointResponse_vpcId,
    createDevEndpointResponse_arguments,
    createDevEndpointResponse_extraJarsS3Path,
    createDevEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDevEndpoint' smart constructor.
data CreateDevEndpoint = CreateDevEndpoint'
  { -- | Security group IDs for the security groups to be used by the new
    -- @DevEndpoint@.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- @DevEndpoint@.
    securityConfiguration :: Core.Maybe Core.Text,
    -- | The public key to be used by this @DevEndpoint@ for authentication. This
    -- attribute is provided for backward compatibility because the recommended
    -- attribute to use is public keys.
    publicKey :: Core.Maybe Core.Text,
    -- | The paths to one or more Python libraries in an Amazon S3 bucket that
    -- should be loaded in your @DevEndpoint@. Multiple values must be complete
    -- paths separated by a comma.
    --
    -- You can only use pure Python libraries with a @DevEndpoint@. Libraries
    -- that rely on C extensions, such as the
    -- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
    -- yet supported.
    extraPythonLibsS3Path :: Core.Maybe Core.Text,
    -- | The number of workers of a defined @workerType@ that are allocated to
    -- the development endpoint.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | Glue version determines the versions of Apache Spark and Python that AWS
    -- Glue supports. The Python version indicates the version supported for
    -- running your ETL scripts on development endpoints.
    --
    -- For more information about the available AWS Glue versions and
    -- corresponding Spark and Python versions, see
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
    glueVersion :: Core.Maybe Core.Text,
    -- | The tags to use with this DevEndpoint. You may use tags to limit access
    -- to the DevEndpoint. For more information about tags in AWS Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
    -- in the developer guide.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The number of AWS Glue Data Processing Units (DPUs) to allocate to this
    -- @DevEndpoint@.
    numberOfNodes :: Core.Maybe Core.Int,
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
    workerType :: Core.Maybe WorkerType,
    -- | The subnet ID for the new @DevEndpoint@ to use.
    subnetId :: Core.Maybe Core.Text,
    -- | A map of arguments used to configure the @DevEndpoint@.
    arguments :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A list of public keys to be used by the development endpoints for
    -- authentication. The use of this attribute is preferred over a single
    -- public key because the public keys allow you to have a different private
    -- key per client.
    --
    -- If you previously created an endpoint with a public key, you must remove
    -- that key to be able to set a list of public keys. Call the
    -- @UpdateDevEndpoint@ API with the public key content in the
    -- @deletePublicKeys@ attribute, and the list of new keys in the
    -- @addPublicKeys@ attribute.
    publicKeys :: Core.Maybe [Core.Text],
    -- | The path to one or more Java @.jar@ files in an S3 bucket that should be
    -- loaded in your @DevEndpoint@.
    extraJarsS3Path :: Core.Maybe Core.Text,
    -- | The name to be assigned to the new @DevEndpoint@.
    endpointName :: Core.Text,
    -- | The IAM role for the @DevEndpoint@.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDevEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createDevEndpoint_securityGroupIds' - Security group IDs for the security groups to be used by the new
-- @DevEndpoint@.
--
-- 'securityConfiguration', 'createDevEndpoint_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- @DevEndpoint@.
--
-- 'publicKey', 'createDevEndpoint_publicKey' - The public key to be used by this @DevEndpoint@ for authentication. This
-- attribute is provided for backward compatibility because the recommended
-- attribute to use is public keys.
--
-- 'extraPythonLibsS3Path', 'createDevEndpoint_extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon S3 bucket that
-- should be loaded in your @DevEndpoint@. Multiple values must be complete
-- paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- yet supported.
--
-- 'numberOfWorkers', 'createDevEndpoint_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'glueVersion', 'createDevEndpoint_glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available AWS Glue versions and
-- corresponding Spark and Python versions, see
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
-- 'tags', 'createDevEndpoint_tags' - The tags to use with this DevEndpoint. You may use tags to limit access
-- to the DevEndpoint. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
--
-- 'numberOfNodes', 'createDevEndpoint_numberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) to allocate to this
-- @DevEndpoint@.
--
-- 'workerType', 'createDevEndpoint_workerType' - The type of predefined worker that is allocated to the development
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
-- 'subnetId', 'createDevEndpoint_subnetId' - The subnet ID for the new @DevEndpoint@ to use.
--
-- 'arguments', 'createDevEndpoint_arguments' - A map of arguments used to configure the @DevEndpoint@.
--
-- 'publicKeys', 'createDevEndpoint_publicKeys' - A list of public keys to be used by the development endpoints for
-- authentication. The use of this attribute is preferred over a single
-- public key because the public keys allow you to have a different private
-- key per client.
--
-- If you previously created an endpoint with a public key, you must remove
-- that key to be able to set a list of public keys. Call the
-- @UpdateDevEndpoint@ API with the public key content in the
-- @deletePublicKeys@ attribute, and the list of new keys in the
-- @addPublicKeys@ attribute.
--
-- 'extraJarsS3Path', 'createDevEndpoint_extraJarsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- 'endpointName', 'createDevEndpoint_endpointName' - The name to be assigned to the new @DevEndpoint@.
--
-- 'roleArn', 'createDevEndpoint_roleArn' - The IAM role for the @DevEndpoint@.
newCreateDevEndpoint ::
  -- | 'endpointName'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  CreateDevEndpoint
newCreateDevEndpoint pEndpointName_ pRoleArn_ =
  CreateDevEndpoint'
    { securityGroupIds = Core.Nothing,
      securityConfiguration = Core.Nothing,
      publicKey = Core.Nothing,
      extraPythonLibsS3Path = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      glueVersion = Core.Nothing,
      tags = Core.Nothing,
      numberOfNodes = Core.Nothing,
      workerType = Core.Nothing,
      subnetId = Core.Nothing,
      arguments = Core.Nothing,
      publicKeys = Core.Nothing,
      extraJarsS3Path = Core.Nothing,
      endpointName = pEndpointName_,
      roleArn = pRoleArn_
    }

-- | Security group IDs for the security groups to be used by the new
-- @DevEndpoint@.
createDevEndpoint_securityGroupIds :: Lens.Lens' CreateDevEndpoint (Core.Maybe [Core.Text])
createDevEndpoint_securityGroupIds = Lens.lens (\CreateDevEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateDevEndpoint' {} a -> s {securityGroupIds = a} :: CreateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- @DevEndpoint@.
createDevEndpoint_securityConfiguration :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Text)
createDevEndpoint_securityConfiguration = Lens.lens (\CreateDevEndpoint' {securityConfiguration} -> securityConfiguration) (\s@CreateDevEndpoint' {} a -> s {securityConfiguration = a} :: CreateDevEndpoint)

-- | The public key to be used by this @DevEndpoint@ for authentication. This
-- attribute is provided for backward compatibility because the recommended
-- attribute to use is public keys.
createDevEndpoint_publicKey :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Text)
createDevEndpoint_publicKey = Lens.lens (\CreateDevEndpoint' {publicKey} -> publicKey) (\s@CreateDevEndpoint' {} a -> s {publicKey = a} :: CreateDevEndpoint)

-- | The paths to one or more Python libraries in an Amazon S3 bucket that
-- should be loaded in your @DevEndpoint@. Multiple values must be complete
-- paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- yet supported.
createDevEndpoint_extraPythonLibsS3Path :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Text)
createDevEndpoint_extraPythonLibsS3Path = Lens.lens (\CreateDevEndpoint' {extraPythonLibsS3Path} -> extraPythonLibsS3Path) (\s@CreateDevEndpoint' {} a -> s {extraPythonLibsS3Path = a} :: CreateDevEndpoint)

-- | The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
createDevEndpoint_numberOfWorkers :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Int)
createDevEndpoint_numberOfWorkers = Lens.lens (\CreateDevEndpoint' {numberOfWorkers} -> numberOfWorkers) (\s@CreateDevEndpoint' {} a -> s {numberOfWorkers = a} :: CreateDevEndpoint)

-- | Glue version determines the versions of Apache Spark and Python that AWS
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available AWS Glue versions and
-- corresponding Spark and Python versions, see
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
createDevEndpoint_glueVersion :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Text)
createDevEndpoint_glueVersion = Lens.lens (\CreateDevEndpoint' {glueVersion} -> glueVersion) (\s@CreateDevEndpoint' {} a -> s {glueVersion = a} :: CreateDevEndpoint)

-- | The tags to use with this DevEndpoint. You may use tags to limit access
-- to the DevEndpoint. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
createDevEndpoint_tags :: Lens.Lens' CreateDevEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
createDevEndpoint_tags = Lens.lens (\CreateDevEndpoint' {tags} -> tags) (\s@CreateDevEndpoint' {} a -> s {tags = a} :: CreateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The number of AWS Glue Data Processing Units (DPUs) to allocate to this
-- @DevEndpoint@.
createDevEndpoint_numberOfNodes :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Int)
createDevEndpoint_numberOfNodes = Lens.lens (\CreateDevEndpoint' {numberOfNodes} -> numberOfNodes) (\s@CreateDevEndpoint' {} a -> s {numberOfNodes = a} :: CreateDevEndpoint)

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
createDevEndpoint_workerType :: Lens.Lens' CreateDevEndpoint (Core.Maybe WorkerType)
createDevEndpoint_workerType = Lens.lens (\CreateDevEndpoint' {workerType} -> workerType) (\s@CreateDevEndpoint' {} a -> s {workerType = a} :: CreateDevEndpoint)

-- | The subnet ID for the new @DevEndpoint@ to use.
createDevEndpoint_subnetId :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Text)
createDevEndpoint_subnetId = Lens.lens (\CreateDevEndpoint' {subnetId} -> subnetId) (\s@CreateDevEndpoint' {} a -> s {subnetId = a} :: CreateDevEndpoint)

-- | A map of arguments used to configure the @DevEndpoint@.
createDevEndpoint_arguments :: Lens.Lens' CreateDevEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
createDevEndpoint_arguments = Lens.lens (\CreateDevEndpoint' {arguments} -> arguments) (\s@CreateDevEndpoint' {} a -> s {arguments = a} :: CreateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | A list of public keys to be used by the development endpoints for
-- authentication. The use of this attribute is preferred over a single
-- public key because the public keys allow you to have a different private
-- key per client.
--
-- If you previously created an endpoint with a public key, you must remove
-- that key to be able to set a list of public keys. Call the
-- @UpdateDevEndpoint@ API with the public key content in the
-- @deletePublicKeys@ attribute, and the list of new keys in the
-- @addPublicKeys@ attribute.
createDevEndpoint_publicKeys :: Lens.Lens' CreateDevEndpoint (Core.Maybe [Core.Text])
createDevEndpoint_publicKeys = Lens.lens (\CreateDevEndpoint' {publicKeys} -> publicKeys) (\s@CreateDevEndpoint' {} a -> s {publicKeys = a} :: CreateDevEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
createDevEndpoint_extraJarsS3Path :: Lens.Lens' CreateDevEndpoint (Core.Maybe Core.Text)
createDevEndpoint_extraJarsS3Path = Lens.lens (\CreateDevEndpoint' {extraJarsS3Path} -> extraJarsS3Path) (\s@CreateDevEndpoint' {} a -> s {extraJarsS3Path = a} :: CreateDevEndpoint)

-- | The name to be assigned to the new @DevEndpoint@.
createDevEndpoint_endpointName :: Lens.Lens' CreateDevEndpoint Core.Text
createDevEndpoint_endpointName = Lens.lens (\CreateDevEndpoint' {endpointName} -> endpointName) (\s@CreateDevEndpoint' {} a -> s {endpointName = a} :: CreateDevEndpoint)

-- | The IAM role for the @DevEndpoint@.
createDevEndpoint_roleArn :: Lens.Lens' CreateDevEndpoint Core.Text
createDevEndpoint_roleArn = Lens.lens (\CreateDevEndpoint' {roleArn} -> roleArn) (\s@CreateDevEndpoint' {} a -> s {roleArn = a} :: CreateDevEndpoint)

instance Core.AWSRequest CreateDevEndpoint where
  type
    AWSResponse CreateDevEndpoint =
      CreateDevEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDevEndpointResponse'
            Core.<$> (x Core..?> "SecurityGroupIds" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "EndpointName")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "YarnEndpointAddress")
            Core.<*> (x Core..?> "SecurityConfiguration")
            Core.<*> (x Core..?> "CreatedTimestamp")
            Core.<*> (x Core..?> "ExtraPythonLibsS3Path")
            Core.<*> (x Core..?> "NumberOfWorkers")
            Core.<*> (x Core..?> "ZeppelinRemoteSparkInterpreterPort")
            Core.<*> (x Core..?> "AvailabilityZone")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "GlueVersion")
            Core.<*> (x Core..?> "NumberOfNodes")
            Core.<*> (x Core..?> "WorkerType")
            Core.<*> (x Core..?> "SubnetId")
            Core.<*> (x Core..?> "VpcId")
            Core.<*> (x Core..?> "Arguments" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ExtraJarsS3Path")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDevEndpoint

instance Core.NFData CreateDevEndpoint

instance Core.ToHeaders CreateDevEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateDevEndpoint" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDevEndpoint where
  toJSON CreateDevEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Core.<$> securityGroupIds,
            ("SecurityConfiguration" Core..=)
              Core.<$> securityConfiguration,
            ("PublicKey" Core..=) Core.<$> publicKey,
            ("ExtraPythonLibsS3Path" Core..=)
              Core.<$> extraPythonLibsS3Path,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("Tags" Core..=) Core.<$> tags,
            ("NumberOfNodes" Core..=) Core.<$> numberOfNodes,
            ("WorkerType" Core..=) Core.<$> workerType,
            ("SubnetId" Core..=) Core.<$> subnetId,
            ("Arguments" Core..=) Core.<$> arguments,
            ("PublicKeys" Core..=) Core.<$> publicKeys,
            ("ExtraJarsS3Path" Core..=) Core.<$> extraJarsS3Path,
            Core.Just ("EndpointName" Core..= endpointName),
            Core.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateDevEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery CreateDevEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDevEndpointResponse' smart constructor.
data CreateDevEndpointResponse = CreateDevEndpointResponse'
  { -- | The security groups assigned to the new @DevEndpoint@.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The current status of the new @DevEndpoint@.
    status :: Core.Maybe Core.Text,
    -- | The name assigned to the new @DevEndpoint@.
    endpointName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the role assigned to the new
    -- @DevEndpoint@.
    roleArn :: Core.Maybe Core.Text,
    -- | The address of the YARN endpoint used by this @DevEndpoint@.
    yarnEndpointAddress :: Core.Maybe Core.Text,
    -- | The name of the @SecurityConfiguration@ structure being used with this
    -- @DevEndpoint@.
    securityConfiguration :: Core.Maybe Core.Text,
    -- | The point in time at which this @DevEndpoint@ was created.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The paths to one or more Python libraries in an S3 bucket that will be
    -- loaded in your @DevEndpoint@.
    extraPythonLibsS3Path :: Core.Maybe Core.Text,
    -- | The number of workers of a defined @workerType@ that are allocated to
    -- the development endpoint.
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | The Apache Zeppelin port for the remote Apache Spark interpreter.
    zeppelinRemoteSparkInterpreterPort :: Core.Maybe Core.Int,
    -- | The AWS Availability Zone where this @DevEndpoint@ is located.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The reason for a current failure in this @DevEndpoint@.
    failureReason :: Core.Maybe Core.Text,
    -- | Glue version determines the versions of Apache Spark and Python that AWS
    -- Glue supports. The Python version indicates the version supported for
    -- running your ETL scripts on development endpoints.
    glueVersion :: Core.Maybe Core.Text,
    -- | The number of AWS Glue Data Processing Units (DPUs) allocated to this
    -- DevEndpoint.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | The type of predefined worker that is allocated to the development
    -- endpoint. May be a value of Standard, G.1X, or G.2X.
    workerType :: Core.Maybe WorkerType,
    -- | The subnet ID assigned to the new @DevEndpoint@.
    subnetId :: Core.Maybe Core.Text,
    -- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
    vpcId :: Core.Maybe Core.Text,
    -- | The map of arguments used to configure this @DevEndpoint@.
    --
    -- Valid arguments are:
    --
    -- -   @\"--enable-glue-datacatalog\": \"\"@
    --
    -- -   @\"GLUE_PYTHON_VERSION\": \"3\"@
    --
    -- -   @\"GLUE_PYTHON_VERSION\": \"2\"@
    --
    -- You can specify a version of Python support for development endpoints by
    -- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
    -- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
    -- defaults to Python 2.
    arguments :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Path to one or more Java @.jar@ files in an S3 bucket that will be
    -- loaded in your @DevEndpoint@.
    extraJarsS3Path :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDevEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createDevEndpointResponse_securityGroupIds' - The security groups assigned to the new @DevEndpoint@.
--
-- 'status', 'createDevEndpointResponse_status' - The current status of the new @DevEndpoint@.
--
-- 'endpointName', 'createDevEndpointResponse_endpointName' - The name assigned to the new @DevEndpoint@.
--
-- 'roleArn', 'createDevEndpointResponse_roleArn' - The Amazon Resource Name (ARN) of the role assigned to the new
-- @DevEndpoint@.
--
-- 'yarnEndpointAddress', 'createDevEndpointResponse_yarnEndpointAddress' - The address of the YARN endpoint used by this @DevEndpoint@.
--
-- 'securityConfiguration', 'createDevEndpointResponse_securityConfiguration' - The name of the @SecurityConfiguration@ structure being used with this
-- @DevEndpoint@.
--
-- 'createdTimestamp', 'createDevEndpointResponse_createdTimestamp' - The point in time at which this @DevEndpoint@ was created.
--
-- 'extraPythonLibsS3Path', 'createDevEndpointResponse_extraPythonLibsS3Path' - The paths to one or more Python libraries in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
--
-- 'numberOfWorkers', 'createDevEndpointResponse_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- 'zeppelinRemoteSparkInterpreterPort', 'createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- 'availabilityZone', 'createDevEndpointResponse_availabilityZone' - The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- 'failureReason', 'createDevEndpointResponse_failureReason' - The reason for a current failure in this @DevEndpoint@.
--
-- 'glueVersion', 'createDevEndpointResponse_glueVersion' - Glue version determines the versions of Apache Spark and Python that AWS
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- 'numberOfNodes', 'createDevEndpointResponse_numberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) allocated to this
-- DevEndpoint.
--
-- 'workerType', 'createDevEndpointResponse_workerType' - The type of predefined worker that is allocated to the development
-- endpoint. May be a value of Standard, G.1X, or G.2X.
--
-- 'subnetId', 'createDevEndpointResponse_subnetId' - The subnet ID assigned to the new @DevEndpoint@.
--
-- 'vpcId', 'createDevEndpointResponse_vpcId' - The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
--
-- 'arguments', 'createDevEndpointResponse_arguments' - The map of arguments used to configure this @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"3\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"2\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
--
-- 'extraJarsS3Path', 'createDevEndpointResponse_extraJarsS3Path' - Path to one or more Java @.jar@ files in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
--
-- 'httpStatus', 'createDevEndpointResponse_httpStatus' - The response's http status code.
newCreateDevEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDevEndpointResponse
newCreateDevEndpointResponse pHttpStatus_ =
  CreateDevEndpointResponse'
    { securityGroupIds =
        Core.Nothing,
      status = Core.Nothing,
      endpointName = Core.Nothing,
      roleArn = Core.Nothing,
      yarnEndpointAddress = Core.Nothing,
      securityConfiguration = Core.Nothing,
      createdTimestamp = Core.Nothing,
      extraPythonLibsS3Path = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      zeppelinRemoteSparkInterpreterPort =
        Core.Nothing,
      availabilityZone = Core.Nothing,
      failureReason = Core.Nothing,
      glueVersion = Core.Nothing,
      numberOfNodes = Core.Nothing,
      workerType = Core.Nothing,
      subnetId = Core.Nothing,
      vpcId = Core.Nothing,
      arguments = Core.Nothing,
      extraJarsS3Path = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The security groups assigned to the new @DevEndpoint@.
createDevEndpointResponse_securityGroupIds :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe [Core.Text])
createDevEndpointResponse_securityGroupIds = Lens.lens (\CreateDevEndpointResponse' {securityGroupIds} -> securityGroupIds) (\s@CreateDevEndpointResponse' {} a -> s {securityGroupIds = a} :: CreateDevEndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | The current status of the new @DevEndpoint@.
createDevEndpointResponse_status :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_status = Lens.lens (\CreateDevEndpointResponse' {status} -> status) (\s@CreateDevEndpointResponse' {} a -> s {status = a} :: CreateDevEndpointResponse)

-- | The name assigned to the new @DevEndpoint@.
createDevEndpointResponse_endpointName :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_endpointName = Lens.lens (\CreateDevEndpointResponse' {endpointName} -> endpointName) (\s@CreateDevEndpointResponse' {} a -> s {endpointName = a} :: CreateDevEndpointResponse)

-- | The Amazon Resource Name (ARN) of the role assigned to the new
-- @DevEndpoint@.
createDevEndpointResponse_roleArn :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_roleArn = Lens.lens (\CreateDevEndpointResponse' {roleArn} -> roleArn) (\s@CreateDevEndpointResponse' {} a -> s {roleArn = a} :: CreateDevEndpointResponse)

-- | The address of the YARN endpoint used by this @DevEndpoint@.
createDevEndpointResponse_yarnEndpointAddress :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_yarnEndpointAddress = Lens.lens (\CreateDevEndpointResponse' {yarnEndpointAddress} -> yarnEndpointAddress) (\s@CreateDevEndpointResponse' {} a -> s {yarnEndpointAddress = a} :: CreateDevEndpointResponse)

-- | The name of the @SecurityConfiguration@ structure being used with this
-- @DevEndpoint@.
createDevEndpointResponse_securityConfiguration :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_securityConfiguration = Lens.lens (\CreateDevEndpointResponse' {securityConfiguration} -> securityConfiguration) (\s@CreateDevEndpointResponse' {} a -> s {securityConfiguration = a} :: CreateDevEndpointResponse)

-- | The point in time at which this @DevEndpoint@ was created.
createDevEndpointResponse_createdTimestamp :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.UTCTime)
createDevEndpointResponse_createdTimestamp = Lens.lens (\CreateDevEndpointResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateDevEndpointResponse' {} a -> s {createdTimestamp = a} :: CreateDevEndpointResponse) Core.. Lens.mapping Core._Time

-- | The paths to one or more Python libraries in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
createDevEndpointResponse_extraPythonLibsS3Path :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_extraPythonLibsS3Path = Lens.lens (\CreateDevEndpointResponse' {extraPythonLibsS3Path} -> extraPythonLibsS3Path) (\s@CreateDevEndpointResponse' {} a -> s {extraPythonLibsS3Path = a} :: CreateDevEndpointResponse)

-- | The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
createDevEndpointResponse_numberOfWorkers :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Int)
createDevEndpointResponse_numberOfWorkers = Lens.lens (\CreateDevEndpointResponse' {numberOfWorkers} -> numberOfWorkers) (\s@CreateDevEndpointResponse' {} a -> s {numberOfWorkers = a} :: CreateDevEndpointResponse)

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Int)
createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort = Lens.lens (\CreateDevEndpointResponse' {zeppelinRemoteSparkInterpreterPort} -> zeppelinRemoteSparkInterpreterPort) (\s@CreateDevEndpointResponse' {} a -> s {zeppelinRemoteSparkInterpreterPort = a} :: CreateDevEndpointResponse)

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
createDevEndpointResponse_availabilityZone :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_availabilityZone = Lens.lens (\CreateDevEndpointResponse' {availabilityZone} -> availabilityZone) (\s@CreateDevEndpointResponse' {} a -> s {availabilityZone = a} :: CreateDevEndpointResponse)

-- | The reason for a current failure in this @DevEndpoint@.
createDevEndpointResponse_failureReason :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_failureReason = Lens.lens (\CreateDevEndpointResponse' {failureReason} -> failureReason) (\s@CreateDevEndpointResponse' {} a -> s {failureReason = a} :: CreateDevEndpointResponse)

-- | Glue version determines the versions of Apache Spark and Python that AWS
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
createDevEndpointResponse_glueVersion :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_glueVersion = Lens.lens (\CreateDevEndpointResponse' {glueVersion} -> glueVersion) (\s@CreateDevEndpointResponse' {} a -> s {glueVersion = a} :: CreateDevEndpointResponse)

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this
-- DevEndpoint.
createDevEndpointResponse_numberOfNodes :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Int)
createDevEndpointResponse_numberOfNodes = Lens.lens (\CreateDevEndpointResponse' {numberOfNodes} -> numberOfNodes) (\s@CreateDevEndpointResponse' {} a -> s {numberOfNodes = a} :: CreateDevEndpointResponse)

-- | The type of predefined worker that is allocated to the development
-- endpoint. May be a value of Standard, G.1X, or G.2X.
createDevEndpointResponse_workerType :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe WorkerType)
createDevEndpointResponse_workerType = Lens.lens (\CreateDevEndpointResponse' {workerType} -> workerType) (\s@CreateDevEndpointResponse' {} a -> s {workerType = a} :: CreateDevEndpointResponse)

-- | The subnet ID assigned to the new @DevEndpoint@.
createDevEndpointResponse_subnetId :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_subnetId = Lens.lens (\CreateDevEndpointResponse' {subnetId} -> subnetId) (\s@CreateDevEndpointResponse' {} a -> s {subnetId = a} :: CreateDevEndpointResponse)

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
createDevEndpointResponse_vpcId :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_vpcId = Lens.lens (\CreateDevEndpointResponse' {vpcId} -> vpcId) (\s@CreateDevEndpointResponse' {} a -> s {vpcId = a} :: CreateDevEndpointResponse)

-- | The map of arguments used to configure this @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"3\"@
--
-- -   @\"GLUE_PYTHON_VERSION\": \"2\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
createDevEndpointResponse_arguments :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
createDevEndpointResponse_arguments = Lens.lens (\CreateDevEndpointResponse' {arguments} -> arguments) (\s@CreateDevEndpointResponse' {} a -> s {arguments = a} :: CreateDevEndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | Path to one or more Java @.jar@ files in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
createDevEndpointResponse_extraJarsS3Path :: Lens.Lens' CreateDevEndpointResponse (Core.Maybe Core.Text)
createDevEndpointResponse_extraJarsS3Path = Lens.lens (\CreateDevEndpointResponse' {extraJarsS3Path} -> extraJarsS3Path) (\s@CreateDevEndpointResponse' {} a -> s {extraJarsS3Path = a} :: CreateDevEndpointResponse)

-- | The response's http status code.
createDevEndpointResponse_httpStatus :: Lens.Lens' CreateDevEndpointResponse Core.Int
createDevEndpointResponse_httpStatus = Lens.lens (\CreateDevEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateDevEndpointResponse' {} a -> s {httpStatus = a} :: CreateDevEndpointResponse)

instance Core.NFData CreateDevEndpointResponse
