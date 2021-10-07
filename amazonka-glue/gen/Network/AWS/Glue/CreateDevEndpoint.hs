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
    createDevEndpoint_numberOfWorkers,
    createDevEndpoint_extraPythonLibsS3Path,
    createDevEndpoint_glueVersion,
    createDevEndpoint_tags,
    createDevEndpoint_numberOfNodes,
    createDevEndpoint_workerType,
    createDevEndpoint_subnetId,
    createDevEndpoint_arguments,
    createDevEndpoint_extraJarsS3Path,
    createDevEndpoint_publicKeys,
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
    createDevEndpointResponse_numberOfWorkers,
    createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort,
    createDevEndpointResponse_extraPythonLibsS3Path,
    createDevEndpointResponse_availabilityZone,
    createDevEndpointResponse_failureReason,
    createDevEndpointResponse_glueVersion,
    createDevEndpointResponse_numberOfNodes,
    createDevEndpointResponse_workerType,
    createDevEndpointResponse_subnetId,
    createDevEndpointResponse_arguments,
    createDevEndpointResponse_vpcId,
    createDevEndpointResponse_extraJarsS3Path,
    createDevEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDevEndpoint' smart constructor.
data CreateDevEndpoint = CreateDevEndpoint'
  { -- | Security group IDs for the security groups to be used by the new
    -- @DevEndpoint@.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- @DevEndpoint@.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
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
    -- | The paths to one or more Python libraries in an Amazon S3 bucket that
    -- should be loaded in your @DevEndpoint@. Multiple values must be complete
    -- paths separated by a comma.
    --
    -- You can only use pure Python libraries with a @DevEndpoint@. Libraries
    -- that rely on C extensions, such as the
    -- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
    -- yet supported.
    extraPythonLibsS3Path :: Prelude.Maybe Prelude.Text,
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
    -- | The tags to use with this DevEndpoint. You may use tags to limit access
    -- to the DevEndpoint. For more information about tags in Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
    -- in the developer guide.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of Glue Data Processing Units (DPUs) to allocate to this
    -- @DevEndpoint@.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
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
    -- | The subnet ID for the new @DevEndpoint@ to use.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | A map of arguments used to configure the @DevEndpoint@.
    arguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The path to one or more Java @.jar@ files in an S3 bucket that should be
    -- loaded in your @DevEndpoint@.
    extraJarsS3Path :: Prelude.Maybe Prelude.Text,
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
    publicKeys :: Prelude.Maybe [Prelude.Text],
    -- | The name to be assigned to the new @DevEndpoint@.
    endpointName :: Prelude.Text,
    -- | The IAM role for the @DevEndpoint@.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'numberOfWorkers', 'createDevEndpoint_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
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
-- 'glueVersion', 'createDevEndpoint_glueVersion' - Glue version determines the versions of Apache Spark and Python that
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
-- 'tags', 'createDevEndpoint_tags' - The tags to use with this DevEndpoint. You may use tags to limit access
-- to the DevEndpoint. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
--
-- 'numberOfNodes', 'createDevEndpoint_numberOfNodes' - The number of Glue Data Processing Units (DPUs) to allocate to this
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
-- 'extraJarsS3Path', 'createDevEndpoint_extraJarsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
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
-- 'endpointName', 'createDevEndpoint_endpointName' - The name to be assigned to the new @DevEndpoint@.
--
-- 'roleArn', 'createDevEndpoint_roleArn' - The IAM role for the @DevEndpoint@.
newCreateDevEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateDevEndpoint
newCreateDevEndpoint pEndpointName_ pRoleArn_ =
  CreateDevEndpoint'
    { securityGroupIds =
        Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      extraPythonLibsS3Path = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      workerType = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      arguments = Prelude.Nothing,
      extraJarsS3Path = Prelude.Nothing,
      publicKeys = Prelude.Nothing,
      endpointName = pEndpointName_,
      roleArn = pRoleArn_
    }

-- | Security group IDs for the security groups to be used by the new
-- @DevEndpoint@.
createDevEndpoint_securityGroupIds :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe [Prelude.Text])
createDevEndpoint_securityGroupIds = Lens.lens (\CreateDevEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateDevEndpoint' {} a -> s {securityGroupIds = a} :: CreateDevEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- @DevEndpoint@.
createDevEndpoint_securityConfiguration :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Text)
createDevEndpoint_securityConfiguration = Lens.lens (\CreateDevEndpoint' {securityConfiguration} -> securityConfiguration) (\s@CreateDevEndpoint' {} a -> s {securityConfiguration = a} :: CreateDevEndpoint)

-- | The public key to be used by this @DevEndpoint@ for authentication. This
-- attribute is provided for backward compatibility because the recommended
-- attribute to use is public keys.
createDevEndpoint_publicKey :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Text)
createDevEndpoint_publicKey = Lens.lens (\CreateDevEndpoint' {publicKey} -> publicKey) (\s@CreateDevEndpoint' {} a -> s {publicKey = a} :: CreateDevEndpoint)

-- | The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
createDevEndpoint_numberOfWorkers :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Int)
createDevEndpoint_numberOfWorkers = Lens.lens (\CreateDevEndpoint' {numberOfWorkers} -> numberOfWorkers) (\s@CreateDevEndpoint' {} a -> s {numberOfWorkers = a} :: CreateDevEndpoint)

-- | The paths to one or more Python libraries in an Amazon S3 bucket that
-- should be loaded in your @DevEndpoint@. Multiple values must be complete
-- paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- yet supported.
createDevEndpoint_extraPythonLibsS3Path :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Text)
createDevEndpoint_extraPythonLibsS3Path = Lens.lens (\CreateDevEndpoint' {extraPythonLibsS3Path} -> extraPythonLibsS3Path) (\s@CreateDevEndpoint' {} a -> s {extraPythonLibsS3Path = a} :: CreateDevEndpoint)

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
createDevEndpoint_glueVersion :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Text)
createDevEndpoint_glueVersion = Lens.lens (\CreateDevEndpoint' {glueVersion} -> glueVersion) (\s@CreateDevEndpoint' {} a -> s {glueVersion = a} :: CreateDevEndpoint)

-- | The tags to use with this DevEndpoint. You may use tags to limit access
-- to the DevEndpoint. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
createDevEndpoint_tags :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDevEndpoint_tags = Lens.lens (\CreateDevEndpoint' {tags} -> tags) (\s@CreateDevEndpoint' {} a -> s {tags = a} :: CreateDevEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The number of Glue Data Processing Units (DPUs) to allocate to this
-- @DevEndpoint@.
createDevEndpoint_numberOfNodes :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Int)
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
createDevEndpoint_workerType :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe WorkerType)
createDevEndpoint_workerType = Lens.lens (\CreateDevEndpoint' {workerType} -> workerType) (\s@CreateDevEndpoint' {} a -> s {workerType = a} :: CreateDevEndpoint)

-- | The subnet ID for the new @DevEndpoint@ to use.
createDevEndpoint_subnetId :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Text)
createDevEndpoint_subnetId = Lens.lens (\CreateDevEndpoint' {subnetId} -> subnetId) (\s@CreateDevEndpoint' {} a -> s {subnetId = a} :: CreateDevEndpoint)

-- | A map of arguments used to configure the @DevEndpoint@.
createDevEndpoint_arguments :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDevEndpoint_arguments = Lens.lens (\CreateDevEndpoint' {arguments} -> arguments) (\s@CreateDevEndpoint' {} a -> s {arguments = a} :: CreateDevEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
createDevEndpoint_extraJarsS3Path :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe Prelude.Text)
createDevEndpoint_extraJarsS3Path = Lens.lens (\CreateDevEndpoint' {extraJarsS3Path} -> extraJarsS3Path) (\s@CreateDevEndpoint' {} a -> s {extraJarsS3Path = a} :: CreateDevEndpoint)

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
createDevEndpoint_publicKeys :: Lens.Lens' CreateDevEndpoint (Prelude.Maybe [Prelude.Text])
createDevEndpoint_publicKeys = Lens.lens (\CreateDevEndpoint' {publicKeys} -> publicKeys) (\s@CreateDevEndpoint' {} a -> s {publicKeys = a} :: CreateDevEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The name to be assigned to the new @DevEndpoint@.
createDevEndpoint_endpointName :: Lens.Lens' CreateDevEndpoint Prelude.Text
createDevEndpoint_endpointName = Lens.lens (\CreateDevEndpoint' {endpointName} -> endpointName) (\s@CreateDevEndpoint' {} a -> s {endpointName = a} :: CreateDevEndpoint)

-- | The IAM role for the @DevEndpoint@.
createDevEndpoint_roleArn :: Lens.Lens' CreateDevEndpoint Prelude.Text
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
            Prelude.<$> ( x Core..?> "SecurityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "EndpointName")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "YarnEndpointAddress")
            Prelude.<*> (x Core..?> "SecurityConfiguration")
            Prelude.<*> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "NumberOfWorkers")
            Prelude.<*> (x Core..?> "ZeppelinRemoteSparkInterpreterPort")
            Prelude.<*> (x Core..?> "ExtraPythonLibsS3Path")
            Prelude.<*> (x Core..?> "AvailabilityZone")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "GlueVersion")
            Prelude.<*> (x Core..?> "NumberOfNodes")
            Prelude.<*> (x Core..?> "WorkerType")
            Prelude.<*> (x Core..?> "SubnetId")
            Prelude.<*> (x Core..?> "Arguments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "VpcId")
            Prelude.<*> (x Core..?> "ExtraJarsS3Path")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDevEndpoint

instance Prelude.NFData CreateDevEndpoint

instance Core.ToHeaders CreateDevEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateDevEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDevEndpoint where
  toJSON CreateDevEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("SecurityConfiguration" Core..=)
              Prelude.<$> securityConfiguration,
            ("PublicKey" Core..=) Prelude.<$> publicKey,
            ("NumberOfWorkers" Core..=)
              Prelude.<$> numberOfWorkers,
            ("ExtraPythonLibsS3Path" Core..=)
              Prelude.<$> extraPythonLibsS3Path,
            ("GlueVersion" Core..=) Prelude.<$> glueVersion,
            ("Tags" Core..=) Prelude.<$> tags,
            ("NumberOfNodes" Core..=) Prelude.<$> numberOfNodes,
            ("WorkerType" Core..=) Prelude.<$> workerType,
            ("SubnetId" Core..=) Prelude.<$> subnetId,
            ("Arguments" Core..=) Prelude.<$> arguments,
            ("ExtraJarsS3Path" Core..=)
              Prelude.<$> extraJarsS3Path,
            ("PublicKeys" Core..=) Prelude.<$> publicKeys,
            Prelude.Just ("EndpointName" Core..= endpointName),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateDevEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDevEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDevEndpointResponse' smart constructor.
data CreateDevEndpointResponse = CreateDevEndpointResponse'
  { -- | The security groups assigned to the new @DevEndpoint@.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The current status of the new @DevEndpoint@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name assigned to the new @DevEndpoint@.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role assigned to the new
    -- @DevEndpoint@.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The address of the YARN endpoint used by this @DevEndpoint@.
    yarnEndpointAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the @SecurityConfiguration@ structure being used with this
    -- @DevEndpoint@.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The point in time at which this @DevEndpoint@ was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The number of workers of a defined @workerType@ that are allocated to
    -- the development endpoint.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The Apache Zeppelin port for the remote Apache Spark interpreter.
    zeppelinRemoteSparkInterpreterPort :: Prelude.Maybe Prelude.Int,
    -- | The paths to one or more Python libraries in an S3 bucket that will be
    -- loaded in your @DevEndpoint@.
    extraPythonLibsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The AWS Availability Zone where this @DevEndpoint@ is located.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The reason for a current failure in this @DevEndpoint@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The Python version indicates the version supported for
    -- running your ETL scripts on development endpoints.
    --
    -- For more information about the available Glue versions and corresponding
    -- Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of Glue Data Processing Units (DPUs) allocated to this
    -- DevEndpoint.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The type of predefined worker that is allocated to the development
    -- endpoint. May be a value of Standard, G.1X, or G.2X.
    workerType :: Prelude.Maybe WorkerType,
    -- | The subnet ID assigned to the new @DevEndpoint@.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The map of arguments used to configure this @DevEndpoint@.
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
    -- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Path to one or more Java @.jar@ files in an S3 bucket that will be
    -- loaded in your @DevEndpoint@.
    extraJarsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'numberOfWorkers', 'createDevEndpointResponse_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- 'zeppelinRemoteSparkInterpreterPort', 'createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- 'extraPythonLibsS3Path', 'createDevEndpointResponse_extraPythonLibsS3Path' - The paths to one or more Python libraries in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
--
-- 'availabilityZone', 'createDevEndpointResponse_availabilityZone' - The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- 'failureReason', 'createDevEndpointResponse_failureReason' - The reason for a current failure in this @DevEndpoint@.
--
-- 'glueVersion', 'createDevEndpointResponse_glueVersion' - Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- 'numberOfNodes', 'createDevEndpointResponse_numberOfNodes' - The number of Glue Data Processing Units (DPUs) allocated to this
-- DevEndpoint.
--
-- 'workerType', 'createDevEndpointResponse_workerType' - The type of predefined worker that is allocated to the development
-- endpoint. May be a value of Standard, G.1X, or G.2X.
--
-- 'subnetId', 'createDevEndpointResponse_subnetId' - The subnet ID assigned to the new @DevEndpoint@.
--
-- 'arguments', 'createDevEndpointResponse_arguments' - The map of arguments used to configure this @DevEndpoint@.
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
-- 'vpcId', 'createDevEndpointResponse_vpcId' - The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
--
-- 'extraJarsS3Path', 'createDevEndpointResponse_extraJarsS3Path' - Path to one or more Java @.jar@ files in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
--
-- 'httpStatus', 'createDevEndpointResponse_httpStatus' - The response's http status code.
newCreateDevEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDevEndpointResponse
newCreateDevEndpointResponse pHttpStatus_ =
  CreateDevEndpointResponse'
    { securityGroupIds =
        Prelude.Nothing,
      status = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      yarnEndpointAddress = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      zeppelinRemoteSparkInterpreterPort =
        Prelude.Nothing,
      extraPythonLibsS3Path = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      workerType = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      arguments = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      extraJarsS3Path = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The security groups assigned to the new @DevEndpoint@.
createDevEndpointResponse_securityGroupIds :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe [Prelude.Text])
createDevEndpointResponse_securityGroupIds = Lens.lens (\CreateDevEndpointResponse' {securityGroupIds} -> securityGroupIds) (\s@CreateDevEndpointResponse' {} a -> s {securityGroupIds = a} :: CreateDevEndpointResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The current status of the new @DevEndpoint@.
createDevEndpointResponse_status :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_status = Lens.lens (\CreateDevEndpointResponse' {status} -> status) (\s@CreateDevEndpointResponse' {} a -> s {status = a} :: CreateDevEndpointResponse)

-- | The name assigned to the new @DevEndpoint@.
createDevEndpointResponse_endpointName :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_endpointName = Lens.lens (\CreateDevEndpointResponse' {endpointName} -> endpointName) (\s@CreateDevEndpointResponse' {} a -> s {endpointName = a} :: CreateDevEndpointResponse)

-- | The Amazon Resource Name (ARN) of the role assigned to the new
-- @DevEndpoint@.
createDevEndpointResponse_roleArn :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_roleArn = Lens.lens (\CreateDevEndpointResponse' {roleArn} -> roleArn) (\s@CreateDevEndpointResponse' {} a -> s {roleArn = a} :: CreateDevEndpointResponse)

-- | The address of the YARN endpoint used by this @DevEndpoint@.
createDevEndpointResponse_yarnEndpointAddress :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_yarnEndpointAddress = Lens.lens (\CreateDevEndpointResponse' {yarnEndpointAddress} -> yarnEndpointAddress) (\s@CreateDevEndpointResponse' {} a -> s {yarnEndpointAddress = a} :: CreateDevEndpointResponse)

-- | The name of the @SecurityConfiguration@ structure being used with this
-- @DevEndpoint@.
createDevEndpointResponse_securityConfiguration :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_securityConfiguration = Lens.lens (\CreateDevEndpointResponse' {securityConfiguration} -> securityConfiguration) (\s@CreateDevEndpointResponse' {} a -> s {securityConfiguration = a} :: CreateDevEndpointResponse)

-- | The point in time at which this @DevEndpoint@ was created.
createDevEndpointResponse_createdTimestamp :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.UTCTime)
createDevEndpointResponse_createdTimestamp = Lens.lens (\CreateDevEndpointResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateDevEndpointResponse' {} a -> s {createdTimestamp = a} :: CreateDevEndpointResponse) Prelude.. Lens.mapping Core._Time

-- | The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
createDevEndpointResponse_numberOfWorkers :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Int)
createDevEndpointResponse_numberOfWorkers = Lens.lens (\CreateDevEndpointResponse' {numberOfWorkers} -> numberOfWorkers) (\s@CreateDevEndpointResponse' {} a -> s {numberOfWorkers = a} :: CreateDevEndpointResponse)

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Int)
createDevEndpointResponse_zeppelinRemoteSparkInterpreterPort = Lens.lens (\CreateDevEndpointResponse' {zeppelinRemoteSparkInterpreterPort} -> zeppelinRemoteSparkInterpreterPort) (\s@CreateDevEndpointResponse' {} a -> s {zeppelinRemoteSparkInterpreterPort = a} :: CreateDevEndpointResponse)

-- | The paths to one or more Python libraries in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
createDevEndpointResponse_extraPythonLibsS3Path :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_extraPythonLibsS3Path = Lens.lens (\CreateDevEndpointResponse' {extraPythonLibsS3Path} -> extraPythonLibsS3Path) (\s@CreateDevEndpointResponse' {} a -> s {extraPythonLibsS3Path = a} :: CreateDevEndpointResponse)

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
createDevEndpointResponse_availabilityZone :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_availabilityZone = Lens.lens (\CreateDevEndpointResponse' {availabilityZone} -> availabilityZone) (\s@CreateDevEndpointResponse' {} a -> s {availabilityZone = a} :: CreateDevEndpointResponse)

-- | The reason for a current failure in this @DevEndpoint@.
createDevEndpointResponse_failureReason :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_failureReason = Lens.lens (\CreateDevEndpointResponse' {failureReason} -> failureReason) (\s@CreateDevEndpointResponse' {} a -> s {failureReason = a} :: CreateDevEndpointResponse)

-- | Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
createDevEndpointResponse_glueVersion :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_glueVersion = Lens.lens (\CreateDevEndpointResponse' {glueVersion} -> glueVersion) (\s@CreateDevEndpointResponse' {} a -> s {glueVersion = a} :: CreateDevEndpointResponse)

-- | The number of Glue Data Processing Units (DPUs) allocated to this
-- DevEndpoint.
createDevEndpointResponse_numberOfNodes :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Int)
createDevEndpointResponse_numberOfNodes = Lens.lens (\CreateDevEndpointResponse' {numberOfNodes} -> numberOfNodes) (\s@CreateDevEndpointResponse' {} a -> s {numberOfNodes = a} :: CreateDevEndpointResponse)

-- | The type of predefined worker that is allocated to the development
-- endpoint. May be a value of Standard, G.1X, or G.2X.
createDevEndpointResponse_workerType :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe WorkerType)
createDevEndpointResponse_workerType = Lens.lens (\CreateDevEndpointResponse' {workerType} -> workerType) (\s@CreateDevEndpointResponse' {} a -> s {workerType = a} :: CreateDevEndpointResponse)

-- | The subnet ID assigned to the new @DevEndpoint@.
createDevEndpointResponse_subnetId :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_subnetId = Lens.lens (\CreateDevEndpointResponse' {subnetId} -> subnetId) (\s@CreateDevEndpointResponse' {} a -> s {subnetId = a} :: CreateDevEndpointResponse)

-- | The map of arguments used to configure this @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
createDevEndpointResponse_arguments :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDevEndpointResponse_arguments = Lens.lens (\CreateDevEndpointResponse' {arguments} -> arguments) (\s@CreateDevEndpointResponse' {} a -> s {arguments = a} :: CreateDevEndpointResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
createDevEndpointResponse_vpcId :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_vpcId = Lens.lens (\CreateDevEndpointResponse' {vpcId} -> vpcId) (\s@CreateDevEndpointResponse' {} a -> s {vpcId = a} :: CreateDevEndpointResponse)

-- | Path to one or more Java @.jar@ files in an S3 bucket that will be
-- loaded in your @DevEndpoint@.
createDevEndpointResponse_extraJarsS3Path :: Lens.Lens' CreateDevEndpointResponse (Prelude.Maybe Prelude.Text)
createDevEndpointResponse_extraJarsS3Path = Lens.lens (\CreateDevEndpointResponse' {extraJarsS3Path} -> extraJarsS3Path) (\s@CreateDevEndpointResponse' {} a -> s {extraJarsS3Path = a} :: CreateDevEndpointResponse)

-- | The response's http status code.
createDevEndpointResponse_httpStatus :: Lens.Lens' CreateDevEndpointResponse Prelude.Int
createDevEndpointResponse_httpStatus = Lens.lens (\CreateDevEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateDevEndpointResponse' {} a -> s {httpStatus = a} :: CreateDevEndpointResponse)

instance Prelude.NFData CreateDevEndpointResponse
