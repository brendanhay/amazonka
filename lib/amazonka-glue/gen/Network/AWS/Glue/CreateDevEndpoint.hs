{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    createDevEndpoint,
    CreateDevEndpoint,

    -- * Request Lenses
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
    cdeEndpointName,
    cdeRoleARN,

    -- * Destructuring the Response
    createDevEndpointResponse,
    CreateDevEndpointResponse,

    -- * Response Lenses
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDevEndpoint' smart constructor.
data CreateDevEndpoint = CreateDevEndpoint'
  { _cdeNumberOfWorkers ::
      !(Maybe Int),
    _cdeExtraPythonLibsS3Path :: !(Maybe Text),
    _cdeSecurityGroupIds :: !(Maybe [Text]),
    _cdePublicKeys :: !(Maybe [Text]),
    _cdeArguments :: !(Maybe (Map Text (Text))),
    _cdeWorkerType :: !(Maybe WorkerType),
    _cdeSecurityConfiguration :: !(Maybe Text),
    _cdePublicKey :: !(Maybe Text),
    _cdeSubnetId :: !(Maybe Text),
    _cdeGlueVersion :: !(Maybe Text),
    _cdeNumberOfNodes :: !(Maybe Int),
    _cdeExtraJARsS3Path :: !(Maybe Text),
    _cdeTags :: !(Maybe (Map Text (Text))),
    _cdeEndpointName :: !Text,
    _cdeRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDevEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdeNumberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to the development endpoint. The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
--
-- * 'cdeExtraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- * 'cdeSecurityGroupIds' - Security group IDs for the security groups to be used by the new @DevEndpoint@ .
--
-- * 'cdePublicKeys' - A list of public keys to be used by the development endpoints for authentication. The use of this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
--
-- * 'cdeArguments' - A map of arguments used to configure the @DevEndpoint@ .
--
-- * 'cdeWorkerType' - The type of predefined worker that is allocated to the development endpoint. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs. Known issue: when a development endpoint is created with the @G.2X@ @WorkerType@ configuration, the Spark drivers for the development endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
--
-- * 'cdeSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
--
-- * 'cdePublicKey' - The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
--
-- * 'cdeSubnetId' - The subnet ID for the new @DevEndpoint@ to use.
--
-- * 'cdeGlueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.  For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide. Development endpoints that are created without specifying a Glue version default to Glue 0.9. You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- * 'cdeNumberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) to allocate to this @DevEndpoint@ .
--
-- * 'cdeExtraJARsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
--
-- * 'cdeTags' - The tags to use with this DevEndpoint. You may use tags to limit access to the DevEndpoint. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- * 'cdeEndpointName' - The name to be assigned to the new @DevEndpoint@ .
--
-- * 'cdeRoleARN' - The IAM role for the @DevEndpoint@ .
createDevEndpoint ::
  -- | 'cdeEndpointName'
  Text ->
  -- | 'cdeRoleARN'
  Text ->
  CreateDevEndpoint
createDevEndpoint pEndpointName_ pRoleARN_ =
  CreateDevEndpoint'
    { _cdeNumberOfWorkers = Nothing,
      _cdeExtraPythonLibsS3Path = Nothing,
      _cdeSecurityGroupIds = Nothing,
      _cdePublicKeys = Nothing,
      _cdeArguments = Nothing,
      _cdeWorkerType = Nothing,
      _cdeSecurityConfiguration = Nothing,
      _cdePublicKey = Nothing,
      _cdeSubnetId = Nothing,
      _cdeGlueVersion = Nothing,
      _cdeNumberOfNodes = Nothing,
      _cdeExtraJARsS3Path = Nothing,
      _cdeTags = Nothing,
      _cdeEndpointName = pEndpointName_,
      _cdeRoleARN = pRoleARN_
    }

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint. The maximum number of workers you can define are 299 for @G.1X@ , and 149 for @G.2X@ .
cdeNumberOfWorkers :: Lens' CreateDevEndpoint (Maybe Int)
cdeNumberOfWorkers = lens _cdeNumberOfWorkers (\s a -> s {_cdeNumberOfWorkers = a})

-- | The paths to one or more Python libraries in an Amazon S3 bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
cdeExtraPythonLibsS3Path :: Lens' CreateDevEndpoint (Maybe Text)
cdeExtraPythonLibsS3Path = lens _cdeExtraPythonLibsS3Path (\s a -> s {_cdeExtraPythonLibsS3Path = a})

-- | Security group IDs for the security groups to be used by the new @DevEndpoint@ .
cdeSecurityGroupIds :: Lens' CreateDevEndpoint [Text]
cdeSecurityGroupIds = lens _cdeSecurityGroupIds (\s a -> s {_cdeSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of public keys to be used by the development endpoints for authentication. The use of this attribute is preferred over a single public key because the public keys allow you to have a different private key per client.
cdePublicKeys :: Lens' CreateDevEndpoint [Text]
cdePublicKeys = lens _cdePublicKeys (\s a -> s {_cdePublicKeys = a}) . _Default . _Coerce

-- | A map of arguments used to configure the @DevEndpoint@ .
cdeArguments :: Lens' CreateDevEndpoint (HashMap Text (Text))
cdeArguments = lens _cdeArguments (\s a -> s {_cdeArguments = a}) . _Default . _Map

-- | The type of predefined worker that is allocated to the development endpoint. Accepts a value of Standard, G.1X, or G.2X.     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.     * For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB of memory, 64 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs.     * For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB of memory, 128 GB disk), and provides 1 executor per worker. We recommend this worker type for memory-intensive jobs. Known issue: when a development endpoint is created with the @G.2X@ @WorkerType@ configuration, the Spark drivers for the development endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
cdeWorkerType :: Lens' CreateDevEndpoint (Maybe WorkerType)
cdeWorkerType = lens _cdeWorkerType (\s a -> s {_cdeWorkerType = a})

-- | The name of the @SecurityConfiguration@ structure to be used with this @DevEndpoint@ .
cdeSecurityConfiguration :: Lens' CreateDevEndpoint (Maybe Text)
cdeSecurityConfiguration = lens _cdeSecurityConfiguration (\s a -> s {_cdeSecurityConfiguration = a})

-- | The public key to be used by this @DevEndpoint@ for authentication. This attribute is provided for backward compatibility because the recommended attribute to use is public keys.
cdePublicKey :: Lens' CreateDevEndpoint (Maybe Text)
cdePublicKey = lens _cdePublicKey (\s a -> s {_cdePublicKey = a})

-- | The subnet ID for the new @DevEndpoint@ to use.
cdeSubnetId :: Lens' CreateDevEndpoint (Maybe Text)
cdeSubnetId = lens _cdeSubnetId (\s a -> s {_cdeSubnetId = a})

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.  For more information about the available AWS Glue versions and corresponding Spark and Python versions, see <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version> in the developer guide. Development endpoints that are created without specifying a Glue version default to Glue 0.9. You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
cdeGlueVersion :: Lens' CreateDevEndpoint (Maybe Text)
cdeGlueVersion = lens _cdeGlueVersion (\s a -> s {_cdeGlueVersion = a})

-- | The number of AWS Glue Data Processing Units (DPUs) to allocate to this @DevEndpoint@ .
cdeNumberOfNodes :: Lens' CreateDevEndpoint (Maybe Int)
cdeNumberOfNodes = lens _cdeNumberOfNodes (\s a -> s {_cdeNumberOfNodes = a})

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
cdeExtraJARsS3Path :: Lens' CreateDevEndpoint (Maybe Text)
cdeExtraJARsS3Path = lens _cdeExtraJARsS3Path (\s a -> s {_cdeExtraJARsS3Path = a})

-- | The tags to use with this DevEndpoint. You may use tags to limit access to the DevEndpoint. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
cdeTags :: Lens' CreateDevEndpoint (HashMap Text (Text))
cdeTags = lens _cdeTags (\s a -> s {_cdeTags = a}) . _Default . _Map

-- | The name to be assigned to the new @DevEndpoint@ .
cdeEndpointName :: Lens' CreateDevEndpoint Text
cdeEndpointName = lens _cdeEndpointName (\s a -> s {_cdeEndpointName = a})

-- | The IAM role for the @DevEndpoint@ .
cdeRoleARN :: Lens' CreateDevEndpoint Text
cdeRoleARN = lens _cdeRoleARN (\s a -> s {_cdeRoleARN = a})

instance AWSRequest CreateDevEndpoint where
  type Rs CreateDevEndpoint = CreateDevEndpointResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          CreateDevEndpointResponse'
            <$> (x .?> "Status")
            <*> (x .?> "FailureReason")
            <*> (x .?> "EndpointName")
            <*> (x .?> "NumberOfWorkers")
            <*> (x .?> "ExtraPythonLibsS3Path")
            <*> (x .?> "SecurityGroupIds" .!@ mempty)
            <*> (x .?> "VpcId")
            <*> (x .?> "Arguments" .!@ mempty)
            <*> (x .?> "WorkerType")
            <*> (x .?> "SecurityConfiguration")
            <*> (x .?> "SubnetId")
            <*> (x .?> "GlueVersion")
            <*> (x .?> "NumberOfNodes")
            <*> (x .?> "AvailabilityZone")
            <*> (x .?> "ZeppelinRemoteSparkInterpreterPort")
            <*> (x .?> "ExtraJarsS3Path")
            <*> (x .?> "CreatedTimestamp")
            <*> (x .?> "YarnEndpointAddress")
            <*> (x .?> "RoleArn")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateDevEndpoint

instance NFData CreateDevEndpoint

instance ToHeaders CreateDevEndpoint where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.CreateDevEndpoint" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDevEndpoint where
  toJSON CreateDevEndpoint' {..} =
    object
      ( catMaybes
          [ ("NumberOfWorkers" .=) <$> _cdeNumberOfWorkers,
            ("ExtraPythonLibsS3Path" .=) <$> _cdeExtraPythonLibsS3Path,
            ("SecurityGroupIds" .=) <$> _cdeSecurityGroupIds,
            ("PublicKeys" .=) <$> _cdePublicKeys,
            ("Arguments" .=) <$> _cdeArguments,
            ("WorkerType" .=) <$> _cdeWorkerType,
            ("SecurityConfiguration" .=) <$> _cdeSecurityConfiguration,
            ("PublicKey" .=) <$> _cdePublicKey,
            ("SubnetId" .=) <$> _cdeSubnetId,
            ("GlueVersion" .=) <$> _cdeGlueVersion,
            ("NumberOfNodes" .=) <$> _cdeNumberOfNodes,
            ("ExtraJarsS3Path" .=) <$> _cdeExtraJARsS3Path,
            ("Tags" .=) <$> _cdeTags,
            Just ("EndpointName" .= _cdeEndpointName),
            Just ("RoleArn" .= _cdeRoleARN)
          ]
      )

instance ToPath CreateDevEndpoint where
  toPath = const "/"

instance ToQuery CreateDevEndpoint where
  toQuery = const mempty

-- | /See:/ 'createDevEndpointResponse' smart constructor.
data CreateDevEndpointResponse = CreateDevEndpointResponse'
  { _cdersStatus ::
      !(Maybe Text),
    _cdersFailureReason :: !(Maybe Text),
    _cdersEndpointName :: !(Maybe Text),
    _cdersNumberOfWorkers :: !(Maybe Int),
    _cdersExtraPythonLibsS3Path ::
      !(Maybe Text),
    _cdersSecurityGroupIds ::
      !(Maybe [Text]),
    _cdersVPCId :: !(Maybe Text),
    _cdersArguments ::
      !(Maybe (Map Text (Text))),
    _cdersWorkerType :: !(Maybe WorkerType),
    _cdersSecurityConfiguration ::
      !(Maybe Text),
    _cdersSubnetId :: !(Maybe Text),
    _cdersGlueVersion :: !(Maybe Text),
    _cdersNumberOfNodes :: !(Maybe Int),
    _cdersAvailabilityZone :: !(Maybe Text),
    _cdersZeppelinRemoteSparkInterpreterPort ::
      !(Maybe Int),
    _cdersExtraJARsS3Path :: !(Maybe Text),
    _cdersCreatedTimestamp ::
      !(Maybe POSIX),
    _cdersYarnEndpointAddress ::
      !(Maybe Text),
    _cdersRoleARN :: !(Maybe Text),
    _cdersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDevEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdersStatus' - The current status of the new @DevEndpoint@ .
--
-- * 'cdersFailureReason' - The reason for a current failure in this @DevEndpoint@ .
--
-- * 'cdersEndpointName' - The name assigned to the new @DevEndpoint@ .
--
-- * 'cdersNumberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to the development endpoint.
--
-- * 'cdersExtraPythonLibsS3Path' - The paths to one or more Python libraries in an S3 bucket that will be loaded in your @DevEndpoint@ .
--
-- * 'cdersSecurityGroupIds' - The security groups assigned to the new @DevEndpoint@ .
--
-- * 'cdersVPCId' - The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
--
-- * 'cdersArguments' - The map of arguments used to configure this @DevEndpoint@ . Valid arguments are:     * @"--enable-glue-datacatalog": ""@      * @"GLUE_PYTHON_VERSION": "3"@      * @"GLUE_PYTHON_VERSION": "2"@  You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- * 'cdersWorkerType' - The type of predefined worker that is allocated to the development endpoint. May be a value of Standard, G.1X, or G.2X.
--
-- * 'cdersSecurityConfiguration' - The name of the @SecurityConfiguration@ structure being used with this @DevEndpoint@ .
--
-- * 'cdersSubnetId' - The subnet ID assigned to the new @DevEndpoint@ .
--
-- * 'cdersGlueVersion' - Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
--
-- * 'cdersNumberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
--
-- * 'cdersAvailabilityZone' - The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- * 'cdersZeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- * 'cdersExtraJARsS3Path' - Path to one or more Java @.jar@ files in an S3 bucket that will be loaded in your @DevEndpoint@ .
--
-- * 'cdersCreatedTimestamp' - The point in time at which this @DevEndpoint@ was created.
--
-- * 'cdersYarnEndpointAddress' - The address of the YARN endpoint used by this @DevEndpoint@ .
--
-- * 'cdersRoleARN' - The Amazon Resource Name (ARN) of the role assigned to the new @DevEndpoint@ .
--
-- * 'cdersResponseStatus' - -- | The response status code.
createDevEndpointResponse ::
  -- | 'cdersResponseStatus'
  Int ->
  CreateDevEndpointResponse
createDevEndpointResponse pResponseStatus_ =
  CreateDevEndpointResponse'
    { _cdersStatus = Nothing,
      _cdersFailureReason = Nothing,
      _cdersEndpointName = Nothing,
      _cdersNumberOfWorkers = Nothing,
      _cdersExtraPythonLibsS3Path = Nothing,
      _cdersSecurityGroupIds = Nothing,
      _cdersVPCId = Nothing,
      _cdersArguments = Nothing,
      _cdersWorkerType = Nothing,
      _cdersSecurityConfiguration = Nothing,
      _cdersSubnetId = Nothing,
      _cdersGlueVersion = Nothing,
      _cdersNumberOfNodes = Nothing,
      _cdersAvailabilityZone = Nothing,
      _cdersZeppelinRemoteSparkInterpreterPort = Nothing,
      _cdersExtraJARsS3Path = Nothing,
      _cdersCreatedTimestamp = Nothing,
      _cdersYarnEndpointAddress = Nothing,
      _cdersRoleARN = Nothing,
      _cdersResponseStatus = pResponseStatus_
    }

-- | The current status of the new @DevEndpoint@ .
cdersStatus :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersStatus = lens _cdersStatus (\s a -> s {_cdersStatus = a})

-- | The reason for a current failure in this @DevEndpoint@ .
cdersFailureReason :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersFailureReason = lens _cdersFailureReason (\s a -> s {_cdersFailureReason = a})

-- | The name assigned to the new @DevEndpoint@ .
cdersEndpointName :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersEndpointName = lens _cdersEndpointName (\s a -> s {_cdersEndpointName = a})

-- | The number of workers of a defined @workerType@ that are allocated to the development endpoint.
cdersNumberOfWorkers :: Lens' CreateDevEndpointResponse (Maybe Int)
cdersNumberOfWorkers = lens _cdersNumberOfWorkers (\s a -> s {_cdersNumberOfWorkers = a})

-- | The paths to one or more Python libraries in an S3 bucket that will be loaded in your @DevEndpoint@ .
cdersExtraPythonLibsS3Path :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersExtraPythonLibsS3Path = lens _cdersExtraPythonLibsS3Path (\s a -> s {_cdersExtraPythonLibsS3Path = a})

-- | The security groups assigned to the new @DevEndpoint@ .
cdersSecurityGroupIds :: Lens' CreateDevEndpointResponse [Text]
cdersSecurityGroupIds = lens _cdersSecurityGroupIds (\s a -> s {_cdersSecurityGroupIds = a}) . _Default . _Coerce

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@ .
cdersVPCId :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersVPCId = lens _cdersVPCId (\s a -> s {_cdersVPCId = a})

-- | The map of arguments used to configure this @DevEndpoint@ . Valid arguments are:     * @"--enable-glue-datacatalog": ""@      * @"GLUE_PYTHON_VERSION": "3"@      * @"GLUE_PYTHON_VERSION": "2"@  You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
cdersArguments :: Lens' CreateDevEndpointResponse (HashMap Text (Text))
cdersArguments = lens _cdersArguments (\s a -> s {_cdersArguments = a}) . _Default . _Map

-- | The type of predefined worker that is allocated to the development endpoint. May be a value of Standard, G.1X, or G.2X.
cdersWorkerType :: Lens' CreateDevEndpointResponse (Maybe WorkerType)
cdersWorkerType = lens _cdersWorkerType (\s a -> s {_cdersWorkerType = a})

-- | The name of the @SecurityConfiguration@ structure being used with this @DevEndpoint@ .
cdersSecurityConfiguration :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersSecurityConfiguration = lens _cdersSecurityConfiguration (\s a -> s {_cdersSecurityConfiguration = a})

-- | The subnet ID assigned to the new @DevEndpoint@ .
cdersSubnetId :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersSubnetId = lens _cdersSubnetId (\s a -> s {_cdersSubnetId = a})

-- | Glue version determines the versions of Apache Spark and Python that AWS Glue supports. The Python version indicates the version supported for running your ETL scripts on development endpoints.
cdersGlueVersion :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersGlueVersion = lens _cdersGlueVersion (\s a -> s {_cdersGlueVersion = a})

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
cdersNumberOfNodes :: Lens' CreateDevEndpointResponse (Maybe Int)
cdersNumberOfNodes = lens _cdersNumberOfNodes (\s a -> s {_cdersNumberOfNodes = a})

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
cdersAvailabilityZone :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersAvailabilityZone = lens _cdersAvailabilityZone (\s a -> s {_cdersAvailabilityZone = a})

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
cdersZeppelinRemoteSparkInterpreterPort :: Lens' CreateDevEndpointResponse (Maybe Int)
cdersZeppelinRemoteSparkInterpreterPort = lens _cdersZeppelinRemoteSparkInterpreterPort (\s a -> s {_cdersZeppelinRemoteSparkInterpreterPort = a})

-- | Path to one or more Java @.jar@ files in an S3 bucket that will be loaded in your @DevEndpoint@ .
cdersExtraJARsS3Path :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersExtraJARsS3Path = lens _cdersExtraJARsS3Path (\s a -> s {_cdersExtraJARsS3Path = a})

-- | The point in time at which this @DevEndpoint@ was created.
cdersCreatedTimestamp :: Lens' CreateDevEndpointResponse (Maybe UTCTime)
cdersCreatedTimestamp = lens _cdersCreatedTimestamp (\s a -> s {_cdersCreatedTimestamp = a}) . mapping _Time

-- | The address of the YARN endpoint used by this @DevEndpoint@ .
cdersYarnEndpointAddress :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersYarnEndpointAddress = lens _cdersYarnEndpointAddress (\s a -> s {_cdersYarnEndpointAddress = a})

-- | The Amazon Resource Name (ARN) of the role assigned to the new @DevEndpoint@ .
cdersRoleARN :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersRoleARN = lens _cdersRoleARN (\s a -> s {_cdersRoleARN = a})

-- | -- | The response status code.
cdersResponseStatus :: Lens' CreateDevEndpointResponse Int
cdersResponseStatus = lens _cdersResponseStatus (\s a -> s {_cdersResponseStatus = a})

instance NFData CreateDevEndpointResponse
