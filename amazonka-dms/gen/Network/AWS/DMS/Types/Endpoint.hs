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
-- Module      : Network.AWS.DMS.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Endpoint where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.DmsSslModeValue
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DocDbSettings
import Network.AWS.DMS.Types.DynamoDbSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.IBMDb2Settings
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MicrosoftSQLServerSettings
import Network.AWS.DMS.Types.MongoDbSettings
import Network.AWS.DMS.Types.MySQLSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.OracleSettings
import Network.AWS.DMS.Types.PostgreSQLSettings
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import Network.AWS.DMS.Types.S3Settings
import Network.AWS.DMS.Types.SybaseSettings
import qualified Network.AWS.Lens as Lens

-- | Describes an endpoint of a database instance in response to operations
-- such as the following:
--
-- -   @CreateEndpoint@
--
-- -   @DescribeEndpoint@
--
-- -   @DescribeEndpointTypes@
--
-- -   @ModifyEndpoint@
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The SSL mode used to connect to the endpoint. The default value is
    -- @none@.
    sslMode :: Core.Maybe DmsSslModeValue,
    -- | The settings for the MongoDB source endpoint. For more information, see
    -- the @MongoDbSettings@ structure.
    mongoDbSettings :: Core.Maybe MongoDbSettings,
    -- | The status of the endpoint.
    status :: Core.Maybe Core.Text,
    -- | The settings for the Amazon Neptune target endpoint. For more
    -- information, see the @NeptuneSettings@ structure.
    neptuneSettings :: Core.Maybe NeptuneSettings,
    -- | The database engine name. Valid values, depending on the EndpointType,
    -- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
    -- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
    -- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
    -- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
    -- @\"sqlserver\"@, and @\"neptune\"@.
    engineName :: Core.Maybe Core.Text,
    -- | The settings for the Elasticsearch source endpoint. For more
    -- information, see the @ElasticsearchSettings@ structure.
    elasticsearchSettings :: Core.Maybe ElasticsearchSettings,
    -- | The external table definition.
    externalTableDefinition :: Core.Maybe Core.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Core.Maybe ReplicationEndpointTypeValue,
    -- | The settings for the Oracle source and target endpoint. For more
    -- information, see the @OracleSettings@ structure.
    oracleSettings :: Core.Maybe OracleSettings,
    -- | The settings for the PostgreSQL source and target endpoint. For more
    -- information, see the @PostgreSQLSettings@ structure.
    postgreSQLSettings :: Core.Maybe PostgreSQLSettings,
    -- | The Amazon Resource Name (ARN) used by the service access IAM role.
    serviceAccessRoleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
    certificateArn :: Core.Maybe Core.Text,
    -- | The settings for the S3 target endpoint. For more information, see the
    -- @S3Settings@ structure.
    s3Settings :: Core.Maybe S3Settings,
    -- | The name of the server at the endpoint.
    serverName :: Core.Maybe Core.Text,
    -- | The settings for the Microsoft SQL Server source and target endpoint.
    -- For more information, see the @MicrosoftSQLServerSettings@ structure.
    microsoftSQLServerSettings :: Core.Maybe MicrosoftSQLServerSettings,
    -- | An AWS KMS key identifier that is used to encrypt the connection
    -- parameters for the endpoint.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
    -- uses your default encryption key.
    --
    -- AWS KMS creates the default encryption key for your AWS account. Your
    -- AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The settings for the IBM Db2 LUW source endpoint. For more information,
    -- see the @IBMDb2Settings@ structure.
    iBMDb2Settings :: Core.Maybe IBMDb2Settings,
    -- | The settings for the MySQL source and target endpoint. For more
    -- information, see the @MySQLSettings@ structure.
    mySQLSettings :: Core.Maybe MySQLSettings,
    -- | The settings in JSON format for the DMS transfer type of source
    -- endpoint.
    --
    -- Possible settings include the following:
    --
    -- -   @ServiceAccessRoleArn@ - The IAM role that has permission to access
    --     the Amazon S3 bucket.
    --
    -- -   @BucketName@ - The name of the S3 bucket to use.
    --
    -- -   @CompressionType@ - An optional parameter to use GZIP to compress
    --     the target files. To use GZIP, set this value to @NONE@ (the
    --     default). To keep the files uncompressed, don\'t use this value.
    --
    -- Shorthand syntax for these settings is as follows:
    -- @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@
    --
    -- JSON syntax for these settings is as follows:
    -- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", \"CompressionType\": \"none\"|\"gzip\" } @
    dmsTransferSettings :: Core.Maybe DmsTransferSettings,
    -- | The port value used to access the endpoint.
    port :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Core.Maybe Core.Text,
    -- | Settings for the Amazon Redshift endpoint.
    redshiftSettings :: Core.Maybe RedshiftSettings,
    -- | The user name used to connect to the endpoint.
    username :: Core.Maybe Core.Text,
    -- | The expanded name for the engine name. For example, if the @EngineName@
    -- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
    engineDisplayName :: Core.Maybe Core.Text,
    -- | The settings for the Apache Kafka target endpoint. For more information,
    -- see the @KafkaSettings@ structure.
    kafkaSettings :: Core.Maybe KafkaSettings,
    docDbSettings :: Core.Maybe DocDbSettings,
    -- | The settings for the DynamoDB target endpoint. For more information, see
    -- the @DynamoDBSettings@ structure.
    dynamoDbSettings :: Core.Maybe DynamoDbSettings,
    -- | Additional connection attributes used to connect to the endpoint.
    extraConnectionAttributes :: Core.Maybe Core.Text,
    -- | Value returned by a call to CreateEndpoint that can be used for
    -- cross-account validation. Use it on a subsequent call to CreateEndpoint
    -- to create the endpoint with a cross-account.
    externalId :: Core.Maybe Core.Text,
    -- | The database endpoint identifier. Identifiers must begin with a letter
    -- and must contain only ASCII letters, digits, and hyphens. They can\'t
    -- end with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Core.Maybe Core.Text,
    -- | The settings for the Amazon Kinesis target endpoint. For more
    -- information, see the @KinesisSettings@ structure.
    kinesisSettings :: Core.Maybe KinesisSettings,
    -- | The settings for the SAP ASE source and target endpoint. For more
    -- information, see the @SybaseSettings@ structure.
    sybaseSettings :: Core.Maybe SybaseSettings,
    -- | The name of the database at the endpoint.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslMode', 'endpoint_sslMode' - The SSL mode used to connect to the endpoint. The default value is
-- @none@.
--
-- 'mongoDbSettings', 'endpoint_mongoDbSettings' - The settings for the MongoDB source endpoint. For more information, see
-- the @MongoDbSettings@ structure.
--
-- 'status', 'endpoint_status' - The status of the endpoint.
--
-- 'neptuneSettings', 'endpoint_neptuneSettings' - The settings for the Amazon Neptune target endpoint. For more
-- information, see the @NeptuneSettings@ structure.
--
-- 'engineName', 'endpoint_engineName' - The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
--
-- 'elasticsearchSettings', 'endpoint_elasticsearchSettings' - The settings for the Elasticsearch source endpoint. For more
-- information, see the @ElasticsearchSettings@ structure.
--
-- 'externalTableDefinition', 'endpoint_externalTableDefinition' - The external table definition.
--
-- 'endpointType', 'endpoint_endpointType' - The type of endpoint. Valid values are @source@ and @target@.
--
-- 'oracleSettings', 'endpoint_oracleSettings' - The settings for the Oracle source and target endpoint. For more
-- information, see the @OracleSettings@ structure.
--
-- 'postgreSQLSettings', 'endpoint_postgreSQLSettings' - The settings for the PostgreSQL source and target endpoint. For more
-- information, see the @PostgreSQLSettings@ structure.
--
-- 'serviceAccessRoleArn', 'endpoint_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service access IAM role.
--
-- 'certificateArn', 'endpoint_certificateArn' - The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
--
-- 's3Settings', 'endpoint_s3Settings' - The settings for the S3 target endpoint. For more information, see the
-- @S3Settings@ structure.
--
-- 'serverName', 'endpoint_serverName' - The name of the server at the endpoint.
--
-- 'microsoftSQLServerSettings', 'endpoint_microsoftSQLServerSettings' - The settings for the Microsoft SQL Server source and target endpoint.
-- For more information, see the @MicrosoftSQLServerSettings@ structure.
--
-- 'kmsKeyId', 'endpoint_kmsKeyId' - An AWS KMS key identifier that is used to encrypt the connection
-- parameters for the endpoint.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
--
-- 'iBMDb2Settings', 'endpoint_iBMDb2Settings' - The settings for the IBM Db2 LUW source endpoint. For more information,
-- see the @IBMDb2Settings@ structure.
--
-- 'mySQLSettings', 'endpoint_mySQLSettings' - The settings for the MySQL source and target endpoint. For more
-- information, see the @MySQLSettings@ structure.
--
-- 'dmsTransferSettings', 'endpoint_dmsTransferSettings' - The settings in JSON format for the DMS transfer type of source
-- endpoint.
--
-- Possible settings include the following:
--
-- -   @ServiceAccessRoleArn@ - The IAM role that has permission to access
--     the Amazon S3 bucket.
--
-- -   @BucketName@ - The name of the S3 bucket to use.
--
-- -   @CompressionType@ - An optional parameter to use GZIP to compress
--     the target files. To use GZIP, set this value to @NONE@ (the
--     default). To keep the files uncompressed, don\'t use this value.
--
-- Shorthand syntax for these settings is as follows:
-- @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@
--
-- JSON syntax for these settings is as follows:
-- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", \"CompressionType\": \"none\"|\"gzip\" } @
--
-- 'port', 'endpoint_port' - The port value used to access the endpoint.
--
-- 'endpointArn', 'endpoint_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
--
-- 'redshiftSettings', 'endpoint_redshiftSettings' - Settings for the Amazon Redshift endpoint.
--
-- 'username', 'endpoint_username' - The user name used to connect to the endpoint.
--
-- 'engineDisplayName', 'endpoint_engineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
--
-- 'kafkaSettings', 'endpoint_kafkaSettings' - The settings for the Apache Kafka target endpoint. For more information,
-- see the @KafkaSettings@ structure.
--
-- 'docDbSettings', 'endpoint_docDbSettings' - Undocumented member.
--
-- 'dynamoDbSettings', 'endpoint_dynamoDbSettings' - The settings for the DynamoDB target endpoint. For more information, see
-- the @DynamoDBSettings@ structure.
--
-- 'extraConnectionAttributes', 'endpoint_extraConnectionAttributes' - Additional connection attributes used to connect to the endpoint.
--
-- 'externalId', 'endpoint_externalId' - Value returned by a call to CreateEndpoint that can be used for
-- cross-account validation. Use it on a subsequent call to CreateEndpoint
-- to create the endpoint with a cross-account.
--
-- 'endpointIdentifier', 'endpoint_endpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
--
-- 'kinesisSettings', 'endpoint_kinesisSettings' - The settings for the Amazon Kinesis target endpoint. For more
-- information, see the @KinesisSettings@ structure.
--
-- 'sybaseSettings', 'endpoint_sybaseSettings' - The settings for the SAP ASE source and target endpoint. For more
-- information, see the @SybaseSettings@ structure.
--
-- 'databaseName', 'endpoint_databaseName' - The name of the database at the endpoint.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { sslMode = Core.Nothing,
      mongoDbSettings = Core.Nothing,
      status = Core.Nothing,
      neptuneSettings = Core.Nothing,
      engineName = Core.Nothing,
      elasticsearchSettings = Core.Nothing,
      externalTableDefinition = Core.Nothing,
      endpointType = Core.Nothing,
      oracleSettings = Core.Nothing,
      postgreSQLSettings = Core.Nothing,
      serviceAccessRoleArn = Core.Nothing,
      certificateArn = Core.Nothing,
      s3Settings = Core.Nothing,
      serverName = Core.Nothing,
      microsoftSQLServerSettings = Core.Nothing,
      kmsKeyId = Core.Nothing,
      iBMDb2Settings = Core.Nothing,
      mySQLSettings = Core.Nothing,
      dmsTransferSettings = Core.Nothing,
      port = Core.Nothing,
      endpointArn = Core.Nothing,
      redshiftSettings = Core.Nothing,
      username = Core.Nothing,
      engineDisplayName = Core.Nothing,
      kafkaSettings = Core.Nothing,
      docDbSettings = Core.Nothing,
      dynamoDbSettings = Core.Nothing,
      extraConnectionAttributes = Core.Nothing,
      externalId = Core.Nothing,
      endpointIdentifier = Core.Nothing,
      kinesisSettings = Core.Nothing,
      sybaseSettings = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The SSL mode used to connect to the endpoint. The default value is
-- @none@.
endpoint_sslMode :: Lens.Lens' Endpoint (Core.Maybe DmsSslModeValue)
endpoint_sslMode = Lens.lens (\Endpoint' {sslMode} -> sslMode) (\s@Endpoint' {} a -> s {sslMode = a} :: Endpoint)

-- | The settings for the MongoDB source endpoint. For more information, see
-- the @MongoDbSettings@ structure.
endpoint_mongoDbSettings :: Lens.Lens' Endpoint (Core.Maybe MongoDbSettings)
endpoint_mongoDbSettings = Lens.lens (\Endpoint' {mongoDbSettings} -> mongoDbSettings) (\s@Endpoint' {} a -> s {mongoDbSettings = a} :: Endpoint)

-- | The status of the endpoint.
endpoint_status :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_status = Lens.lens (\Endpoint' {status} -> status) (\s@Endpoint' {} a -> s {status = a} :: Endpoint)

-- | The settings for the Amazon Neptune target endpoint. For more
-- information, see the @NeptuneSettings@ structure.
endpoint_neptuneSettings :: Lens.Lens' Endpoint (Core.Maybe NeptuneSettings)
endpoint_neptuneSettings = Lens.lens (\Endpoint' {neptuneSettings} -> neptuneSettings) (\s@Endpoint' {} a -> s {neptuneSettings = a} :: Endpoint)

-- | The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
endpoint_engineName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_engineName = Lens.lens (\Endpoint' {engineName} -> engineName) (\s@Endpoint' {} a -> s {engineName = a} :: Endpoint)

-- | The settings for the Elasticsearch source endpoint. For more
-- information, see the @ElasticsearchSettings@ structure.
endpoint_elasticsearchSettings :: Lens.Lens' Endpoint (Core.Maybe ElasticsearchSettings)
endpoint_elasticsearchSettings = Lens.lens (\Endpoint' {elasticsearchSettings} -> elasticsearchSettings) (\s@Endpoint' {} a -> s {elasticsearchSettings = a} :: Endpoint)

-- | The external table definition.
endpoint_externalTableDefinition :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_externalTableDefinition = Lens.lens (\Endpoint' {externalTableDefinition} -> externalTableDefinition) (\s@Endpoint' {} a -> s {externalTableDefinition = a} :: Endpoint)

-- | The type of endpoint. Valid values are @source@ and @target@.
endpoint_endpointType :: Lens.Lens' Endpoint (Core.Maybe ReplicationEndpointTypeValue)
endpoint_endpointType = Lens.lens (\Endpoint' {endpointType} -> endpointType) (\s@Endpoint' {} a -> s {endpointType = a} :: Endpoint)

-- | The settings for the Oracle source and target endpoint. For more
-- information, see the @OracleSettings@ structure.
endpoint_oracleSettings :: Lens.Lens' Endpoint (Core.Maybe OracleSettings)
endpoint_oracleSettings = Lens.lens (\Endpoint' {oracleSettings} -> oracleSettings) (\s@Endpoint' {} a -> s {oracleSettings = a} :: Endpoint)

-- | The settings for the PostgreSQL source and target endpoint. For more
-- information, see the @PostgreSQLSettings@ structure.
endpoint_postgreSQLSettings :: Lens.Lens' Endpoint (Core.Maybe PostgreSQLSettings)
endpoint_postgreSQLSettings = Lens.lens (\Endpoint' {postgreSQLSettings} -> postgreSQLSettings) (\s@Endpoint' {} a -> s {postgreSQLSettings = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
endpoint_serviceAccessRoleArn :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_serviceAccessRoleArn = Lens.lens (\Endpoint' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@Endpoint' {} a -> s {serviceAccessRoleArn = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
endpoint_certificateArn :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_certificateArn = Lens.lens (\Endpoint' {certificateArn} -> certificateArn) (\s@Endpoint' {} a -> s {certificateArn = a} :: Endpoint)

-- | The settings for the S3 target endpoint. For more information, see the
-- @S3Settings@ structure.
endpoint_s3Settings :: Lens.Lens' Endpoint (Core.Maybe S3Settings)
endpoint_s3Settings = Lens.lens (\Endpoint' {s3Settings} -> s3Settings) (\s@Endpoint' {} a -> s {s3Settings = a} :: Endpoint)

-- | The name of the server at the endpoint.
endpoint_serverName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_serverName = Lens.lens (\Endpoint' {serverName} -> serverName) (\s@Endpoint' {} a -> s {serverName = a} :: Endpoint)

-- | The settings for the Microsoft SQL Server source and target endpoint.
-- For more information, see the @MicrosoftSQLServerSettings@ structure.
endpoint_microsoftSQLServerSettings :: Lens.Lens' Endpoint (Core.Maybe MicrosoftSQLServerSettings)
endpoint_microsoftSQLServerSettings = Lens.lens (\Endpoint' {microsoftSQLServerSettings} -> microsoftSQLServerSettings) (\s@Endpoint' {} a -> s {microsoftSQLServerSettings = a} :: Endpoint)

-- | An AWS KMS key identifier that is used to encrypt the connection
-- parameters for the endpoint.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
endpoint_kmsKeyId :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_kmsKeyId = Lens.lens (\Endpoint' {kmsKeyId} -> kmsKeyId) (\s@Endpoint' {} a -> s {kmsKeyId = a} :: Endpoint)

-- | The settings for the IBM Db2 LUW source endpoint. For more information,
-- see the @IBMDb2Settings@ structure.
endpoint_iBMDb2Settings :: Lens.Lens' Endpoint (Core.Maybe IBMDb2Settings)
endpoint_iBMDb2Settings = Lens.lens (\Endpoint' {iBMDb2Settings} -> iBMDb2Settings) (\s@Endpoint' {} a -> s {iBMDb2Settings = a} :: Endpoint)

-- | The settings for the MySQL source and target endpoint. For more
-- information, see the @MySQLSettings@ structure.
endpoint_mySQLSettings :: Lens.Lens' Endpoint (Core.Maybe MySQLSettings)
endpoint_mySQLSettings = Lens.lens (\Endpoint' {mySQLSettings} -> mySQLSettings) (\s@Endpoint' {} a -> s {mySQLSettings = a} :: Endpoint)

-- | The settings in JSON format for the DMS transfer type of source
-- endpoint.
--
-- Possible settings include the following:
--
-- -   @ServiceAccessRoleArn@ - The IAM role that has permission to access
--     the Amazon S3 bucket.
--
-- -   @BucketName@ - The name of the S3 bucket to use.
--
-- -   @CompressionType@ - An optional parameter to use GZIP to compress
--     the target files. To use GZIP, set this value to @NONE@ (the
--     default). To keep the files uncompressed, don\'t use this value.
--
-- Shorthand syntax for these settings is as follows:
-- @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@
--
-- JSON syntax for these settings is as follows:
-- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", \"CompressionType\": \"none\"|\"gzip\" } @
endpoint_dmsTransferSettings :: Lens.Lens' Endpoint (Core.Maybe DmsTransferSettings)
endpoint_dmsTransferSettings = Lens.lens (\Endpoint' {dmsTransferSettings} -> dmsTransferSettings) (\s@Endpoint' {} a -> s {dmsTransferSettings = a} :: Endpoint)

-- | The port value used to access the endpoint.
endpoint_port :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
endpoint_endpointArn :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_endpointArn = Lens.lens (\Endpoint' {endpointArn} -> endpointArn) (\s@Endpoint' {} a -> s {endpointArn = a} :: Endpoint)

-- | Settings for the Amazon Redshift endpoint.
endpoint_redshiftSettings :: Lens.Lens' Endpoint (Core.Maybe RedshiftSettings)
endpoint_redshiftSettings = Lens.lens (\Endpoint' {redshiftSettings} -> redshiftSettings) (\s@Endpoint' {} a -> s {redshiftSettings = a} :: Endpoint)

-- | The user name used to connect to the endpoint.
endpoint_username :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_username = Lens.lens (\Endpoint' {username} -> username) (\s@Endpoint' {} a -> s {username = a} :: Endpoint)

-- | The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
endpoint_engineDisplayName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_engineDisplayName = Lens.lens (\Endpoint' {engineDisplayName} -> engineDisplayName) (\s@Endpoint' {} a -> s {engineDisplayName = a} :: Endpoint)

-- | The settings for the Apache Kafka target endpoint. For more information,
-- see the @KafkaSettings@ structure.
endpoint_kafkaSettings :: Lens.Lens' Endpoint (Core.Maybe KafkaSettings)
endpoint_kafkaSettings = Lens.lens (\Endpoint' {kafkaSettings} -> kafkaSettings) (\s@Endpoint' {} a -> s {kafkaSettings = a} :: Endpoint)

-- | Undocumented member.
endpoint_docDbSettings :: Lens.Lens' Endpoint (Core.Maybe DocDbSettings)
endpoint_docDbSettings = Lens.lens (\Endpoint' {docDbSettings} -> docDbSettings) (\s@Endpoint' {} a -> s {docDbSettings = a} :: Endpoint)

-- | The settings for the DynamoDB target endpoint. For more information, see
-- the @DynamoDBSettings@ structure.
endpoint_dynamoDbSettings :: Lens.Lens' Endpoint (Core.Maybe DynamoDbSettings)
endpoint_dynamoDbSettings = Lens.lens (\Endpoint' {dynamoDbSettings} -> dynamoDbSettings) (\s@Endpoint' {} a -> s {dynamoDbSettings = a} :: Endpoint)

-- | Additional connection attributes used to connect to the endpoint.
endpoint_extraConnectionAttributes :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_extraConnectionAttributes = Lens.lens (\Endpoint' {extraConnectionAttributes} -> extraConnectionAttributes) (\s@Endpoint' {} a -> s {extraConnectionAttributes = a} :: Endpoint)

-- | Value returned by a call to CreateEndpoint that can be used for
-- cross-account validation. Use it on a subsequent call to CreateEndpoint
-- to create the endpoint with a cross-account.
endpoint_externalId :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_externalId = Lens.lens (\Endpoint' {externalId} -> externalId) (\s@Endpoint' {} a -> s {externalId = a} :: Endpoint)

-- | The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
endpoint_endpointIdentifier :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_endpointIdentifier = Lens.lens (\Endpoint' {endpointIdentifier} -> endpointIdentifier) (\s@Endpoint' {} a -> s {endpointIdentifier = a} :: Endpoint)

-- | The settings for the Amazon Kinesis target endpoint. For more
-- information, see the @KinesisSettings@ structure.
endpoint_kinesisSettings :: Lens.Lens' Endpoint (Core.Maybe KinesisSettings)
endpoint_kinesisSettings = Lens.lens (\Endpoint' {kinesisSettings} -> kinesisSettings) (\s@Endpoint' {} a -> s {kinesisSettings = a} :: Endpoint)

-- | The settings for the SAP ASE source and target endpoint. For more
-- information, see the @SybaseSettings@ structure.
endpoint_sybaseSettings :: Lens.Lens' Endpoint (Core.Maybe SybaseSettings)
endpoint_sybaseSettings = Lens.lens (\Endpoint' {sybaseSettings} -> sybaseSettings) (\s@Endpoint' {} a -> s {sybaseSettings = a} :: Endpoint)

-- | The name of the database at the endpoint.
endpoint_databaseName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_databaseName = Lens.lens (\Endpoint' {databaseName} -> databaseName) (\s@Endpoint' {} a -> s {databaseName = a} :: Endpoint)

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Core.<$> (x Core..:? "SslMode")
            Core.<*> (x Core..:? "MongoDbSettings")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "NeptuneSettings")
            Core.<*> (x Core..:? "EngineName")
            Core.<*> (x Core..:? "ElasticsearchSettings")
            Core.<*> (x Core..:? "ExternalTableDefinition")
            Core.<*> (x Core..:? "EndpointType")
            Core.<*> (x Core..:? "OracleSettings")
            Core.<*> (x Core..:? "PostgreSQLSettings")
            Core.<*> (x Core..:? "ServiceAccessRoleArn")
            Core.<*> (x Core..:? "CertificateArn")
            Core.<*> (x Core..:? "S3Settings")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "MicrosoftSQLServerSettings")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "IBMDb2Settings")
            Core.<*> (x Core..:? "MySQLSettings")
            Core.<*> (x Core..:? "DmsTransferSettings")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "EndpointArn")
            Core.<*> (x Core..:? "RedshiftSettings")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "EngineDisplayName")
            Core.<*> (x Core..:? "KafkaSettings")
            Core.<*> (x Core..:? "DocDbSettings")
            Core.<*> (x Core..:? "DynamoDbSettings")
            Core.<*> (x Core..:? "ExtraConnectionAttributes")
            Core.<*> (x Core..:? "ExternalId")
            Core.<*> (x Core..:? "EndpointIdentifier")
            Core.<*> (x Core..:? "KinesisSettings")
            Core.<*> (x Core..:? "SybaseSettings")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable Endpoint

instance Core.NFData Endpoint
