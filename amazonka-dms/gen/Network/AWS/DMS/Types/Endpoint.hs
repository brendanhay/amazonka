{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Prelude as Prelude

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
    sslMode :: Prelude.Maybe DmsSslModeValue,
    -- | The settings for the MongoDB source endpoint. For more information, see
    -- the @MongoDbSettings@ structure.
    mongoDbSettings :: Prelude.Maybe MongoDbSettings,
    -- | The status of the endpoint.
    status :: Prelude.Maybe Prelude.Text,
    -- | The settings for the Amazon Neptune target endpoint. For more
    -- information, see the @NeptuneSettings@ structure.
    neptuneSettings :: Prelude.Maybe NeptuneSettings,
    -- | The database engine name. Valid values, depending on the EndpointType,
    -- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
    -- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
    -- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
    -- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
    -- @\"sqlserver\"@, and @\"neptune\"@.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | The settings for the Elasticsearch source endpoint. For more
    -- information, see the @ElasticsearchSettings@ structure.
    elasticsearchSettings :: Prelude.Maybe ElasticsearchSettings,
    -- | The external table definition.
    externalTableDefinition :: Prelude.Maybe Prelude.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Prelude.Maybe ReplicationEndpointTypeValue,
    -- | The settings for the Oracle source and target endpoint. For more
    -- information, see the @OracleSettings@ structure.
    oracleSettings :: Prelude.Maybe OracleSettings,
    -- | The settings for the PostgreSQL source and target endpoint. For more
    -- information, see the @PostgreSQLSettings@ structure.
    postgreSQLSettings :: Prelude.Maybe PostgreSQLSettings,
    -- | The Amazon Resource Name (ARN) used by the service access IAM role.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The settings for the S3 target endpoint. For more information, see the
    -- @S3Settings@ structure.
    s3Settings :: Prelude.Maybe S3Settings,
    -- | The name of the server at the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The settings for the Microsoft SQL Server source and target endpoint.
    -- For more information, see the @MicrosoftSQLServerSettings@ structure.
    microsoftSQLServerSettings :: Prelude.Maybe MicrosoftSQLServerSettings,
    -- | An AWS KMS key identifier that is used to encrypt the connection
    -- parameters for the endpoint.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
    -- uses your default encryption key.
    --
    -- AWS KMS creates the default encryption key for your AWS account. Your
    -- AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The settings for the IBM Db2 LUW source endpoint. For more information,
    -- see the @IBMDb2Settings@ structure.
    iBMDb2Settings :: Prelude.Maybe IBMDb2Settings,
    -- | The settings for the MySQL source and target endpoint. For more
    -- information, see the @MySQLSettings@ structure.
    mySQLSettings :: Prelude.Maybe MySQLSettings,
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
    dmsTransferSettings :: Prelude.Maybe DmsTransferSettings,
    -- | The port value used to access the endpoint.
    port :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | Settings for the Amazon Redshift endpoint.
    redshiftSettings :: Prelude.Maybe RedshiftSettings,
    -- | The user name used to connect to the endpoint.
    username :: Prelude.Maybe Prelude.Text,
    -- | The expanded name for the engine name. For example, if the @EngineName@
    -- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
    engineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The settings for the Apache Kafka target endpoint. For more information,
    -- see the @KafkaSettings@ structure.
    kafkaSettings :: Prelude.Maybe KafkaSettings,
    docDbSettings :: Prelude.Maybe DocDbSettings,
    -- | The settings for the DynamoDB target endpoint. For more information, see
    -- the @DynamoDBSettings@ structure.
    dynamoDbSettings :: Prelude.Maybe DynamoDbSettings,
    -- | Additional connection attributes used to connect to the endpoint.
    extraConnectionAttributes :: Prelude.Maybe Prelude.Text,
    -- | Value returned by a call to CreateEndpoint that can be used for
    -- cross-account validation. Use it on a subsequent call to CreateEndpoint
    -- to create the endpoint with a cross-account.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The database endpoint identifier. Identifiers must begin with a letter
    -- and must contain only ASCII letters, digits, and hyphens. They can\'t
    -- end with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The settings for the Amazon Kinesis target endpoint. For more
    -- information, see the @KinesisSettings@ structure.
    kinesisSettings :: Prelude.Maybe KinesisSettings,
    -- | The settings for the SAP ASE source and target endpoint. For more
    -- information, see the @SybaseSettings@ structure.
    sybaseSettings :: Prelude.Maybe SybaseSettings,
    -- | The name of the database at the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { sslMode = Prelude.Nothing,
      mongoDbSettings = Prelude.Nothing,
      status = Prelude.Nothing,
      neptuneSettings = Prelude.Nothing,
      engineName = Prelude.Nothing,
      elasticsearchSettings = Prelude.Nothing,
      externalTableDefinition = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      oracleSettings = Prelude.Nothing,
      postgreSQLSettings = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      s3Settings = Prelude.Nothing,
      serverName = Prelude.Nothing,
      microsoftSQLServerSettings = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      iBMDb2Settings = Prelude.Nothing,
      mySQLSettings = Prelude.Nothing,
      dmsTransferSettings = Prelude.Nothing,
      port = Prelude.Nothing,
      endpointArn = Prelude.Nothing,
      redshiftSettings = Prelude.Nothing,
      username = Prelude.Nothing,
      engineDisplayName = Prelude.Nothing,
      kafkaSettings = Prelude.Nothing,
      docDbSettings = Prelude.Nothing,
      dynamoDbSettings = Prelude.Nothing,
      extraConnectionAttributes = Prelude.Nothing,
      externalId = Prelude.Nothing,
      endpointIdentifier = Prelude.Nothing,
      kinesisSettings = Prelude.Nothing,
      sybaseSettings = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The SSL mode used to connect to the endpoint. The default value is
-- @none@.
endpoint_sslMode :: Lens.Lens' Endpoint (Prelude.Maybe DmsSslModeValue)
endpoint_sslMode = Lens.lens (\Endpoint' {sslMode} -> sslMode) (\s@Endpoint' {} a -> s {sslMode = a} :: Endpoint)

-- | The settings for the MongoDB source endpoint. For more information, see
-- the @MongoDbSettings@ structure.
endpoint_mongoDbSettings :: Lens.Lens' Endpoint (Prelude.Maybe MongoDbSettings)
endpoint_mongoDbSettings = Lens.lens (\Endpoint' {mongoDbSettings} -> mongoDbSettings) (\s@Endpoint' {} a -> s {mongoDbSettings = a} :: Endpoint)

-- | The status of the endpoint.
endpoint_status :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_status = Lens.lens (\Endpoint' {status} -> status) (\s@Endpoint' {} a -> s {status = a} :: Endpoint)

-- | The settings for the Amazon Neptune target endpoint. For more
-- information, see the @NeptuneSettings@ structure.
endpoint_neptuneSettings :: Lens.Lens' Endpoint (Prelude.Maybe NeptuneSettings)
endpoint_neptuneSettings = Lens.lens (\Endpoint' {neptuneSettings} -> neptuneSettings) (\s@Endpoint' {} a -> s {neptuneSettings = a} :: Endpoint)

-- | The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
endpoint_engineName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_engineName = Lens.lens (\Endpoint' {engineName} -> engineName) (\s@Endpoint' {} a -> s {engineName = a} :: Endpoint)

-- | The settings for the Elasticsearch source endpoint. For more
-- information, see the @ElasticsearchSettings@ structure.
endpoint_elasticsearchSettings :: Lens.Lens' Endpoint (Prelude.Maybe ElasticsearchSettings)
endpoint_elasticsearchSettings = Lens.lens (\Endpoint' {elasticsearchSettings} -> elasticsearchSettings) (\s@Endpoint' {} a -> s {elasticsearchSettings = a} :: Endpoint)

-- | The external table definition.
endpoint_externalTableDefinition :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_externalTableDefinition = Lens.lens (\Endpoint' {externalTableDefinition} -> externalTableDefinition) (\s@Endpoint' {} a -> s {externalTableDefinition = a} :: Endpoint)

-- | The type of endpoint. Valid values are @source@ and @target@.
endpoint_endpointType :: Lens.Lens' Endpoint (Prelude.Maybe ReplicationEndpointTypeValue)
endpoint_endpointType = Lens.lens (\Endpoint' {endpointType} -> endpointType) (\s@Endpoint' {} a -> s {endpointType = a} :: Endpoint)

-- | The settings for the Oracle source and target endpoint. For more
-- information, see the @OracleSettings@ structure.
endpoint_oracleSettings :: Lens.Lens' Endpoint (Prelude.Maybe OracleSettings)
endpoint_oracleSettings = Lens.lens (\Endpoint' {oracleSettings} -> oracleSettings) (\s@Endpoint' {} a -> s {oracleSettings = a} :: Endpoint)

-- | The settings for the PostgreSQL source and target endpoint. For more
-- information, see the @PostgreSQLSettings@ structure.
endpoint_postgreSQLSettings :: Lens.Lens' Endpoint (Prelude.Maybe PostgreSQLSettings)
endpoint_postgreSQLSettings = Lens.lens (\Endpoint' {postgreSQLSettings} -> postgreSQLSettings) (\s@Endpoint' {} a -> s {postgreSQLSettings = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
endpoint_serviceAccessRoleArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_serviceAccessRoleArn = Lens.lens (\Endpoint' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@Endpoint' {} a -> s {serviceAccessRoleArn = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
endpoint_certificateArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_certificateArn = Lens.lens (\Endpoint' {certificateArn} -> certificateArn) (\s@Endpoint' {} a -> s {certificateArn = a} :: Endpoint)

-- | The settings for the S3 target endpoint. For more information, see the
-- @S3Settings@ structure.
endpoint_s3Settings :: Lens.Lens' Endpoint (Prelude.Maybe S3Settings)
endpoint_s3Settings = Lens.lens (\Endpoint' {s3Settings} -> s3Settings) (\s@Endpoint' {} a -> s {s3Settings = a} :: Endpoint)

-- | The name of the server at the endpoint.
endpoint_serverName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_serverName = Lens.lens (\Endpoint' {serverName} -> serverName) (\s@Endpoint' {} a -> s {serverName = a} :: Endpoint)

-- | The settings for the Microsoft SQL Server source and target endpoint.
-- For more information, see the @MicrosoftSQLServerSettings@ structure.
endpoint_microsoftSQLServerSettings :: Lens.Lens' Endpoint (Prelude.Maybe MicrosoftSQLServerSettings)
endpoint_microsoftSQLServerSettings = Lens.lens (\Endpoint' {microsoftSQLServerSettings} -> microsoftSQLServerSettings) (\s@Endpoint' {} a -> s {microsoftSQLServerSettings = a} :: Endpoint)

-- | An AWS KMS key identifier that is used to encrypt the connection
-- parameters for the endpoint.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
endpoint_kmsKeyId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_kmsKeyId = Lens.lens (\Endpoint' {kmsKeyId} -> kmsKeyId) (\s@Endpoint' {} a -> s {kmsKeyId = a} :: Endpoint)

-- | The settings for the IBM Db2 LUW source endpoint. For more information,
-- see the @IBMDb2Settings@ structure.
endpoint_iBMDb2Settings :: Lens.Lens' Endpoint (Prelude.Maybe IBMDb2Settings)
endpoint_iBMDb2Settings = Lens.lens (\Endpoint' {iBMDb2Settings} -> iBMDb2Settings) (\s@Endpoint' {} a -> s {iBMDb2Settings = a} :: Endpoint)

-- | The settings for the MySQL source and target endpoint. For more
-- information, see the @MySQLSettings@ structure.
endpoint_mySQLSettings :: Lens.Lens' Endpoint (Prelude.Maybe MySQLSettings)
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
endpoint_dmsTransferSettings :: Lens.Lens' Endpoint (Prelude.Maybe DmsTransferSettings)
endpoint_dmsTransferSettings = Lens.lens (\Endpoint' {dmsTransferSettings} -> dmsTransferSettings) (\s@Endpoint' {} a -> s {dmsTransferSettings = a} :: Endpoint)

-- | The port value used to access the endpoint.
endpoint_port :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
endpoint_endpointArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointArn = Lens.lens (\Endpoint' {endpointArn} -> endpointArn) (\s@Endpoint' {} a -> s {endpointArn = a} :: Endpoint)

-- | Settings for the Amazon Redshift endpoint.
endpoint_redshiftSettings :: Lens.Lens' Endpoint (Prelude.Maybe RedshiftSettings)
endpoint_redshiftSettings = Lens.lens (\Endpoint' {redshiftSettings} -> redshiftSettings) (\s@Endpoint' {} a -> s {redshiftSettings = a} :: Endpoint)

-- | The user name used to connect to the endpoint.
endpoint_username :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_username = Lens.lens (\Endpoint' {username} -> username) (\s@Endpoint' {} a -> s {username = a} :: Endpoint)

-- | The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
endpoint_engineDisplayName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_engineDisplayName = Lens.lens (\Endpoint' {engineDisplayName} -> engineDisplayName) (\s@Endpoint' {} a -> s {engineDisplayName = a} :: Endpoint)

-- | The settings for the Apache Kafka target endpoint. For more information,
-- see the @KafkaSettings@ structure.
endpoint_kafkaSettings :: Lens.Lens' Endpoint (Prelude.Maybe KafkaSettings)
endpoint_kafkaSettings = Lens.lens (\Endpoint' {kafkaSettings} -> kafkaSettings) (\s@Endpoint' {} a -> s {kafkaSettings = a} :: Endpoint)

-- | Undocumented member.
endpoint_docDbSettings :: Lens.Lens' Endpoint (Prelude.Maybe DocDbSettings)
endpoint_docDbSettings = Lens.lens (\Endpoint' {docDbSettings} -> docDbSettings) (\s@Endpoint' {} a -> s {docDbSettings = a} :: Endpoint)

-- | The settings for the DynamoDB target endpoint. For more information, see
-- the @DynamoDBSettings@ structure.
endpoint_dynamoDbSettings :: Lens.Lens' Endpoint (Prelude.Maybe DynamoDbSettings)
endpoint_dynamoDbSettings = Lens.lens (\Endpoint' {dynamoDbSettings} -> dynamoDbSettings) (\s@Endpoint' {} a -> s {dynamoDbSettings = a} :: Endpoint)

-- | Additional connection attributes used to connect to the endpoint.
endpoint_extraConnectionAttributes :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_extraConnectionAttributes = Lens.lens (\Endpoint' {extraConnectionAttributes} -> extraConnectionAttributes) (\s@Endpoint' {} a -> s {extraConnectionAttributes = a} :: Endpoint)

-- | Value returned by a call to CreateEndpoint that can be used for
-- cross-account validation. Use it on a subsequent call to CreateEndpoint
-- to create the endpoint with a cross-account.
endpoint_externalId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_externalId = Lens.lens (\Endpoint' {externalId} -> externalId) (\s@Endpoint' {} a -> s {externalId = a} :: Endpoint)

-- | The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
endpoint_endpointIdentifier :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointIdentifier = Lens.lens (\Endpoint' {endpointIdentifier} -> endpointIdentifier) (\s@Endpoint' {} a -> s {endpointIdentifier = a} :: Endpoint)

-- | The settings for the Amazon Kinesis target endpoint. For more
-- information, see the @KinesisSettings@ structure.
endpoint_kinesisSettings :: Lens.Lens' Endpoint (Prelude.Maybe KinesisSettings)
endpoint_kinesisSettings = Lens.lens (\Endpoint' {kinesisSettings} -> kinesisSettings) (\s@Endpoint' {} a -> s {kinesisSettings = a} :: Endpoint)

-- | The settings for the SAP ASE source and target endpoint. For more
-- information, see the @SybaseSettings@ structure.
endpoint_sybaseSettings :: Lens.Lens' Endpoint (Prelude.Maybe SybaseSettings)
endpoint_sybaseSettings = Lens.lens (\Endpoint' {sybaseSettings} -> sybaseSettings) (\s@Endpoint' {} a -> s {sybaseSettings = a} :: Endpoint)

-- | The name of the database at the endpoint.
endpoint_databaseName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_databaseName = Lens.lens (\Endpoint' {databaseName} -> databaseName) (\s@Endpoint' {} a -> s {databaseName = a} :: Endpoint)

instance Prelude.FromJSON Endpoint where
  parseJSON =
    Prelude.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Prelude..:? "SslMode")
            Prelude.<*> (x Prelude..:? "MongoDbSettings")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "NeptuneSettings")
            Prelude.<*> (x Prelude..:? "EngineName")
            Prelude.<*> (x Prelude..:? "ElasticsearchSettings")
            Prelude.<*> (x Prelude..:? "ExternalTableDefinition")
            Prelude.<*> (x Prelude..:? "EndpointType")
            Prelude.<*> (x Prelude..:? "OracleSettings")
            Prelude.<*> (x Prelude..:? "PostgreSQLSettings")
            Prelude.<*> (x Prelude..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Prelude..:? "CertificateArn")
            Prelude.<*> (x Prelude..:? "S3Settings")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "MicrosoftSQLServerSettings")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "IBMDb2Settings")
            Prelude.<*> (x Prelude..:? "MySQLSettings")
            Prelude.<*> (x Prelude..:? "DmsTransferSettings")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "EndpointArn")
            Prelude.<*> (x Prelude..:? "RedshiftSettings")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "EngineDisplayName")
            Prelude.<*> (x Prelude..:? "KafkaSettings")
            Prelude.<*> (x Prelude..:? "DocDbSettings")
            Prelude.<*> (x Prelude..:? "DynamoDbSettings")
            Prelude.<*> (x Prelude..:? "ExtraConnectionAttributes")
            Prelude.<*> (x Prelude..:? "ExternalId")
            Prelude.<*> (x Prelude..:? "EndpointIdentifier")
            Prelude.<*> (x Prelude..:? "KinesisSettings")
            Prelude.<*> (x Prelude..:? "SybaseSettings")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable Endpoint

instance Prelude.NFData Endpoint
