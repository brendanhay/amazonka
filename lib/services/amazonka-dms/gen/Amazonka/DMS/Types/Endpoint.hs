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
-- Module      : Amazonka.DMS.Types.Endpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.DmsSslModeValue
import Amazonka.DMS.Types.DmsTransferSettings
import Amazonka.DMS.Types.DocDbSettings
import Amazonka.DMS.Types.DynamoDbSettings
import Amazonka.DMS.Types.ElasticsearchSettings
import Amazonka.DMS.Types.GcpMySQLSettings
import Amazonka.DMS.Types.IBMDb2Settings
import Amazonka.DMS.Types.KafkaSettings
import Amazonka.DMS.Types.KinesisSettings
import Amazonka.DMS.Types.MicrosoftSQLServerSettings
import Amazonka.DMS.Types.MongoDbSettings
import Amazonka.DMS.Types.MySQLSettings
import Amazonka.DMS.Types.NeptuneSettings
import Amazonka.DMS.Types.OracleSettings
import Amazonka.DMS.Types.PostgreSQLSettings
import Amazonka.DMS.Types.RedisSettings
import Amazonka.DMS.Types.RedshiftSettings
import Amazonka.DMS.Types.ReplicationEndpointTypeValue
import Amazonka.DMS.Types.S3Settings
import Amazonka.DMS.Types.SybaseSettings
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an endpoint of a database instance in response to operations
-- such as the following:
--
-- -   @CreateEndpoint@
--
-- -   @DescribeEndpoint@
--
-- -   @ModifyEndpoint@
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The port value used to access the endpoint.
    port :: Prelude.Maybe Prelude.Int,
    -- | The settings for the OpenSearch source endpoint. For more information,
    -- see the @ElasticsearchSettings@ structure.
    elasticsearchSettings :: Prelude.Maybe ElasticsearchSettings,
    -- | Settings for the Amazon Redshift endpoint.
    redshiftSettings :: Prelude.Maybe RedshiftSettings,
    -- | The external table definition.
    externalTableDefinition :: Prelude.Maybe Prelude.Text,
    -- | The database engine name. Valid values, depending on the EndpointType,
    -- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
    -- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
    -- @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
    -- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
    -- @\"documentdb\"@, @\"sqlserver\"@, @\"neptune\"@, and @\"babelfish\"@.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | The expanded name for the engine name. For example, if the @EngineName@
    -- parameter is \"aurora\", this value would be \"Amazon Aurora MySQL\".
    engineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The settings for the MySQL source and target endpoint. For more
    -- information, see the @MySQLSettings@ structure.
    mySQLSettings :: Prelude.Maybe MySQLSettings,
    -- | The SSL mode used to connect to the endpoint. The default value is
    -- @none@.
    sslMode :: Prelude.Maybe DmsSslModeValue,
    -- | The database endpoint identifier. Identifiers must begin with a letter
    -- and must contain only ASCII letters, digits, and hyphens. They can\'t
    -- end with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the server at the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    docDbSettings :: Prelude.Maybe DocDbSettings,
    -- | The name of the database at the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The user name used to connect to the endpoint.
    username :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the source GCP MySQL endpoint.
    gcpMySQLSettings :: Prelude.Maybe GcpMySQLSettings,
    -- | The Amazon Resource Name (ARN) used by the service to access the IAM
    -- role. The role must allow the @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Additional connection attributes used to connect to the endpoint.
    extraConnectionAttributes :: Prelude.Maybe Prelude.Text,
    -- | The settings for the Amazon Neptune target endpoint. For more
    -- information, see the @NeptuneSettings@ structure.
    neptuneSettings :: Prelude.Maybe NeptuneSettings,
    -- | Value returned by a call to CreateEndpoint that can be used for
    -- cross-account validation. Use it on a subsequent call to CreateEndpoint
    -- to create the endpoint with a cross-account.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The status of the endpoint.
    status :: Prelude.Maybe Prelude.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Prelude.Maybe ReplicationEndpointTypeValue,
    -- | The settings for the Amazon Kinesis target endpoint. For more
    -- information, see the @KinesisSettings@ structure.
    kinesisSettings :: Prelude.Maybe KinesisSettings,
    -- | The settings for the Oracle source and target endpoint. For more
    -- information, see the @OracleSettings@ structure.
    oracleSettings :: Prelude.Maybe OracleSettings,
    -- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The settings for the DynamoDB target endpoint. For more information, see
    -- the @DynamoDBSettings@ structure.
    dynamoDbSettings :: Prelude.Maybe DynamoDbSettings,
    -- | The settings for the Redis target endpoint. For more information, see
    -- the @RedisSettings@ structure.
    redisSettings :: Prelude.Maybe RedisSettings,
    -- | The settings for the S3 target endpoint. For more information, see the
    -- @S3Settings@ structure.
    s3Settings :: Prelude.Maybe S3Settings,
    -- | An KMS key identifier that is used to encrypt the connection parameters
    -- for the endpoint.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
    -- uses your default encryption key.
    --
    -- KMS creates the default encryption key for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default
    -- encryption key for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The settings for the Microsoft SQL Server source and target endpoint.
    -- For more information, see the @MicrosoftSQLServerSettings@ structure.
    microsoftSQLServerSettings :: Prelude.Maybe MicrosoftSQLServerSettings,
    -- | The settings for the Apache Kafka target endpoint. For more information,
    -- see the @KafkaSettings@ structure.
    kafkaSettings :: Prelude.Maybe KafkaSettings,
    -- | The settings for the DMS Transfer type source. For more information, see
    -- the DmsTransferSettings structure.
    dmsTransferSettings :: Prelude.Maybe DmsTransferSettings,
    -- | The settings for the SAP ASE source and target endpoint. For more
    -- information, see the @SybaseSettings@ structure.
    sybaseSettings :: Prelude.Maybe SybaseSettings,
    -- | The settings for the PostgreSQL source and target endpoint. For more
    -- information, see the @PostgreSQLSettings@ structure.
    postgreSQLSettings :: Prelude.Maybe PostgreSQLSettings,
    -- | The settings for the IBM Db2 LUW source endpoint. For more information,
    -- see the @IBMDb2Settings@ structure.
    iBMDb2Settings :: Prelude.Maybe IBMDb2Settings,
    -- | The settings for the MongoDB source endpoint. For more information, see
    -- the @MongoDbSettings@ structure.
    mongoDbSettings :: Prelude.Maybe MongoDbSettings,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'endpoint_port' - The port value used to access the endpoint.
--
-- 'elasticsearchSettings', 'endpoint_elasticsearchSettings' - The settings for the OpenSearch source endpoint. For more information,
-- see the @ElasticsearchSettings@ structure.
--
-- 'redshiftSettings', 'endpoint_redshiftSettings' - Settings for the Amazon Redshift endpoint.
--
-- 'externalTableDefinition', 'endpoint_externalTableDefinition' - The external table definition.
--
-- 'engineName', 'endpoint_engineName' - The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
-- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
-- @\"documentdb\"@, @\"sqlserver\"@, @\"neptune\"@, and @\"babelfish\"@.
--
-- 'engineDisplayName', 'endpoint_engineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora\", this value would be \"Amazon Aurora MySQL\".
--
-- 'mySQLSettings', 'endpoint_mySQLSettings' - The settings for the MySQL source and target endpoint. For more
-- information, see the @MySQLSettings@ structure.
--
-- 'sslMode', 'endpoint_sslMode' - The SSL mode used to connect to the endpoint. The default value is
-- @none@.
--
-- 'endpointIdentifier', 'endpoint_endpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
--
-- 'serverName', 'endpoint_serverName' - The name of the server at the endpoint.
--
-- 'docDbSettings', 'endpoint_docDbSettings' - Undocumented member.
--
-- 'databaseName', 'endpoint_databaseName' - The name of the database at the endpoint.
--
-- 'username', 'endpoint_username' - The user name used to connect to the endpoint.
--
-- 'gcpMySQLSettings', 'endpoint_gcpMySQLSettings' - Settings in JSON format for the source GCP MySQL endpoint.
--
-- 'serviceAccessRoleArn', 'endpoint_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action.
--
-- 'extraConnectionAttributes', 'endpoint_extraConnectionAttributes' - Additional connection attributes used to connect to the endpoint.
--
-- 'neptuneSettings', 'endpoint_neptuneSettings' - The settings for the Amazon Neptune target endpoint. For more
-- information, see the @NeptuneSettings@ structure.
--
-- 'externalId', 'endpoint_externalId' - Value returned by a call to CreateEndpoint that can be used for
-- cross-account validation. Use it on a subsequent call to CreateEndpoint
-- to create the endpoint with a cross-account.
--
-- 'status', 'endpoint_status' - The status of the endpoint.
--
-- 'endpointType', 'endpoint_endpointType' - The type of endpoint. Valid values are @source@ and @target@.
--
-- 'kinesisSettings', 'endpoint_kinesisSettings' - The settings for the Amazon Kinesis target endpoint. For more
-- information, see the @KinesisSettings@ structure.
--
-- 'oracleSettings', 'endpoint_oracleSettings' - The settings for the Oracle source and target endpoint. For more
-- information, see the @OracleSettings@ structure.
--
-- 'certificateArn', 'endpoint_certificateArn' - The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
--
-- 'dynamoDbSettings', 'endpoint_dynamoDbSettings' - The settings for the DynamoDB target endpoint. For more information, see
-- the @DynamoDBSettings@ structure.
--
-- 'redisSettings', 'endpoint_redisSettings' - The settings for the Redis target endpoint. For more information, see
-- the @RedisSettings@ structure.
--
-- 's3Settings', 'endpoint_s3Settings' - The settings for the S3 target endpoint. For more information, see the
-- @S3Settings@ structure.
--
-- 'kmsKeyId', 'endpoint_kmsKeyId' - An KMS key identifier that is used to encrypt the connection parameters
-- for the endpoint.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
--
-- 'microsoftSQLServerSettings', 'endpoint_microsoftSQLServerSettings' - The settings for the Microsoft SQL Server source and target endpoint.
-- For more information, see the @MicrosoftSQLServerSettings@ structure.
--
-- 'kafkaSettings', 'endpoint_kafkaSettings' - The settings for the Apache Kafka target endpoint. For more information,
-- see the @KafkaSettings@ structure.
--
-- 'dmsTransferSettings', 'endpoint_dmsTransferSettings' - The settings for the DMS Transfer type source. For more information, see
-- the DmsTransferSettings structure.
--
-- 'sybaseSettings', 'endpoint_sybaseSettings' - The settings for the SAP ASE source and target endpoint. For more
-- information, see the @SybaseSettings@ structure.
--
-- 'postgreSQLSettings', 'endpoint_postgreSQLSettings' - The settings for the PostgreSQL source and target endpoint. For more
-- information, see the @PostgreSQLSettings@ structure.
--
-- 'iBMDb2Settings', 'endpoint_iBMDb2Settings' - The settings for the IBM Db2 LUW source endpoint. For more information,
-- see the @IBMDb2Settings@ structure.
--
-- 'mongoDbSettings', 'endpoint_mongoDbSettings' - The settings for the MongoDB source endpoint. For more information, see
-- the @MongoDbSettings@ structure.
--
-- 'endpointArn', 'endpoint_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { port = Prelude.Nothing,
      elasticsearchSettings = Prelude.Nothing,
      redshiftSettings = Prelude.Nothing,
      externalTableDefinition = Prelude.Nothing,
      engineName = Prelude.Nothing,
      engineDisplayName = Prelude.Nothing,
      mySQLSettings = Prelude.Nothing,
      sslMode = Prelude.Nothing,
      endpointIdentifier = Prelude.Nothing,
      serverName = Prelude.Nothing,
      docDbSettings = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      username = Prelude.Nothing,
      gcpMySQLSettings = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      extraConnectionAttributes = Prelude.Nothing,
      neptuneSettings = Prelude.Nothing,
      externalId = Prelude.Nothing,
      status = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      kinesisSettings = Prelude.Nothing,
      oracleSettings = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      dynamoDbSettings = Prelude.Nothing,
      redisSettings = Prelude.Nothing,
      s3Settings = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      microsoftSQLServerSettings = Prelude.Nothing,
      kafkaSettings = Prelude.Nothing,
      dmsTransferSettings = Prelude.Nothing,
      sybaseSettings = Prelude.Nothing,
      postgreSQLSettings = Prelude.Nothing,
      iBMDb2Settings = Prelude.Nothing,
      mongoDbSettings = Prelude.Nothing,
      endpointArn = Prelude.Nothing
    }

-- | The port value used to access the endpoint.
endpoint_port :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

-- | The settings for the OpenSearch source endpoint. For more information,
-- see the @ElasticsearchSettings@ structure.
endpoint_elasticsearchSettings :: Lens.Lens' Endpoint (Prelude.Maybe ElasticsearchSettings)
endpoint_elasticsearchSettings = Lens.lens (\Endpoint' {elasticsearchSettings} -> elasticsearchSettings) (\s@Endpoint' {} a -> s {elasticsearchSettings = a} :: Endpoint)

-- | Settings for the Amazon Redshift endpoint.
endpoint_redshiftSettings :: Lens.Lens' Endpoint (Prelude.Maybe RedshiftSettings)
endpoint_redshiftSettings = Lens.lens (\Endpoint' {redshiftSettings} -> redshiftSettings) (\s@Endpoint' {} a -> s {redshiftSettings = a} :: Endpoint)

-- | The external table definition.
endpoint_externalTableDefinition :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_externalTableDefinition = Lens.lens (\Endpoint' {externalTableDefinition} -> externalTableDefinition) (\s@Endpoint' {} a -> s {externalTableDefinition = a} :: Endpoint)

-- | The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
-- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
-- @\"documentdb\"@, @\"sqlserver\"@, @\"neptune\"@, and @\"babelfish\"@.
endpoint_engineName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_engineName = Lens.lens (\Endpoint' {engineName} -> engineName) (\s@Endpoint' {} a -> s {engineName = a} :: Endpoint)

-- | The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora\", this value would be \"Amazon Aurora MySQL\".
endpoint_engineDisplayName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_engineDisplayName = Lens.lens (\Endpoint' {engineDisplayName} -> engineDisplayName) (\s@Endpoint' {} a -> s {engineDisplayName = a} :: Endpoint)

-- | The settings for the MySQL source and target endpoint. For more
-- information, see the @MySQLSettings@ structure.
endpoint_mySQLSettings :: Lens.Lens' Endpoint (Prelude.Maybe MySQLSettings)
endpoint_mySQLSettings = Lens.lens (\Endpoint' {mySQLSettings} -> mySQLSettings) (\s@Endpoint' {} a -> s {mySQLSettings = a} :: Endpoint)

-- | The SSL mode used to connect to the endpoint. The default value is
-- @none@.
endpoint_sslMode :: Lens.Lens' Endpoint (Prelude.Maybe DmsSslModeValue)
endpoint_sslMode = Lens.lens (\Endpoint' {sslMode} -> sslMode) (\s@Endpoint' {} a -> s {sslMode = a} :: Endpoint)

-- | The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
endpoint_endpointIdentifier :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointIdentifier = Lens.lens (\Endpoint' {endpointIdentifier} -> endpointIdentifier) (\s@Endpoint' {} a -> s {endpointIdentifier = a} :: Endpoint)

-- | The name of the server at the endpoint.
endpoint_serverName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_serverName = Lens.lens (\Endpoint' {serverName} -> serverName) (\s@Endpoint' {} a -> s {serverName = a} :: Endpoint)

-- | Undocumented member.
endpoint_docDbSettings :: Lens.Lens' Endpoint (Prelude.Maybe DocDbSettings)
endpoint_docDbSettings = Lens.lens (\Endpoint' {docDbSettings} -> docDbSettings) (\s@Endpoint' {} a -> s {docDbSettings = a} :: Endpoint)

-- | The name of the database at the endpoint.
endpoint_databaseName :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_databaseName = Lens.lens (\Endpoint' {databaseName} -> databaseName) (\s@Endpoint' {} a -> s {databaseName = a} :: Endpoint)

-- | The user name used to connect to the endpoint.
endpoint_username :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_username = Lens.lens (\Endpoint' {username} -> username) (\s@Endpoint' {} a -> s {username = a} :: Endpoint)

-- | Settings in JSON format for the source GCP MySQL endpoint.
endpoint_gcpMySQLSettings :: Lens.Lens' Endpoint (Prelude.Maybe GcpMySQLSettings)
endpoint_gcpMySQLSettings = Lens.lens (\Endpoint' {gcpMySQLSettings} -> gcpMySQLSettings) (\s@Endpoint' {} a -> s {gcpMySQLSettings = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action.
endpoint_serviceAccessRoleArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_serviceAccessRoleArn = Lens.lens (\Endpoint' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@Endpoint' {} a -> s {serviceAccessRoleArn = a} :: Endpoint)

-- | Additional connection attributes used to connect to the endpoint.
endpoint_extraConnectionAttributes :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_extraConnectionAttributes = Lens.lens (\Endpoint' {extraConnectionAttributes} -> extraConnectionAttributes) (\s@Endpoint' {} a -> s {extraConnectionAttributes = a} :: Endpoint)

-- | The settings for the Amazon Neptune target endpoint. For more
-- information, see the @NeptuneSettings@ structure.
endpoint_neptuneSettings :: Lens.Lens' Endpoint (Prelude.Maybe NeptuneSettings)
endpoint_neptuneSettings = Lens.lens (\Endpoint' {neptuneSettings} -> neptuneSettings) (\s@Endpoint' {} a -> s {neptuneSettings = a} :: Endpoint)

-- | Value returned by a call to CreateEndpoint that can be used for
-- cross-account validation. Use it on a subsequent call to CreateEndpoint
-- to create the endpoint with a cross-account.
endpoint_externalId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_externalId = Lens.lens (\Endpoint' {externalId} -> externalId) (\s@Endpoint' {} a -> s {externalId = a} :: Endpoint)

-- | The status of the endpoint.
endpoint_status :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_status = Lens.lens (\Endpoint' {status} -> status) (\s@Endpoint' {} a -> s {status = a} :: Endpoint)

-- | The type of endpoint. Valid values are @source@ and @target@.
endpoint_endpointType :: Lens.Lens' Endpoint (Prelude.Maybe ReplicationEndpointTypeValue)
endpoint_endpointType = Lens.lens (\Endpoint' {endpointType} -> endpointType) (\s@Endpoint' {} a -> s {endpointType = a} :: Endpoint)

-- | The settings for the Amazon Kinesis target endpoint. For more
-- information, see the @KinesisSettings@ structure.
endpoint_kinesisSettings :: Lens.Lens' Endpoint (Prelude.Maybe KinesisSettings)
endpoint_kinesisSettings = Lens.lens (\Endpoint' {kinesisSettings} -> kinesisSettings) (\s@Endpoint' {} a -> s {kinesisSettings = a} :: Endpoint)

-- | The settings for the Oracle source and target endpoint. For more
-- information, see the @OracleSettings@ structure.
endpoint_oracleSettings :: Lens.Lens' Endpoint (Prelude.Maybe OracleSettings)
endpoint_oracleSettings = Lens.lens (\Endpoint' {oracleSettings} -> oracleSettings) (\s@Endpoint' {} a -> s {oracleSettings = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
endpoint_certificateArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_certificateArn = Lens.lens (\Endpoint' {certificateArn} -> certificateArn) (\s@Endpoint' {} a -> s {certificateArn = a} :: Endpoint)

-- | The settings for the DynamoDB target endpoint. For more information, see
-- the @DynamoDBSettings@ structure.
endpoint_dynamoDbSettings :: Lens.Lens' Endpoint (Prelude.Maybe DynamoDbSettings)
endpoint_dynamoDbSettings = Lens.lens (\Endpoint' {dynamoDbSettings} -> dynamoDbSettings) (\s@Endpoint' {} a -> s {dynamoDbSettings = a} :: Endpoint)

-- | The settings for the Redis target endpoint. For more information, see
-- the @RedisSettings@ structure.
endpoint_redisSettings :: Lens.Lens' Endpoint (Prelude.Maybe RedisSettings)
endpoint_redisSettings = Lens.lens (\Endpoint' {redisSettings} -> redisSettings) (\s@Endpoint' {} a -> s {redisSettings = a} :: Endpoint)

-- | The settings for the S3 target endpoint. For more information, see the
-- @S3Settings@ structure.
endpoint_s3Settings :: Lens.Lens' Endpoint (Prelude.Maybe S3Settings)
endpoint_s3Settings = Lens.lens (\Endpoint' {s3Settings} -> s3Settings) (\s@Endpoint' {} a -> s {s3Settings = a} :: Endpoint)

-- | An KMS key identifier that is used to encrypt the connection parameters
-- for the endpoint.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
endpoint_kmsKeyId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_kmsKeyId = Lens.lens (\Endpoint' {kmsKeyId} -> kmsKeyId) (\s@Endpoint' {} a -> s {kmsKeyId = a} :: Endpoint)

-- | The settings for the Microsoft SQL Server source and target endpoint.
-- For more information, see the @MicrosoftSQLServerSettings@ structure.
endpoint_microsoftSQLServerSettings :: Lens.Lens' Endpoint (Prelude.Maybe MicrosoftSQLServerSettings)
endpoint_microsoftSQLServerSettings = Lens.lens (\Endpoint' {microsoftSQLServerSettings} -> microsoftSQLServerSettings) (\s@Endpoint' {} a -> s {microsoftSQLServerSettings = a} :: Endpoint)

-- | The settings for the Apache Kafka target endpoint. For more information,
-- see the @KafkaSettings@ structure.
endpoint_kafkaSettings :: Lens.Lens' Endpoint (Prelude.Maybe KafkaSettings)
endpoint_kafkaSettings = Lens.lens (\Endpoint' {kafkaSettings} -> kafkaSettings) (\s@Endpoint' {} a -> s {kafkaSettings = a} :: Endpoint)

-- | The settings for the DMS Transfer type source. For more information, see
-- the DmsTransferSettings structure.
endpoint_dmsTransferSettings :: Lens.Lens' Endpoint (Prelude.Maybe DmsTransferSettings)
endpoint_dmsTransferSettings = Lens.lens (\Endpoint' {dmsTransferSettings} -> dmsTransferSettings) (\s@Endpoint' {} a -> s {dmsTransferSettings = a} :: Endpoint)

-- | The settings for the SAP ASE source and target endpoint. For more
-- information, see the @SybaseSettings@ structure.
endpoint_sybaseSettings :: Lens.Lens' Endpoint (Prelude.Maybe SybaseSettings)
endpoint_sybaseSettings = Lens.lens (\Endpoint' {sybaseSettings} -> sybaseSettings) (\s@Endpoint' {} a -> s {sybaseSettings = a} :: Endpoint)

-- | The settings for the PostgreSQL source and target endpoint. For more
-- information, see the @PostgreSQLSettings@ structure.
endpoint_postgreSQLSettings :: Lens.Lens' Endpoint (Prelude.Maybe PostgreSQLSettings)
endpoint_postgreSQLSettings = Lens.lens (\Endpoint' {postgreSQLSettings} -> postgreSQLSettings) (\s@Endpoint' {} a -> s {postgreSQLSettings = a} :: Endpoint)

-- | The settings for the IBM Db2 LUW source endpoint. For more information,
-- see the @IBMDb2Settings@ structure.
endpoint_iBMDb2Settings :: Lens.Lens' Endpoint (Prelude.Maybe IBMDb2Settings)
endpoint_iBMDb2Settings = Lens.lens (\Endpoint' {iBMDb2Settings} -> iBMDb2Settings) (\s@Endpoint' {} a -> s {iBMDb2Settings = a} :: Endpoint)

-- | The settings for the MongoDB source endpoint. For more information, see
-- the @MongoDbSettings@ structure.
endpoint_mongoDbSettings :: Lens.Lens' Endpoint (Prelude.Maybe MongoDbSettings)
endpoint_mongoDbSettings = Lens.lens (\Endpoint' {mongoDbSettings} -> mongoDbSettings) (\s@Endpoint' {} a -> s {mongoDbSettings = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
endpoint_endpointArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointArn = Lens.lens (\Endpoint' {endpointArn} -> endpointArn) (\s@Endpoint' {} a -> s {endpointArn = a} :: Endpoint)

instance Data.FromJSON Endpoint where
  parseJSON =
    Data.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "ElasticsearchSettings")
            Prelude.<*> (x Data..:? "RedshiftSettings")
            Prelude.<*> (x Data..:? "ExternalTableDefinition")
            Prelude.<*> (x Data..:? "EngineName")
            Prelude.<*> (x Data..:? "EngineDisplayName")
            Prelude.<*> (x Data..:? "MySQLSettings")
            Prelude.<*> (x Data..:? "SslMode")
            Prelude.<*> (x Data..:? "EndpointIdentifier")
            Prelude.<*> (x Data..:? "ServerName")
            Prelude.<*> (x Data..:? "DocDbSettings")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Username")
            Prelude.<*> (x Data..:? "GcpMySQLSettings")
            Prelude.<*> (x Data..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Data..:? "ExtraConnectionAttributes")
            Prelude.<*> (x Data..:? "NeptuneSettings")
            Prelude.<*> (x Data..:? "ExternalId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "EndpointType")
            Prelude.<*> (x Data..:? "KinesisSettings")
            Prelude.<*> (x Data..:? "OracleSettings")
            Prelude.<*> (x Data..:? "CertificateArn")
            Prelude.<*> (x Data..:? "DynamoDbSettings")
            Prelude.<*> (x Data..:? "RedisSettings")
            Prelude.<*> (x Data..:? "S3Settings")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "MicrosoftSQLServerSettings")
            Prelude.<*> (x Data..:? "KafkaSettings")
            Prelude.<*> (x Data..:? "DmsTransferSettings")
            Prelude.<*> (x Data..:? "SybaseSettings")
            Prelude.<*> (x Data..:? "PostgreSQLSettings")
            Prelude.<*> (x Data..:? "IBMDb2Settings")
            Prelude.<*> (x Data..:? "MongoDbSettings")
            Prelude.<*> (x Data..:? "EndpointArn")
      )

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` elasticsearchSettings
      `Prelude.hashWithSalt` redshiftSettings
      `Prelude.hashWithSalt` externalTableDefinition
      `Prelude.hashWithSalt` engineName
      `Prelude.hashWithSalt` engineDisplayName
      `Prelude.hashWithSalt` mySQLSettings
      `Prelude.hashWithSalt` sslMode
      `Prelude.hashWithSalt` endpointIdentifier
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` docDbSettings
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` gcpMySQLSettings
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` extraConnectionAttributes
      `Prelude.hashWithSalt` neptuneSettings
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` kinesisSettings
      `Prelude.hashWithSalt` oracleSettings
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` dynamoDbSettings
      `Prelude.hashWithSalt` redisSettings
      `Prelude.hashWithSalt` s3Settings
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` microsoftSQLServerSettings
      `Prelude.hashWithSalt` kafkaSettings
      `Prelude.hashWithSalt` dmsTransferSettings
      `Prelude.hashWithSalt` sybaseSettings
      `Prelude.hashWithSalt` postgreSQLSettings
      `Prelude.hashWithSalt` iBMDb2Settings
      `Prelude.hashWithSalt` mongoDbSettings
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf elasticsearchSettings
      `Prelude.seq` Prelude.rnf redshiftSettings
      `Prelude.seq` Prelude.rnf externalTableDefinition
      `Prelude.seq` Prelude.rnf engineName
      `Prelude.seq` Prelude.rnf engineDisplayName
      `Prelude.seq` Prelude.rnf mySQLSettings
      `Prelude.seq` Prelude.rnf sslMode
      `Prelude.seq` Prelude.rnf endpointIdentifier
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf docDbSettings
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf gcpMySQLSettings
      `Prelude.seq` Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf extraConnectionAttributes
      `Prelude.seq` Prelude.rnf neptuneSettings
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf kinesisSettings
      `Prelude.seq` Prelude.rnf oracleSettings
      `Prelude.seq` Prelude.rnf
        certificateArn
      `Prelude.seq` Prelude.rnf
        dynamoDbSettings
      `Prelude.seq` Prelude.rnf
        redisSettings
      `Prelude.seq` Prelude.rnf
        s3Settings
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        microsoftSQLServerSettings
      `Prelude.seq` Prelude.rnf
        kafkaSettings
      `Prelude.seq` Prelude.rnf
        dmsTransferSettings
      `Prelude.seq` Prelude.rnf
        sybaseSettings
      `Prelude.seq` Prelude.rnf
        postgreSQLSettings
      `Prelude.seq` Prelude.rnf
        iBMDb2Settings
      `Prelude.seq` Prelude.rnf
        mongoDbSettings
      `Prelude.seq` Prelude.rnf
        endpointArn
