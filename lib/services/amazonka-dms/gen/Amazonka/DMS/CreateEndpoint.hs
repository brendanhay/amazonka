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
-- Module      : Amazonka.DMS.CreateEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the provided settings.
--
-- For a MySQL source or target endpoint, don\'t explicitly specify the
-- database using the @DatabaseName@ request parameter on the
-- @CreateEndpoint@ API call. Specifying @DatabaseName@ when you create a
-- MySQL endpoint replicates all the task tables to this single database.
-- For MySQL endpoints, you specify the database only when you specify the
-- schema in the table-mapping rules of the DMS task.
module Amazonka.DMS.CreateEndpoint
  ( -- * Creating a Request
    CreateEndpoint (..),
    newCreateEndpoint,

    -- * Request Lenses
    createEndpoint_certificateArn,
    createEndpoint_databaseName,
    createEndpoint_dmsTransferSettings,
    createEndpoint_docDbSettings,
    createEndpoint_dynamoDbSettings,
    createEndpoint_elasticsearchSettings,
    createEndpoint_externalTableDefinition,
    createEndpoint_extraConnectionAttributes,
    createEndpoint_gcpMySQLSettings,
    createEndpoint_iBMDb2Settings,
    createEndpoint_kafkaSettings,
    createEndpoint_kinesisSettings,
    createEndpoint_kmsKeyId,
    createEndpoint_microsoftSQLServerSettings,
    createEndpoint_mongoDbSettings,
    createEndpoint_mySQLSettings,
    createEndpoint_neptuneSettings,
    createEndpoint_oracleSettings,
    createEndpoint_password,
    createEndpoint_port,
    createEndpoint_postgreSQLSettings,
    createEndpoint_redisSettings,
    createEndpoint_redshiftSettings,
    createEndpoint_resourceIdentifier,
    createEndpoint_s3Settings,
    createEndpoint_serverName,
    createEndpoint_serviceAccessRoleArn,
    createEndpoint_sslMode,
    createEndpoint_sybaseSettings,
    createEndpoint_tags,
    createEndpoint_username,
    createEndpoint_endpointIdentifier,
    createEndpoint_endpointType,
    createEndpoint_engineName,

    -- * Destructuring the Response
    CreateEndpointResponse (..),
    newCreateEndpointResponse,

    -- * Response Lenses
    createEndpointResponse_endpoint,
    createEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint database. For a MySQL source or target
    -- endpoint, do not specify DatabaseName. To migrate to a specific
    -- database, use this setting and @targetDbType@.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The settings in JSON format for the DMS transfer type of source
    -- endpoint.
    --
    -- Possible settings include the following:
    --
    -- -   @ServiceAccessRoleArn@ - The Amazon Resource Name (ARN) used by the
    --     service access IAM role. The role must allow the @iam:PassRole@
    --     action.
    --
    -- -   @BucketName@ - The name of the S3 bucket to use.
    --
    -- Shorthand syntax for these settings is as follows:
    -- @ServiceAccessRoleArn=string,BucketName=string@
    --
    -- JSON syntax for these settings is as follows:
    -- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", } @
    dmsTransferSettings :: Prelude.Maybe DmsTransferSettings,
    docDbSettings :: Prelude.Maybe DocDbSettings,
    -- | Settings in JSON format for the target Amazon DynamoDB endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html#CHAP_Target.DynamoDB.ObjectMapping Using Object Mapping to Migrate Data to DynamoDB>
    -- in the /Database Migration Service User Guide./
    dynamoDbSettings :: Prelude.Maybe DynamoDbSettings,
    -- | Settings in JSON format for the target OpenSearch endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using OpenSearch as a Target for DMS>
    -- in the /Database Migration Service User Guide/.
    elasticsearchSettings :: Prelude.Maybe ElasticsearchSettings,
    -- | The external table definition.
    externalTableDefinition :: Prelude.Maybe Prelude.Text,
    -- | Additional attributes associated with the connection. Each attribute is
    -- specified as a name-value pair associated by an equal sign (=). Multiple
    -- attributes are separated by a semicolon (;) with no additional white
    -- space. For information on the attributes available for connecting your
    -- source or target endpoint, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with DMS Endpoints>
    -- in the /Database Migration Service User Guide./
    extraConnectionAttributes :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the source GCP MySQL endpoint.
    gcpMySQLSettings :: Prelude.Maybe GcpMySQLSettings,
    -- | Settings in JSON format for the source IBM Db2 LUW endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html#CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for DMS>
    -- in the /Database Migration Service User Guide./
    iBMDb2Settings :: Prelude.Maybe IBMDb2Settings,
    -- | Settings in JSON format for the target Apache Kafka endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html#CHAP_Target.Kafka.ObjectMapping Using object mapping to migrate data to a Kafka topic>
    -- in the /Database Migration Service User Guide./
    kafkaSettings :: Prelude.Maybe KafkaSettings,
    -- | Settings in JSON format for the target endpoint for Amazon Kinesis Data
    -- Streams. For more information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html#CHAP_Target.Kinesis.ObjectMapping Using object mapping to migrate data to a Kinesis data stream>
    -- in the /Database Migration Service User Guide./
    kinesisSettings :: Prelude.Maybe KinesisSettings,
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
    -- | Settings in JSON format for the source and target Microsoft SQL Server
    -- endpoint. For information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html#CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html#CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for DMS>
    -- in the /Database Migration Service User Guide./
    microsoftSQLServerSettings :: Prelude.Maybe MicrosoftSQLServerSettings,
    -- | Settings in JSON format for the source MongoDB endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Endpoint configuration settings when using MongoDB as a source for Database Migration Service>
    -- in the /Database Migration Service User Guide./
    mongoDbSettings :: Prelude.Maybe MongoDbSettings,
    -- | Settings in JSON format for the source and target MySQL endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html#CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html#CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for DMS>
    -- in the /Database Migration Service User Guide./
    mySQLSettings :: Prelude.Maybe MySQLSettings,
    -- | Settings in JSON format for the target Amazon Neptune endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying graph-mapping rules using Gremlin and R2RML for Amazon Neptune as a target>
    -- in the /Database Migration Service User Guide./
    neptuneSettings :: Prelude.Maybe NeptuneSettings,
    -- | Settings in JSON format for the source and target Oracle endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html#CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for DMS>
    -- in the /Database Migration Service User Guide./
    oracleSettings :: Prelude.Maybe OracleSettings,
    -- | The password to be used to log in to the endpoint database.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The port used by the endpoint database.
    port :: Prelude.Maybe Prelude.Int,
    -- | Settings in JSON format for the source and target PostgreSQL endpoint.
    -- For information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html#CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for DMS>
    -- in the /Database Migration Service User Guide./
    postgreSQLSettings :: Prelude.Maybe PostgreSQLSettings,
    -- | Settings in JSON format for the target Redis endpoint.
    redisSettings :: Prelude.Maybe RedisSettings,
    redshiftSettings :: Prelude.Maybe RedshiftSettings,
    -- | A friendly name for the resource identifier at the end of the
    -- @EndpointArn@ response parameter that is returned in the created
    -- @Endpoint@ object. The value for this parameter can have up to 31
    -- characters. It can contain only ASCII letters, digits, and hyphen
    -- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
    -- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
    -- For example, this value might result in the @EndpointArn@ value
    -- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
    -- specify a @ResourceIdentifier@ value, DMS generates a default identifier
    -- value for the end of @EndpointArn@.
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the target Amazon S3 endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for DMS>
    -- in the /Database Migration Service User Guide./
    s3Settings :: Prelude.Maybe S3Settings,
    -- | The name of the server where the endpoint database resides.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the service access role that you want
    -- to use to create the endpoint. The role must allow the @iam:PassRole@
    -- action.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The
    -- default is @none@
    sslMode :: Prelude.Maybe DmsSslModeValue,
    -- | Settings in JSON format for the source and target SAP ASE endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html#CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html#CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for DMS>
    -- in the /Database Migration Service User Guide./
    sybaseSettings :: Prelude.Maybe SybaseSettings,
    -- | One or more tags to be assigned to the endpoint.
    tags :: Prelude.Maybe [Tag],
    -- | The user name to be used to log in to the endpoint database.
    username :: Prelude.Maybe Prelude.Text,
    -- | The database endpoint identifier. Identifiers must begin with a letter
    -- and must contain only ASCII letters, digits, and hyphens. They can\'t
    -- end with a hyphen, or contain two consecutive hyphens.
    endpointIdentifier :: Prelude.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: ReplicationEndpointTypeValue,
    -- | The type of engine for the endpoint. Valid values, depending on the
    -- @EndpointType@ value, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
    -- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"opensearch\"@,
    -- @\"redshift\"@, @\"s3\"@, @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@,
    -- @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@,
    -- @\"elasticsearch\"@, @\"docdb\"@, @\"sqlserver\"@, @\"neptune\"@, and
    -- @\"babelfish\"@.
    engineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'createEndpoint_certificateArn' - The Amazon Resource Name (ARN) for the certificate.
--
-- 'databaseName', 'createEndpoint_databaseName' - The name of the endpoint database. For a MySQL source or target
-- endpoint, do not specify DatabaseName. To migrate to a specific
-- database, use this setting and @targetDbType@.
--
-- 'dmsTransferSettings', 'createEndpoint_dmsTransferSettings' - The settings in JSON format for the DMS transfer type of source
-- endpoint.
--
-- Possible settings include the following:
--
-- -   @ServiceAccessRoleArn@ - The Amazon Resource Name (ARN) used by the
--     service access IAM role. The role must allow the @iam:PassRole@
--     action.
--
-- -   @BucketName@ - The name of the S3 bucket to use.
--
-- Shorthand syntax for these settings is as follows:
-- @ServiceAccessRoleArn=string,BucketName=string@
--
-- JSON syntax for these settings is as follows:
-- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", } @
--
-- 'docDbSettings', 'createEndpoint_docDbSettings' - Undocumented member.
--
-- 'dynamoDbSettings', 'createEndpoint_dynamoDbSettings' - Settings in JSON format for the target Amazon DynamoDB endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html#CHAP_Target.DynamoDB.ObjectMapping Using Object Mapping to Migrate Data to DynamoDB>
-- in the /Database Migration Service User Guide./
--
-- 'elasticsearchSettings', 'createEndpoint_elasticsearchSettings' - Settings in JSON format for the target OpenSearch endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using OpenSearch as a Target for DMS>
-- in the /Database Migration Service User Guide/.
--
-- 'externalTableDefinition', 'createEndpoint_externalTableDefinition' - The external table definition.
--
-- 'extraConnectionAttributes', 'createEndpoint_extraConnectionAttributes' - Additional attributes associated with the connection. Each attribute is
-- specified as a name-value pair associated by an equal sign (=). Multiple
-- attributes are separated by a semicolon (;) with no additional white
-- space. For information on the attributes available for connecting your
-- source or target endpoint, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with DMS Endpoints>
-- in the /Database Migration Service User Guide./
--
-- 'gcpMySQLSettings', 'createEndpoint_gcpMySQLSettings' - Settings in JSON format for the source GCP MySQL endpoint.
--
-- 'iBMDb2Settings', 'createEndpoint_iBMDb2Settings' - Settings in JSON format for the source IBM Db2 LUW endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html#CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for DMS>
-- in the /Database Migration Service User Guide./
--
-- 'kafkaSettings', 'createEndpoint_kafkaSettings' - Settings in JSON format for the target Apache Kafka endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html#CHAP_Target.Kafka.ObjectMapping Using object mapping to migrate data to a Kafka topic>
-- in the /Database Migration Service User Guide./
--
-- 'kinesisSettings', 'createEndpoint_kinesisSettings' - Settings in JSON format for the target endpoint for Amazon Kinesis Data
-- Streams. For more information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html#CHAP_Target.Kinesis.ObjectMapping Using object mapping to migrate data to a Kinesis data stream>
-- in the /Database Migration Service User Guide./
--
-- 'kmsKeyId', 'createEndpoint_kmsKeyId' - An KMS key identifier that is used to encrypt the connection parameters
-- for the endpoint.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
--
-- 'microsoftSQLServerSettings', 'createEndpoint_microsoftSQLServerSettings' - Settings in JSON format for the source and target Microsoft SQL Server
-- endpoint. For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html#CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html#CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for DMS>
-- in the /Database Migration Service User Guide./
--
-- 'mongoDbSettings', 'createEndpoint_mongoDbSettings' - Settings in JSON format for the source MongoDB endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Endpoint configuration settings when using MongoDB as a source for Database Migration Service>
-- in the /Database Migration Service User Guide./
--
-- 'mySQLSettings', 'createEndpoint_mySQLSettings' - Settings in JSON format for the source and target MySQL endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html#CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html#CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for DMS>
-- in the /Database Migration Service User Guide./
--
-- 'neptuneSettings', 'createEndpoint_neptuneSettings' - Settings in JSON format for the target Amazon Neptune endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying graph-mapping rules using Gremlin and R2RML for Amazon Neptune as a target>
-- in the /Database Migration Service User Guide./
--
-- 'oracleSettings', 'createEndpoint_oracleSettings' - Settings in JSON format for the source and target Oracle endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html#CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for DMS>
-- in the /Database Migration Service User Guide./
--
-- 'password', 'createEndpoint_password' - The password to be used to log in to the endpoint database.
--
-- 'port', 'createEndpoint_port' - The port used by the endpoint database.
--
-- 'postgreSQLSettings', 'createEndpoint_postgreSQLSettings' - Settings in JSON format for the source and target PostgreSQL endpoint.
-- For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html#CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for DMS>
-- in the /Database Migration Service User Guide./
--
-- 'redisSettings', 'createEndpoint_redisSettings' - Settings in JSON format for the target Redis endpoint.
--
-- 'redshiftSettings', 'createEndpoint_redshiftSettings' - Undocumented member.
--
-- 'resourceIdentifier', 'createEndpoint_resourceIdentifier' - A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, DMS generates a default identifier
-- value for the end of @EndpointArn@.
--
-- 's3Settings', 'createEndpoint_s3Settings' - Settings in JSON format for the target Amazon S3 endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for DMS>
-- in the /Database Migration Service User Guide./
--
-- 'serverName', 'createEndpoint_serverName' - The name of the server where the endpoint database resides.
--
-- 'serviceAccessRoleArn', 'createEndpoint_serviceAccessRoleArn' - The Amazon Resource Name (ARN) for the service access role that you want
-- to use to create the endpoint. The role must allow the @iam:PassRole@
-- action.
--
-- 'sslMode', 'createEndpoint_sslMode' - The Secure Sockets Layer (SSL) mode to use for the SSL connection. The
-- default is @none@
--
-- 'sybaseSettings', 'createEndpoint_sybaseSettings' - Settings in JSON format for the source and target SAP ASE endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html#CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html#CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for DMS>
-- in the /Database Migration Service User Guide./
--
-- 'tags', 'createEndpoint_tags' - One or more tags to be assigned to the endpoint.
--
-- 'username', 'createEndpoint_username' - The user name to be used to log in to the endpoint database.
--
-- 'endpointIdentifier', 'createEndpoint_endpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen, or contain two consecutive hyphens.
--
-- 'endpointType', 'createEndpoint_endpointType' - The type of endpoint. Valid values are @source@ and @target@.
--
-- 'engineName', 'createEndpoint_engineName' - The type of engine for the endpoint. Valid values, depending on the
-- @EndpointType@ value, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
-- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"opensearch\"@,
-- @\"redshift\"@, @\"s3\"@, @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@,
-- @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@,
-- @\"elasticsearch\"@, @\"docdb\"@, @\"sqlserver\"@, @\"neptune\"@, and
-- @\"babelfish\"@.
newCreateEndpoint ::
  -- | 'endpointIdentifier'
  Prelude.Text ->
  -- | 'endpointType'
  ReplicationEndpointTypeValue ->
  -- | 'engineName'
  Prelude.Text ->
  CreateEndpoint
newCreateEndpoint
  pEndpointIdentifier_
  pEndpointType_
  pEngineName_ =
    CreateEndpoint'
      { certificateArn = Prelude.Nothing,
        databaseName = Prelude.Nothing,
        dmsTransferSettings = Prelude.Nothing,
        docDbSettings = Prelude.Nothing,
        dynamoDbSettings = Prelude.Nothing,
        elasticsearchSettings = Prelude.Nothing,
        externalTableDefinition = Prelude.Nothing,
        extraConnectionAttributes = Prelude.Nothing,
        gcpMySQLSettings = Prelude.Nothing,
        iBMDb2Settings = Prelude.Nothing,
        kafkaSettings = Prelude.Nothing,
        kinesisSettings = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        microsoftSQLServerSettings = Prelude.Nothing,
        mongoDbSettings = Prelude.Nothing,
        mySQLSettings = Prelude.Nothing,
        neptuneSettings = Prelude.Nothing,
        oracleSettings = Prelude.Nothing,
        password = Prelude.Nothing,
        port = Prelude.Nothing,
        postgreSQLSettings = Prelude.Nothing,
        redisSettings = Prelude.Nothing,
        redshiftSettings = Prelude.Nothing,
        resourceIdentifier = Prelude.Nothing,
        s3Settings = Prelude.Nothing,
        serverName = Prelude.Nothing,
        serviceAccessRoleArn = Prelude.Nothing,
        sslMode = Prelude.Nothing,
        sybaseSettings = Prelude.Nothing,
        tags = Prelude.Nothing,
        username = Prelude.Nothing,
        endpointIdentifier = pEndpointIdentifier_,
        endpointType = pEndpointType_,
        engineName = pEngineName_
      }

-- | The Amazon Resource Name (ARN) for the certificate.
createEndpoint_certificateArn :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_certificateArn = Lens.lens (\CreateEndpoint' {certificateArn} -> certificateArn) (\s@CreateEndpoint' {} a -> s {certificateArn = a} :: CreateEndpoint)

-- | The name of the endpoint database. For a MySQL source or target
-- endpoint, do not specify DatabaseName. To migrate to a specific
-- database, use this setting and @targetDbType@.
createEndpoint_databaseName :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_databaseName = Lens.lens (\CreateEndpoint' {databaseName} -> databaseName) (\s@CreateEndpoint' {} a -> s {databaseName = a} :: CreateEndpoint)

-- | The settings in JSON format for the DMS transfer type of source
-- endpoint.
--
-- Possible settings include the following:
--
-- -   @ServiceAccessRoleArn@ - The Amazon Resource Name (ARN) used by the
--     service access IAM role. The role must allow the @iam:PassRole@
--     action.
--
-- -   @BucketName@ - The name of the S3 bucket to use.
--
-- Shorthand syntax for these settings is as follows:
-- @ServiceAccessRoleArn=string,BucketName=string@
--
-- JSON syntax for these settings is as follows:
-- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", } @
createEndpoint_dmsTransferSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe DmsTransferSettings)
createEndpoint_dmsTransferSettings = Lens.lens (\CreateEndpoint' {dmsTransferSettings} -> dmsTransferSettings) (\s@CreateEndpoint' {} a -> s {dmsTransferSettings = a} :: CreateEndpoint)

-- | Undocumented member.
createEndpoint_docDbSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe DocDbSettings)
createEndpoint_docDbSettings = Lens.lens (\CreateEndpoint' {docDbSettings} -> docDbSettings) (\s@CreateEndpoint' {} a -> s {docDbSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html#CHAP_Target.DynamoDB.ObjectMapping Using Object Mapping to Migrate Data to DynamoDB>
-- in the /Database Migration Service User Guide./
createEndpoint_dynamoDbSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe DynamoDbSettings)
createEndpoint_dynamoDbSettings = Lens.lens (\CreateEndpoint' {dynamoDbSettings} -> dynamoDbSettings) (\s@CreateEndpoint' {} a -> s {dynamoDbSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the target OpenSearch endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using OpenSearch as a Target for DMS>
-- in the /Database Migration Service User Guide/.
createEndpoint_elasticsearchSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe ElasticsearchSettings)
createEndpoint_elasticsearchSettings = Lens.lens (\CreateEndpoint' {elasticsearchSettings} -> elasticsearchSettings) (\s@CreateEndpoint' {} a -> s {elasticsearchSettings = a} :: CreateEndpoint)

-- | The external table definition.
createEndpoint_externalTableDefinition :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_externalTableDefinition = Lens.lens (\CreateEndpoint' {externalTableDefinition} -> externalTableDefinition) (\s@CreateEndpoint' {} a -> s {externalTableDefinition = a} :: CreateEndpoint)

-- | Additional attributes associated with the connection. Each attribute is
-- specified as a name-value pair associated by an equal sign (=). Multiple
-- attributes are separated by a semicolon (;) with no additional white
-- space. For information on the attributes available for connecting your
-- source or target endpoint, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with DMS Endpoints>
-- in the /Database Migration Service User Guide./
createEndpoint_extraConnectionAttributes :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_extraConnectionAttributes = Lens.lens (\CreateEndpoint' {extraConnectionAttributes} -> extraConnectionAttributes) (\s@CreateEndpoint' {} a -> s {extraConnectionAttributes = a} :: CreateEndpoint)

-- | Settings in JSON format for the source GCP MySQL endpoint.
createEndpoint_gcpMySQLSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe GcpMySQLSettings)
createEndpoint_gcpMySQLSettings = Lens.lens (\CreateEndpoint' {gcpMySQLSettings} -> gcpMySQLSettings) (\s@CreateEndpoint' {} a -> s {gcpMySQLSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html#CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for DMS>
-- in the /Database Migration Service User Guide./
createEndpoint_iBMDb2Settings :: Lens.Lens' CreateEndpoint (Prelude.Maybe IBMDb2Settings)
createEndpoint_iBMDb2Settings = Lens.lens (\CreateEndpoint' {iBMDb2Settings} -> iBMDb2Settings) (\s@CreateEndpoint' {} a -> s {iBMDb2Settings = a} :: CreateEndpoint)

-- | Settings in JSON format for the target Apache Kafka endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html#CHAP_Target.Kafka.ObjectMapping Using object mapping to migrate data to a Kafka topic>
-- in the /Database Migration Service User Guide./
createEndpoint_kafkaSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe KafkaSettings)
createEndpoint_kafkaSettings = Lens.lens (\CreateEndpoint' {kafkaSettings} -> kafkaSettings) (\s@CreateEndpoint' {} a -> s {kafkaSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data
-- Streams. For more information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html#CHAP_Target.Kinesis.ObjectMapping Using object mapping to migrate data to a Kinesis data stream>
-- in the /Database Migration Service User Guide./
createEndpoint_kinesisSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe KinesisSettings)
createEndpoint_kinesisSettings = Lens.lens (\CreateEndpoint' {kinesisSettings} -> kinesisSettings) (\s@CreateEndpoint' {} a -> s {kinesisSettings = a} :: CreateEndpoint)

-- | An KMS key identifier that is used to encrypt the connection parameters
-- for the endpoint.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
createEndpoint_kmsKeyId :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_kmsKeyId = Lens.lens (\CreateEndpoint' {kmsKeyId} -> kmsKeyId) (\s@CreateEndpoint' {} a -> s {kmsKeyId = a} :: CreateEndpoint)

-- | Settings in JSON format for the source and target Microsoft SQL Server
-- endpoint. For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html#CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html#CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for DMS>
-- in the /Database Migration Service User Guide./
createEndpoint_microsoftSQLServerSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe MicrosoftSQLServerSettings)
createEndpoint_microsoftSQLServerSettings = Lens.lens (\CreateEndpoint' {microsoftSQLServerSettings} -> microsoftSQLServerSettings) (\s@CreateEndpoint' {} a -> s {microsoftSQLServerSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the source MongoDB endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Endpoint configuration settings when using MongoDB as a source for Database Migration Service>
-- in the /Database Migration Service User Guide./
createEndpoint_mongoDbSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe MongoDbSettings)
createEndpoint_mongoDbSettings = Lens.lens (\CreateEndpoint' {mongoDbSettings} -> mongoDbSettings) (\s@CreateEndpoint' {} a -> s {mongoDbSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the source and target MySQL endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html#CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html#CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for DMS>
-- in the /Database Migration Service User Guide./
createEndpoint_mySQLSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe MySQLSettings)
createEndpoint_mySQLSettings = Lens.lens (\CreateEndpoint' {mySQLSettings} -> mySQLSettings) (\s@CreateEndpoint' {} a -> s {mySQLSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying graph-mapping rules using Gremlin and R2RML for Amazon Neptune as a target>
-- in the /Database Migration Service User Guide./
createEndpoint_neptuneSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe NeptuneSettings)
createEndpoint_neptuneSettings = Lens.lens (\CreateEndpoint' {neptuneSettings} -> neptuneSettings) (\s@CreateEndpoint' {} a -> s {neptuneSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the source and target Oracle endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html#CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for DMS>
-- in the /Database Migration Service User Guide./
createEndpoint_oracleSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe OracleSettings)
createEndpoint_oracleSettings = Lens.lens (\CreateEndpoint' {oracleSettings} -> oracleSettings) (\s@CreateEndpoint' {} a -> s {oracleSettings = a} :: CreateEndpoint)

-- | The password to be used to log in to the endpoint database.
createEndpoint_password :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_password = Lens.lens (\CreateEndpoint' {password} -> password) (\s@CreateEndpoint' {} a -> s {password = a} :: CreateEndpoint) Prelude.. Lens.mapping Data._Sensitive

-- | The port used by the endpoint database.
createEndpoint_port :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Int)
createEndpoint_port = Lens.lens (\CreateEndpoint' {port} -> port) (\s@CreateEndpoint' {} a -> s {port = a} :: CreateEndpoint)

-- | Settings in JSON format for the source and target PostgreSQL endpoint.
-- For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html#CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for DMS>
-- in the /Database Migration Service User Guide./
createEndpoint_postgreSQLSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe PostgreSQLSettings)
createEndpoint_postgreSQLSettings = Lens.lens (\CreateEndpoint' {postgreSQLSettings} -> postgreSQLSettings) (\s@CreateEndpoint' {} a -> s {postgreSQLSettings = a} :: CreateEndpoint)

-- | Settings in JSON format for the target Redis endpoint.
createEndpoint_redisSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe RedisSettings)
createEndpoint_redisSettings = Lens.lens (\CreateEndpoint' {redisSettings} -> redisSettings) (\s@CreateEndpoint' {} a -> s {redisSettings = a} :: CreateEndpoint)

-- | Undocumented member.
createEndpoint_redshiftSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe RedshiftSettings)
createEndpoint_redshiftSettings = Lens.lens (\CreateEndpoint' {redshiftSettings} -> redshiftSettings) (\s@CreateEndpoint' {} a -> s {redshiftSettings = a} :: CreateEndpoint)

-- | A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, DMS generates a default identifier
-- value for the end of @EndpointArn@.
createEndpoint_resourceIdentifier :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_resourceIdentifier = Lens.lens (\CreateEndpoint' {resourceIdentifier} -> resourceIdentifier) (\s@CreateEndpoint' {} a -> s {resourceIdentifier = a} :: CreateEndpoint)

-- | Settings in JSON format for the target Amazon S3 endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for DMS>
-- in the /Database Migration Service User Guide./
createEndpoint_s3Settings :: Lens.Lens' CreateEndpoint (Prelude.Maybe S3Settings)
createEndpoint_s3Settings = Lens.lens (\CreateEndpoint' {s3Settings} -> s3Settings) (\s@CreateEndpoint' {} a -> s {s3Settings = a} :: CreateEndpoint)

-- | The name of the server where the endpoint database resides.
createEndpoint_serverName :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_serverName = Lens.lens (\CreateEndpoint' {serverName} -> serverName) (\s@CreateEndpoint' {} a -> s {serverName = a} :: CreateEndpoint)

-- | The Amazon Resource Name (ARN) for the service access role that you want
-- to use to create the endpoint. The role must allow the @iam:PassRole@
-- action.
createEndpoint_serviceAccessRoleArn :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_serviceAccessRoleArn = Lens.lens (\CreateEndpoint' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@CreateEndpoint' {} a -> s {serviceAccessRoleArn = a} :: CreateEndpoint)

-- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The
-- default is @none@
createEndpoint_sslMode :: Lens.Lens' CreateEndpoint (Prelude.Maybe DmsSslModeValue)
createEndpoint_sslMode = Lens.lens (\CreateEndpoint' {sslMode} -> sslMode) (\s@CreateEndpoint' {} a -> s {sslMode = a} :: CreateEndpoint)

-- | Settings in JSON format for the source and target SAP ASE endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html#CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html#CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for DMS>
-- in the /Database Migration Service User Guide./
createEndpoint_sybaseSettings :: Lens.Lens' CreateEndpoint (Prelude.Maybe SybaseSettings)
createEndpoint_sybaseSettings = Lens.lens (\CreateEndpoint' {sybaseSettings} -> sybaseSettings) (\s@CreateEndpoint' {} a -> s {sybaseSettings = a} :: CreateEndpoint)

-- | One or more tags to be assigned to the endpoint.
createEndpoint_tags :: Lens.Lens' CreateEndpoint (Prelude.Maybe [Tag])
createEndpoint_tags = Lens.lens (\CreateEndpoint' {tags} -> tags) (\s@CreateEndpoint' {} a -> s {tags = a} :: CreateEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The user name to be used to log in to the endpoint database.
createEndpoint_username :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_username = Lens.lens (\CreateEndpoint' {username} -> username) (\s@CreateEndpoint' {} a -> s {username = a} :: CreateEndpoint)

-- | The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen, or contain two consecutive hyphens.
createEndpoint_endpointIdentifier :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_endpointIdentifier = Lens.lens (\CreateEndpoint' {endpointIdentifier} -> endpointIdentifier) (\s@CreateEndpoint' {} a -> s {endpointIdentifier = a} :: CreateEndpoint)

-- | The type of endpoint. Valid values are @source@ and @target@.
createEndpoint_endpointType :: Lens.Lens' CreateEndpoint ReplicationEndpointTypeValue
createEndpoint_endpointType = Lens.lens (\CreateEndpoint' {endpointType} -> endpointType) (\s@CreateEndpoint' {} a -> s {endpointType = a} :: CreateEndpoint)

-- | The type of engine for the endpoint. Valid values, depending on the
-- @EndpointType@ value, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
-- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"opensearch\"@,
-- @\"redshift\"@, @\"s3\"@, @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@,
-- @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@,
-- @\"elasticsearch\"@, @\"docdb\"@, @\"sqlserver\"@, @\"neptune\"@, and
-- @\"babelfish\"@.
createEndpoint_engineName :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_engineName = Lens.lens (\CreateEndpoint' {engineName} -> engineName) (\s@CreateEndpoint' {} a -> s {engineName = a} :: CreateEndpoint)

instance Core.AWSRequest CreateEndpoint where
  type
    AWSResponse CreateEndpoint =
      CreateEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Prelude.<$> (x Data..?> "Endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpoint where
  hashWithSalt _salt CreateEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` dmsTransferSettings
      `Prelude.hashWithSalt` docDbSettings
      `Prelude.hashWithSalt` dynamoDbSettings
      `Prelude.hashWithSalt` elasticsearchSettings
      `Prelude.hashWithSalt` externalTableDefinition
      `Prelude.hashWithSalt` extraConnectionAttributes
      `Prelude.hashWithSalt` gcpMySQLSettings
      `Prelude.hashWithSalt` iBMDb2Settings
      `Prelude.hashWithSalt` kafkaSettings
      `Prelude.hashWithSalt` kinesisSettings
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` microsoftSQLServerSettings
      `Prelude.hashWithSalt` mongoDbSettings
      `Prelude.hashWithSalt` mySQLSettings
      `Prelude.hashWithSalt` neptuneSettings
      `Prelude.hashWithSalt` oracleSettings
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` postgreSQLSettings
      `Prelude.hashWithSalt` redisSettings
      `Prelude.hashWithSalt` redshiftSettings
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` s3Settings
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` sslMode
      `Prelude.hashWithSalt` sybaseSettings
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` endpointIdentifier
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` engineName

instance Prelude.NFData CreateEndpoint where
  rnf CreateEndpoint' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf dmsTransferSettings
      `Prelude.seq` Prelude.rnf docDbSettings
      `Prelude.seq` Prelude.rnf dynamoDbSettings
      `Prelude.seq` Prelude.rnf elasticsearchSettings
      `Prelude.seq` Prelude.rnf externalTableDefinition
      `Prelude.seq` Prelude.rnf extraConnectionAttributes
      `Prelude.seq` Prelude.rnf gcpMySQLSettings
      `Prelude.seq` Prelude.rnf iBMDb2Settings
      `Prelude.seq` Prelude.rnf kafkaSettings
      `Prelude.seq` Prelude.rnf kinesisSettings
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf microsoftSQLServerSettings
      `Prelude.seq` Prelude.rnf mongoDbSettings
      `Prelude.seq` Prelude.rnf mySQLSettings
      `Prelude.seq` Prelude.rnf neptuneSettings
      `Prelude.seq` Prelude.rnf oracleSettings
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf
        postgreSQLSettings
      `Prelude.seq` Prelude.rnf redisSettings
      `Prelude.seq` Prelude.rnf
        redshiftSettings
      `Prelude.seq` Prelude.rnf
        resourceIdentifier
      `Prelude.seq` Prelude.rnf
        s3Settings
      `Prelude.seq` Prelude.rnf
        serverName
      `Prelude.seq` Prelude.rnf
        serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf
        sslMode
      `Prelude.seq` Prelude.rnf
        sybaseSettings
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        username
      `Prelude.seq` Prelude.rnf
        endpointIdentifier
      `Prelude.seq` Prelude.rnf
        endpointType
      `Prelude.seq` Prelude.rnf
        engineName

instance Data.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.CreateEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("DmsTransferSettings" Data..=)
              Prelude.<$> dmsTransferSettings,
            ("DocDbSettings" Data..=) Prelude.<$> docDbSettings,
            ("DynamoDbSettings" Data..=)
              Prelude.<$> dynamoDbSettings,
            ("ElasticsearchSettings" Data..=)
              Prelude.<$> elasticsearchSettings,
            ("ExternalTableDefinition" Data..=)
              Prelude.<$> externalTableDefinition,
            ("ExtraConnectionAttributes" Data..=)
              Prelude.<$> extraConnectionAttributes,
            ("GcpMySQLSettings" Data..=)
              Prelude.<$> gcpMySQLSettings,
            ("IBMDb2Settings" Data..=)
              Prelude.<$> iBMDb2Settings,
            ("KafkaSettings" Data..=) Prelude.<$> kafkaSettings,
            ("KinesisSettings" Data..=)
              Prelude.<$> kinesisSettings,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("MicrosoftSQLServerSettings" Data..=)
              Prelude.<$> microsoftSQLServerSettings,
            ("MongoDbSettings" Data..=)
              Prelude.<$> mongoDbSettings,
            ("MySQLSettings" Data..=) Prelude.<$> mySQLSettings,
            ("NeptuneSettings" Data..=)
              Prelude.<$> neptuneSettings,
            ("OracleSettings" Data..=)
              Prelude.<$> oracleSettings,
            ("Password" Data..=) Prelude.<$> password,
            ("Port" Data..=) Prelude.<$> port,
            ("PostgreSQLSettings" Data..=)
              Prelude.<$> postgreSQLSettings,
            ("RedisSettings" Data..=) Prelude.<$> redisSettings,
            ("RedshiftSettings" Data..=)
              Prelude.<$> redshiftSettings,
            ("ResourceIdentifier" Data..=)
              Prelude.<$> resourceIdentifier,
            ("S3Settings" Data..=) Prelude.<$> s3Settings,
            ("ServerName" Data..=) Prelude.<$> serverName,
            ("ServiceAccessRoleArn" Data..=)
              Prelude.<$> serviceAccessRoleArn,
            ("SslMode" Data..=) Prelude.<$> sslMode,
            ("SybaseSettings" Data..=)
              Prelude.<$> sybaseSettings,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Username" Data..=) Prelude.<$> username,
            Prelude.Just
              ("EndpointIdentifier" Data..= endpointIdentifier),
            Prelude.Just ("EndpointType" Data..= endpointType),
            Prelude.Just ("EngineName" Data..= engineName)
          ]
      )

instance Data.ToPath CreateEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The endpoint that was created.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'createEndpointResponse_endpoint' - The endpoint that was created.
--
-- 'httpStatus', 'createEndpointResponse_httpStatus' - The response's http status code.
newCreateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEndpointResponse
newCreateEndpointResponse pHttpStatus_ =
  CreateEndpointResponse'
    { endpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint that was created.
createEndpointResponse_endpoint :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe Endpoint)
createEndpointResponse_endpoint = Lens.lens (\CreateEndpointResponse' {endpoint} -> endpoint) (\s@CreateEndpointResponse' {} a -> s {endpoint = a} :: CreateEndpointResponse)

-- | The response's http status code.
createEndpointResponse_httpStatus :: Lens.Lens' CreateEndpointResponse Prelude.Int
createEndpointResponse_httpStatus = Lens.lens (\CreateEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointResponse' {} a -> s {httpStatus = a} :: CreateEndpointResponse)

instance Prelude.NFData CreateEndpointResponse where
  rnf CreateEndpointResponse' {..} =
    Prelude.rnf endpoint `Prelude.seq`
      Prelude.rnf httpStatus
