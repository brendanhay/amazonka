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
-- Module      : Network.AWS.DMS.ModifyEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified endpoint.
module Network.AWS.DMS.ModifyEndpoint
  ( -- * Creating a Request
    ModifyEndpoint (..),
    newModifyEndpoint,

    -- * Request Lenses
    modifyEndpoint_sslMode,
    modifyEndpoint_mongoDbSettings,
    modifyEndpoint_neptuneSettings,
    modifyEndpoint_engineName,
    modifyEndpoint_elasticsearchSettings,
    modifyEndpoint_externalTableDefinition,
    modifyEndpoint_endpointType,
    modifyEndpoint_oracleSettings,
    modifyEndpoint_postgreSQLSettings,
    modifyEndpoint_serviceAccessRoleArn,
    modifyEndpoint_certificateArn,
    modifyEndpoint_s3Settings,
    modifyEndpoint_serverName,
    modifyEndpoint_microsoftSQLServerSettings,
    modifyEndpoint_iBMDb2Settings,
    modifyEndpoint_mySQLSettings,
    modifyEndpoint_password,
    modifyEndpoint_dmsTransferSettings,
    modifyEndpoint_port,
    modifyEndpoint_redshiftSettings,
    modifyEndpoint_username,
    modifyEndpoint_kafkaSettings,
    modifyEndpoint_docDbSettings,
    modifyEndpoint_dynamoDbSettings,
    modifyEndpoint_extraConnectionAttributes,
    modifyEndpoint_endpointIdentifier,
    modifyEndpoint_kinesisSettings,
    modifyEndpoint_sybaseSettings,
    modifyEndpoint_databaseName,
    modifyEndpoint_endpointArn,

    -- * Destructuring the Response
    ModifyEndpointResponse (..),
    newModifyEndpointResponse,

    -- * Response Lenses
    modifyEndpointResponse_endpoint,
    modifyEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyEndpoint' smart constructor.
data ModifyEndpoint = ModifyEndpoint'
  { -- | The SSL mode used to connect to the endpoint. The default value is
    -- @none@.
    sslMode :: Prelude.Maybe DmsSslModeValue,
    -- | Settings in JSON format for the source MongoDB endpoint. For more
    -- information about the available settings, see the configuration
    -- properties section in
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    mongoDbSettings :: Prelude.Maybe MongoDbSettings,
    -- | Settings in JSON format for the target Amazon Neptune endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target>
    -- in the /AWS Database Migration Service User Guide./
    neptuneSettings :: Prelude.Maybe NeptuneSettings,
    -- | The type of engine for the endpoint. Valid values, depending on the
    -- EndpointType, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
    -- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@,
    -- @\"s3\"@, @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
    -- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
    -- @\"documentdb\"@, @\"sqlserver\"@, and @\"neptune\"@.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the target Elasticsearch endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    elasticsearchSettings :: Prelude.Maybe ElasticsearchSettings,
    -- | The external table definition.
    externalTableDefinition :: Prelude.Maybe Prelude.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Prelude.Maybe ReplicationEndpointTypeValue,
    -- | Settings in JSON format for the source and target Oracle endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    oracleSettings :: Prelude.Maybe OracleSettings,
    -- | Settings in JSON format for the source and target PostgreSQL endpoint.
    -- For information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    postgreSQLSettings :: Prelude.Maybe PostgreSQLSettings,
    -- | The Amazon Resource Name (ARN) for the service access role you want to
    -- use to modify the endpoint.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate used for SSL
    -- connection.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the target Amazon S3 endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    s3Settings :: Prelude.Maybe S3Settings,
    -- | The name of the server where the endpoint database resides.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the source and target Microsoft SQL Server
    -- endpoint. For information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    microsoftSQLServerSettings :: Prelude.Maybe MicrosoftSQLServerSettings,
    -- | Settings in JSON format for the source IBM Db2 LUW endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    iBMDb2Settings :: Prelude.Maybe IBMDb2Settings,
    -- | Settings in JSON format for the source and target MySQL endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    mySQLSettings :: Prelude.Maybe MySQLSettings,
    -- | The password to be used to login to the endpoint database.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The settings in JSON format for the DMS transfer type of source
    -- endpoint.
    --
    -- Attributes include the following:
    --
    -- -   serviceAccessRoleArn - The AWS Identity and Access Management (IAM)
    --     role that has permission to access the Amazon S3 bucket.
    --
    -- -   BucketName - The name of the S3 bucket to use.
    --
    -- -   compressionType - An optional parameter to use GZIP to compress the
    --     target files. Either set this parameter to NONE (the default) or
    --     don\'t use it to leave the files uncompressed.
    --
    -- Shorthand syntax for these settings is as follows:
    -- @ServiceAccessRoleArn=string ,BucketName=string,CompressionType=string@
    --
    -- JSON syntax for these settings is as follows:
    -- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", \"CompressionType\": \"none\"|\"gzip\" } @
    dmsTransferSettings :: Prelude.Maybe DmsTransferSettings,
    -- | The port used by the endpoint database.
    port :: Prelude.Maybe Prelude.Int,
    redshiftSettings :: Prelude.Maybe RedshiftSettings,
    -- | The user name to be used to login to the endpoint database.
    username :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the target Apache Kafka endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    kafkaSettings :: Prelude.Maybe KafkaSettings,
    -- | Settings in JSON format for the source DocumentDB endpoint. For more
    -- information about the available settings, see the configuration
    -- properties section in
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    docDbSettings :: Prelude.Maybe DocDbSettings,
    -- | Settings in JSON format for the target Amazon DynamoDB endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB>
    -- in the /AWS Database Migration Service User Guide./
    dynamoDbSettings :: Prelude.Maybe DynamoDbSettings,
    -- | Additional attributes associated with the connection. To reset this
    -- parameter, pass the empty string (\"\") as an argument.
    extraConnectionAttributes :: Prelude.Maybe Prelude.Text,
    -- | The database endpoint identifier. Identifiers must begin with a letter
    -- and must contain only ASCII letters, digits, and hyphens. They can\'t
    -- end with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Settings in JSON format for the target endpoint for Amazon Kinesis Data
    -- Streams. For more information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    kinesisSettings :: Prelude.Maybe KinesisSettings,
    -- | Settings in JSON format for the source and target SAP ASE endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    sybaseSettings :: Prelude.Maybe SybaseSettings,
    -- | The name of the endpoint database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslMode', 'modifyEndpoint_sslMode' - The SSL mode used to connect to the endpoint. The default value is
-- @none@.
--
-- 'mongoDbSettings', 'modifyEndpoint_mongoDbSettings' - Settings in JSON format for the source MongoDB endpoint. For more
-- information about the available settings, see the configuration
-- properties section in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
--
-- 'neptuneSettings', 'modifyEndpoint_neptuneSettings' - Settings in JSON format for the target Amazon Neptune endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target>
-- in the /AWS Database Migration Service User Guide./
--
-- 'engineName', 'modifyEndpoint_engineName' - The type of engine for the endpoint. Valid values, depending on the
-- EndpointType, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
-- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@,
-- @\"s3\"@, @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
-- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
-- @\"documentdb\"@, @\"sqlserver\"@, and @\"neptune\"@.
--
-- 'elasticsearchSettings', 'modifyEndpoint_elasticsearchSettings' - Settings in JSON format for the target Elasticsearch endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'externalTableDefinition', 'modifyEndpoint_externalTableDefinition' - The external table definition.
--
-- 'endpointType', 'modifyEndpoint_endpointType' - The type of endpoint. Valid values are @source@ and @target@.
--
-- 'oracleSettings', 'modifyEndpoint_oracleSettings' - Settings in JSON format for the source and target Oracle endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'postgreSQLSettings', 'modifyEndpoint_postgreSQLSettings' - Settings in JSON format for the source and target PostgreSQL endpoint.
-- For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'serviceAccessRoleArn', 'modifyEndpoint_serviceAccessRoleArn' - The Amazon Resource Name (ARN) for the service access role you want to
-- use to modify the endpoint.
--
-- 'certificateArn', 'modifyEndpoint_certificateArn' - The Amazon Resource Name (ARN) of the certificate used for SSL
-- connection.
--
-- 's3Settings', 'modifyEndpoint_s3Settings' - Settings in JSON format for the target Amazon S3 endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'serverName', 'modifyEndpoint_serverName' - The name of the server where the endpoint database resides.
--
-- 'microsoftSQLServerSettings', 'modifyEndpoint_microsoftSQLServerSettings' - Settings in JSON format for the source and target Microsoft SQL Server
-- endpoint. For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'iBMDb2Settings', 'modifyEndpoint_iBMDb2Settings' - Settings in JSON format for the source IBM Db2 LUW endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'mySQLSettings', 'modifyEndpoint_mySQLSettings' - Settings in JSON format for the source and target MySQL endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'password', 'modifyEndpoint_password' - The password to be used to login to the endpoint database.
--
-- 'dmsTransferSettings', 'modifyEndpoint_dmsTransferSettings' - The settings in JSON format for the DMS transfer type of source
-- endpoint.
--
-- Attributes include the following:
--
-- -   serviceAccessRoleArn - The AWS Identity and Access Management (IAM)
--     role that has permission to access the Amazon S3 bucket.
--
-- -   BucketName - The name of the S3 bucket to use.
--
-- -   compressionType - An optional parameter to use GZIP to compress the
--     target files. Either set this parameter to NONE (the default) or
--     don\'t use it to leave the files uncompressed.
--
-- Shorthand syntax for these settings is as follows:
-- @ServiceAccessRoleArn=string ,BucketName=string,CompressionType=string@
--
-- JSON syntax for these settings is as follows:
-- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", \"CompressionType\": \"none\"|\"gzip\" } @
--
-- 'port', 'modifyEndpoint_port' - The port used by the endpoint database.
--
-- 'redshiftSettings', 'modifyEndpoint_redshiftSettings' - Undocumented member.
--
-- 'username', 'modifyEndpoint_username' - The user name to be used to login to the endpoint database.
--
-- 'kafkaSettings', 'modifyEndpoint_kafkaSettings' - Settings in JSON format for the target Apache Kafka endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
--
-- 'docDbSettings', 'modifyEndpoint_docDbSettings' - Settings in JSON format for the source DocumentDB endpoint. For more
-- information about the available settings, see the configuration
-- properties section in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
--
-- 'dynamoDbSettings', 'modifyEndpoint_dynamoDbSettings' - Settings in JSON format for the target Amazon DynamoDB endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB>
-- in the /AWS Database Migration Service User Guide./
--
-- 'extraConnectionAttributes', 'modifyEndpoint_extraConnectionAttributes' - Additional attributes associated with the connection. To reset this
-- parameter, pass the empty string (\"\") as an argument.
--
-- 'endpointIdentifier', 'modifyEndpoint_endpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
--
-- 'kinesisSettings', 'modifyEndpoint_kinesisSettings' - Settings in JSON format for the target endpoint for Amazon Kinesis Data
-- Streams. For more information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
--
-- 'sybaseSettings', 'modifyEndpoint_sybaseSettings' - Settings in JSON format for the source and target SAP ASE endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
--
-- 'databaseName', 'modifyEndpoint_databaseName' - The name of the endpoint database.
--
-- 'endpointArn', 'modifyEndpoint_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
newModifyEndpoint ::
  -- | 'endpointArn'
  Prelude.Text ->
  ModifyEndpoint
newModifyEndpoint pEndpointArn_ =
  ModifyEndpoint'
    { sslMode = Prelude.Nothing,
      mongoDbSettings = Prelude.Nothing,
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
      iBMDb2Settings = Prelude.Nothing,
      mySQLSettings = Prelude.Nothing,
      password = Prelude.Nothing,
      dmsTransferSettings = Prelude.Nothing,
      port = Prelude.Nothing,
      redshiftSettings = Prelude.Nothing,
      username = Prelude.Nothing,
      kafkaSettings = Prelude.Nothing,
      docDbSettings = Prelude.Nothing,
      dynamoDbSettings = Prelude.Nothing,
      extraConnectionAttributes = Prelude.Nothing,
      endpointIdentifier = Prelude.Nothing,
      kinesisSettings = Prelude.Nothing,
      sybaseSettings = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      endpointArn = pEndpointArn_
    }

-- | The SSL mode used to connect to the endpoint. The default value is
-- @none@.
modifyEndpoint_sslMode :: Lens.Lens' ModifyEndpoint (Prelude.Maybe DmsSslModeValue)
modifyEndpoint_sslMode = Lens.lens (\ModifyEndpoint' {sslMode} -> sslMode) (\s@ModifyEndpoint' {} a -> s {sslMode = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source MongoDB endpoint. For more
-- information about the available settings, see the configuration
-- properties section in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_mongoDbSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe MongoDbSettings)
modifyEndpoint_mongoDbSettings = Lens.lens (\ModifyEndpoint' {mongoDbSettings} -> mongoDbSettings) (\s@ModifyEndpoint' {} a -> s {mongoDbSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_neptuneSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe NeptuneSettings)
modifyEndpoint_neptuneSettings = Lens.lens (\ModifyEndpoint' {neptuneSettings} -> neptuneSettings) (\s@ModifyEndpoint' {} a -> s {neptuneSettings = a} :: ModifyEndpoint)

-- | The type of engine for the endpoint. Valid values, depending on the
-- EndpointType, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
-- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@,
-- @\"s3\"@, @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
-- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
-- @\"documentdb\"@, @\"sqlserver\"@, and @\"neptune\"@.
modifyEndpoint_engineName :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_engineName = Lens.lens (\ModifyEndpoint' {engineName} -> engineName) (\s@ModifyEndpoint' {} a -> s {engineName = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Elasticsearch endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_elasticsearchSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe ElasticsearchSettings)
modifyEndpoint_elasticsearchSettings = Lens.lens (\ModifyEndpoint' {elasticsearchSettings} -> elasticsearchSettings) (\s@ModifyEndpoint' {} a -> s {elasticsearchSettings = a} :: ModifyEndpoint)

-- | The external table definition.
modifyEndpoint_externalTableDefinition :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_externalTableDefinition = Lens.lens (\ModifyEndpoint' {externalTableDefinition} -> externalTableDefinition) (\s@ModifyEndpoint' {} a -> s {externalTableDefinition = a} :: ModifyEndpoint)

-- | The type of endpoint. Valid values are @source@ and @target@.
modifyEndpoint_endpointType :: Lens.Lens' ModifyEndpoint (Prelude.Maybe ReplicationEndpointTypeValue)
modifyEndpoint_endpointType = Lens.lens (\ModifyEndpoint' {endpointType} -> endpointType) (\s@ModifyEndpoint' {} a -> s {endpointType = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target Oracle endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_oracleSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe OracleSettings)
modifyEndpoint_oracleSettings = Lens.lens (\ModifyEndpoint' {oracleSettings} -> oracleSettings) (\s@ModifyEndpoint' {} a -> s {oracleSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target PostgreSQL endpoint.
-- For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_postgreSQLSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe PostgreSQLSettings)
modifyEndpoint_postgreSQLSettings = Lens.lens (\ModifyEndpoint' {postgreSQLSettings} -> postgreSQLSettings) (\s@ModifyEndpoint' {} a -> s {postgreSQLSettings = a} :: ModifyEndpoint)

-- | The Amazon Resource Name (ARN) for the service access role you want to
-- use to modify the endpoint.
modifyEndpoint_serviceAccessRoleArn :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_serviceAccessRoleArn = Lens.lens (\ModifyEndpoint' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@ModifyEndpoint' {} a -> s {serviceAccessRoleArn = a} :: ModifyEndpoint)

-- | The Amazon Resource Name (ARN) of the certificate used for SSL
-- connection.
modifyEndpoint_certificateArn :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_certificateArn = Lens.lens (\ModifyEndpoint' {certificateArn} -> certificateArn) (\s@ModifyEndpoint' {} a -> s {certificateArn = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Amazon S3 endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_s3Settings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe S3Settings)
modifyEndpoint_s3Settings = Lens.lens (\ModifyEndpoint' {s3Settings} -> s3Settings) (\s@ModifyEndpoint' {} a -> s {s3Settings = a} :: ModifyEndpoint)

-- | The name of the server where the endpoint database resides.
modifyEndpoint_serverName :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_serverName = Lens.lens (\ModifyEndpoint' {serverName} -> serverName) (\s@ModifyEndpoint' {} a -> s {serverName = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target Microsoft SQL Server
-- endpoint. For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_microsoftSQLServerSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe MicrosoftSQLServerSettings)
modifyEndpoint_microsoftSQLServerSettings = Lens.lens (\ModifyEndpoint' {microsoftSQLServerSettings} -> microsoftSQLServerSettings) (\s@ModifyEndpoint' {} a -> s {microsoftSQLServerSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_iBMDb2Settings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe IBMDb2Settings)
modifyEndpoint_iBMDb2Settings = Lens.lens (\ModifyEndpoint' {iBMDb2Settings} -> iBMDb2Settings) (\s@ModifyEndpoint' {} a -> s {iBMDb2Settings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target MySQL endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_mySQLSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe MySQLSettings)
modifyEndpoint_mySQLSettings = Lens.lens (\ModifyEndpoint' {mySQLSettings} -> mySQLSettings) (\s@ModifyEndpoint' {} a -> s {mySQLSettings = a} :: ModifyEndpoint)

-- | The password to be used to login to the endpoint database.
modifyEndpoint_password :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_password = Lens.lens (\ModifyEndpoint' {password} -> password) (\s@ModifyEndpoint' {} a -> s {password = a} :: ModifyEndpoint) Prelude.. Lens.mapping Core._Sensitive

-- | The settings in JSON format for the DMS transfer type of source
-- endpoint.
--
-- Attributes include the following:
--
-- -   serviceAccessRoleArn - The AWS Identity and Access Management (IAM)
--     role that has permission to access the Amazon S3 bucket.
--
-- -   BucketName - The name of the S3 bucket to use.
--
-- -   compressionType - An optional parameter to use GZIP to compress the
--     target files. Either set this parameter to NONE (the default) or
--     don\'t use it to leave the files uncompressed.
--
-- Shorthand syntax for these settings is as follows:
-- @ServiceAccessRoleArn=string ,BucketName=string,CompressionType=string@
--
-- JSON syntax for these settings is as follows:
-- @{ \"ServiceAccessRoleArn\": \"string\", \"BucketName\": \"string\", \"CompressionType\": \"none\"|\"gzip\" } @
modifyEndpoint_dmsTransferSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe DmsTransferSettings)
modifyEndpoint_dmsTransferSettings = Lens.lens (\ModifyEndpoint' {dmsTransferSettings} -> dmsTransferSettings) (\s@ModifyEndpoint' {} a -> s {dmsTransferSettings = a} :: ModifyEndpoint)

-- | The port used by the endpoint database.
modifyEndpoint_port :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Int)
modifyEndpoint_port = Lens.lens (\ModifyEndpoint' {port} -> port) (\s@ModifyEndpoint' {} a -> s {port = a} :: ModifyEndpoint)

-- | Undocumented member.
modifyEndpoint_redshiftSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe RedshiftSettings)
modifyEndpoint_redshiftSettings = Lens.lens (\ModifyEndpoint' {redshiftSettings} -> redshiftSettings) (\s@ModifyEndpoint' {} a -> s {redshiftSettings = a} :: ModifyEndpoint)

-- | The user name to be used to login to the endpoint database.
modifyEndpoint_username :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_username = Lens.lens (\ModifyEndpoint' {username} -> username) (\s@ModifyEndpoint' {} a -> s {username = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Apache Kafka endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_kafkaSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe KafkaSettings)
modifyEndpoint_kafkaSettings = Lens.lens (\ModifyEndpoint' {kafkaSettings} -> kafkaSettings) (\s@ModifyEndpoint' {} a -> s {kafkaSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source DocumentDB endpoint. For more
-- information about the available settings, see the configuration
-- properties section in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_docDbSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe DocDbSettings)
modifyEndpoint_docDbSettings = Lens.lens (\ModifyEndpoint' {docDbSettings} -> docDbSettings) (\s@ModifyEndpoint' {} a -> s {docDbSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_dynamoDbSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe DynamoDbSettings)
modifyEndpoint_dynamoDbSettings = Lens.lens (\ModifyEndpoint' {dynamoDbSettings} -> dynamoDbSettings) (\s@ModifyEndpoint' {} a -> s {dynamoDbSettings = a} :: ModifyEndpoint)

-- | Additional attributes associated with the connection. To reset this
-- parameter, pass the empty string (\"\") as an argument.
modifyEndpoint_extraConnectionAttributes :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_extraConnectionAttributes = Lens.lens (\ModifyEndpoint' {extraConnectionAttributes} -> extraConnectionAttributes) (\s@ModifyEndpoint' {} a -> s {extraConnectionAttributes = a} :: ModifyEndpoint)

-- | The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
modifyEndpoint_endpointIdentifier :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_endpointIdentifier = Lens.lens (\ModifyEndpoint' {endpointIdentifier} -> endpointIdentifier) (\s@ModifyEndpoint' {} a -> s {endpointIdentifier = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data
-- Streams. For more information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_kinesisSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe KinesisSettings)
modifyEndpoint_kinesisSettings = Lens.lens (\ModifyEndpoint' {kinesisSettings} -> kinesisSettings) (\s@ModifyEndpoint' {} a -> s {kinesisSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target SAP ASE endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_sybaseSettings :: Lens.Lens' ModifyEndpoint (Prelude.Maybe SybaseSettings)
modifyEndpoint_sybaseSettings = Lens.lens (\ModifyEndpoint' {sybaseSettings} -> sybaseSettings) (\s@ModifyEndpoint' {} a -> s {sybaseSettings = a} :: ModifyEndpoint)

-- | The name of the endpoint database.
modifyEndpoint_databaseName :: Lens.Lens' ModifyEndpoint (Prelude.Maybe Prelude.Text)
modifyEndpoint_databaseName = Lens.lens (\ModifyEndpoint' {databaseName} -> databaseName) (\s@ModifyEndpoint' {} a -> s {databaseName = a} :: ModifyEndpoint)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
modifyEndpoint_endpointArn :: Lens.Lens' ModifyEndpoint Prelude.Text
modifyEndpoint_endpointArn = Lens.lens (\ModifyEndpoint' {endpointArn} -> endpointArn) (\s@ModifyEndpoint' {} a -> s {endpointArn = a} :: ModifyEndpoint)

instance Core.AWSRequest ModifyEndpoint where
  type
    AWSResponse ModifyEndpoint =
      ModifyEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyEndpointResponse'
            Prelude.<$> (x Core..?> "Endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyEndpoint

instance Prelude.NFData ModifyEndpoint

instance Core.ToHeaders ModifyEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.ModifyEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ModifyEndpoint where
  toJSON ModifyEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SslMode" Core..=) Prelude.<$> sslMode,
            ("MongoDbSettings" Core..=)
              Prelude.<$> mongoDbSettings,
            ("NeptuneSettings" Core..=)
              Prelude.<$> neptuneSettings,
            ("EngineName" Core..=) Prelude.<$> engineName,
            ("ElasticsearchSettings" Core..=)
              Prelude.<$> elasticsearchSettings,
            ("ExternalTableDefinition" Core..=)
              Prelude.<$> externalTableDefinition,
            ("EndpointType" Core..=) Prelude.<$> endpointType,
            ("OracleSettings" Core..=)
              Prelude.<$> oracleSettings,
            ("PostgreSQLSettings" Core..=)
              Prelude.<$> postgreSQLSettings,
            ("ServiceAccessRoleArn" Core..=)
              Prelude.<$> serviceAccessRoleArn,
            ("CertificateArn" Core..=)
              Prelude.<$> certificateArn,
            ("S3Settings" Core..=) Prelude.<$> s3Settings,
            ("ServerName" Core..=) Prelude.<$> serverName,
            ("MicrosoftSQLServerSettings" Core..=)
              Prelude.<$> microsoftSQLServerSettings,
            ("IBMDb2Settings" Core..=)
              Prelude.<$> iBMDb2Settings,
            ("MySQLSettings" Core..=) Prelude.<$> mySQLSettings,
            ("Password" Core..=) Prelude.<$> password,
            ("DmsTransferSettings" Core..=)
              Prelude.<$> dmsTransferSettings,
            ("Port" Core..=) Prelude.<$> port,
            ("RedshiftSettings" Core..=)
              Prelude.<$> redshiftSettings,
            ("Username" Core..=) Prelude.<$> username,
            ("KafkaSettings" Core..=) Prelude.<$> kafkaSettings,
            ("DocDbSettings" Core..=) Prelude.<$> docDbSettings,
            ("DynamoDbSettings" Core..=)
              Prelude.<$> dynamoDbSettings,
            ("ExtraConnectionAttributes" Core..=)
              Prelude.<$> extraConnectionAttributes,
            ("EndpointIdentifier" Core..=)
              Prelude.<$> endpointIdentifier,
            ("KinesisSettings" Core..=)
              Prelude.<$> kinesisSettings,
            ("SybaseSettings" Core..=)
              Prelude.<$> sybaseSettings,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            Prelude.Just ("EndpointArn" Core..= endpointArn)
          ]
      )

instance Core.ToPath ModifyEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newModifyEndpointResponse' smart constructor.
data ModifyEndpointResponse = ModifyEndpointResponse'
  { -- | The modified endpoint.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'modifyEndpointResponse_endpoint' - The modified endpoint.
--
-- 'httpStatus', 'modifyEndpointResponse_httpStatus' - The response's http status code.
newModifyEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyEndpointResponse
newModifyEndpointResponse pHttpStatus_ =
  ModifyEndpointResponse'
    { endpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The modified endpoint.
modifyEndpointResponse_endpoint :: Lens.Lens' ModifyEndpointResponse (Prelude.Maybe Endpoint)
modifyEndpointResponse_endpoint = Lens.lens (\ModifyEndpointResponse' {endpoint} -> endpoint) (\s@ModifyEndpointResponse' {} a -> s {endpoint = a} :: ModifyEndpointResponse)

-- | The response's http status code.
modifyEndpointResponse_httpStatus :: Lens.Lens' ModifyEndpointResponse Prelude.Int
modifyEndpointResponse_httpStatus = Lens.lens (\ModifyEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyEndpointResponse' {} a -> s {httpStatus = a} :: ModifyEndpointResponse)

instance Prelude.NFData ModifyEndpointResponse
