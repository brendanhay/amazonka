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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyEndpoint' smart constructor.
data ModifyEndpoint = ModifyEndpoint'
  { -- | The SSL mode used to connect to the endpoint. The default value is
    -- @none@.
    sslMode :: Core.Maybe DmsSslModeValue,
    -- | Settings in JSON format for the source MongoDB endpoint. For more
    -- information about the available settings, see the configuration
    -- properties section in
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    mongoDbSettings :: Core.Maybe MongoDbSettings,
    -- | Settings in JSON format for the target Amazon Neptune endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target>
    -- in the /AWS Database Migration Service User Guide./
    neptuneSettings :: Core.Maybe NeptuneSettings,
    -- | The type of engine for the endpoint. Valid values, depending on the
    -- EndpointType, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
    -- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@,
    -- @\"s3\"@, @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
    -- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
    -- @\"documentdb\"@, @\"sqlserver\"@, and @\"neptune\"@.
    engineName :: Core.Maybe Core.Text,
    -- | Settings in JSON format for the target Elasticsearch endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    elasticsearchSettings :: Core.Maybe ElasticsearchSettings,
    -- | The external table definition.
    externalTableDefinition :: Core.Maybe Core.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Core.Maybe ReplicationEndpointTypeValue,
    -- | Settings in JSON format for the source and target Oracle endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    oracleSettings :: Core.Maybe OracleSettings,
    -- | Settings in JSON format for the source and target PostgreSQL endpoint.
    -- For information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    postgreSQLSettings :: Core.Maybe PostgreSQLSettings,
    -- | The Amazon Resource Name (ARN) for the service access role you want to
    -- use to modify the endpoint.
    serviceAccessRoleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the certificate used for SSL
    -- connection.
    certificateArn :: Core.Maybe Core.Text,
    -- | Settings in JSON format for the target Amazon S3 endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    s3Settings :: Core.Maybe S3Settings,
    -- | The name of the server where the endpoint database resides.
    serverName :: Core.Maybe Core.Text,
    -- | Settings in JSON format for the source and target Microsoft SQL Server
    -- endpoint. For information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    microsoftSQLServerSettings :: Core.Maybe MicrosoftSQLServerSettings,
    -- | Settings in JSON format for the source IBM Db2 LUW endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    iBMDb2Settings :: Core.Maybe IBMDb2Settings,
    -- | Settings in JSON format for the source and target MySQL endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    mySQLSettings :: Core.Maybe MySQLSettings,
    -- | The password to be used to login to the endpoint database.
    password :: Core.Maybe (Core.Sensitive Core.Text),
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
    dmsTransferSettings :: Core.Maybe DmsTransferSettings,
    -- | The port used by the endpoint database.
    port :: Core.Maybe Core.Int,
    redshiftSettings :: Core.Maybe RedshiftSettings,
    -- | The user name to be used to login to the endpoint database.
    username :: Core.Maybe Core.Text,
    -- | Settings in JSON format for the target Apache Kafka endpoint. For more
    -- information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    kafkaSettings :: Core.Maybe KafkaSettings,
    -- | Settings in JSON format for the source DocumentDB endpoint. For more
    -- information about the available settings, see the configuration
    -- properties section in
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    docDbSettings :: Core.Maybe DocDbSettings,
    -- | Settings in JSON format for the target Amazon DynamoDB endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB>
    -- in the /AWS Database Migration Service User Guide./
    dynamoDbSettings :: Core.Maybe DynamoDbSettings,
    -- | Additional attributes associated with the connection. To reset this
    -- parameter, pass the empty string (\"\") as an argument.
    extraConnectionAttributes :: Core.Maybe Core.Text,
    -- | The database endpoint identifier. Identifiers must begin with a letter
    -- and must contain only ASCII letters, digits, and hyphens. They can\'t
    -- end with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Core.Maybe Core.Text,
    -- | Settings in JSON format for the target endpoint for Amazon Kinesis Data
    -- Streams. For more information about the available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service>
    -- in the /AWS Database Migration Service User Guide./
    kinesisSettings :: Core.Maybe KinesisSettings,
    -- | Settings in JSON format for the source and target SAP ASE endpoint. For
    -- information about other available settings, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS>
    -- in the /AWS Database Migration Service User Guide./
    sybaseSettings :: Core.Maybe SybaseSettings,
    -- | The name of the endpoint database.
    databaseName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  ModifyEndpoint
newModifyEndpoint pEndpointArn_ =
  ModifyEndpoint'
    { sslMode = Core.Nothing,
      mongoDbSettings = Core.Nothing,
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
      iBMDb2Settings = Core.Nothing,
      mySQLSettings = Core.Nothing,
      password = Core.Nothing,
      dmsTransferSettings = Core.Nothing,
      port = Core.Nothing,
      redshiftSettings = Core.Nothing,
      username = Core.Nothing,
      kafkaSettings = Core.Nothing,
      docDbSettings = Core.Nothing,
      dynamoDbSettings = Core.Nothing,
      extraConnectionAttributes = Core.Nothing,
      endpointIdentifier = Core.Nothing,
      kinesisSettings = Core.Nothing,
      sybaseSettings = Core.Nothing,
      databaseName = Core.Nothing,
      endpointArn = pEndpointArn_
    }

-- | The SSL mode used to connect to the endpoint. The default value is
-- @none@.
modifyEndpoint_sslMode :: Lens.Lens' ModifyEndpoint (Core.Maybe DmsSslModeValue)
modifyEndpoint_sslMode = Lens.lens (\ModifyEndpoint' {sslMode} -> sslMode) (\s@ModifyEndpoint' {} a -> s {sslMode = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source MongoDB endpoint. For more
-- information about the available settings, see the configuration
-- properties section in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_mongoDbSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe MongoDbSettings)
modifyEndpoint_mongoDbSettings = Lens.lens (\ModifyEndpoint' {mongoDbSettings} -> mongoDbSettings) (\s@ModifyEndpoint' {} a -> s {mongoDbSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_neptuneSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe NeptuneSettings)
modifyEndpoint_neptuneSettings = Lens.lens (\ModifyEndpoint' {neptuneSettings} -> neptuneSettings) (\s@ModifyEndpoint' {} a -> s {neptuneSettings = a} :: ModifyEndpoint)

-- | The type of engine for the endpoint. Valid values, depending on the
-- EndpointType, include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@,
-- @\"mariadb\"@, @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@,
-- @\"s3\"@, @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
-- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
-- @\"documentdb\"@, @\"sqlserver\"@, and @\"neptune\"@.
modifyEndpoint_engineName :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_engineName = Lens.lens (\ModifyEndpoint' {engineName} -> engineName) (\s@ModifyEndpoint' {} a -> s {engineName = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Elasticsearch endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_elasticsearchSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe ElasticsearchSettings)
modifyEndpoint_elasticsearchSettings = Lens.lens (\ModifyEndpoint' {elasticsearchSettings} -> elasticsearchSettings) (\s@ModifyEndpoint' {} a -> s {elasticsearchSettings = a} :: ModifyEndpoint)

-- | The external table definition.
modifyEndpoint_externalTableDefinition :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_externalTableDefinition = Lens.lens (\ModifyEndpoint' {externalTableDefinition} -> externalTableDefinition) (\s@ModifyEndpoint' {} a -> s {externalTableDefinition = a} :: ModifyEndpoint)

-- | The type of endpoint. Valid values are @source@ and @target@.
modifyEndpoint_endpointType :: Lens.Lens' ModifyEndpoint (Core.Maybe ReplicationEndpointTypeValue)
modifyEndpoint_endpointType = Lens.lens (\ModifyEndpoint' {endpointType} -> endpointType) (\s@ModifyEndpoint' {} a -> s {endpointType = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target Oracle endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_oracleSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe OracleSettings)
modifyEndpoint_oracleSettings = Lens.lens (\ModifyEndpoint' {oracleSettings} -> oracleSettings) (\s@ModifyEndpoint' {} a -> s {oracleSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target PostgreSQL endpoint.
-- For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_postgreSQLSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe PostgreSQLSettings)
modifyEndpoint_postgreSQLSettings = Lens.lens (\ModifyEndpoint' {postgreSQLSettings} -> postgreSQLSettings) (\s@ModifyEndpoint' {} a -> s {postgreSQLSettings = a} :: ModifyEndpoint)

-- | The Amazon Resource Name (ARN) for the service access role you want to
-- use to modify the endpoint.
modifyEndpoint_serviceAccessRoleArn :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_serviceAccessRoleArn = Lens.lens (\ModifyEndpoint' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@ModifyEndpoint' {} a -> s {serviceAccessRoleArn = a} :: ModifyEndpoint)

-- | The Amazon Resource Name (ARN) of the certificate used for SSL
-- connection.
modifyEndpoint_certificateArn :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_certificateArn = Lens.lens (\ModifyEndpoint' {certificateArn} -> certificateArn) (\s@ModifyEndpoint' {} a -> s {certificateArn = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Amazon S3 endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_s3Settings :: Lens.Lens' ModifyEndpoint (Core.Maybe S3Settings)
modifyEndpoint_s3Settings = Lens.lens (\ModifyEndpoint' {s3Settings} -> s3Settings) (\s@ModifyEndpoint' {} a -> s {s3Settings = a} :: ModifyEndpoint)

-- | The name of the server where the endpoint database resides.
modifyEndpoint_serverName :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_serverName = Lens.lens (\ModifyEndpoint' {serverName} -> serverName) (\s@ModifyEndpoint' {} a -> s {serverName = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target Microsoft SQL Server
-- endpoint. For information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_microsoftSQLServerSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe MicrosoftSQLServerSettings)
modifyEndpoint_microsoftSQLServerSettings = Lens.lens (\ModifyEndpoint' {microsoftSQLServerSettings} -> microsoftSQLServerSettings) (\s@ModifyEndpoint' {} a -> s {microsoftSQLServerSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_iBMDb2Settings :: Lens.Lens' ModifyEndpoint (Core.Maybe IBMDb2Settings)
modifyEndpoint_iBMDb2Settings = Lens.lens (\ModifyEndpoint' {iBMDb2Settings} -> iBMDb2Settings) (\s@ModifyEndpoint' {} a -> s {iBMDb2Settings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target MySQL endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_mySQLSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe MySQLSettings)
modifyEndpoint_mySQLSettings = Lens.lens (\ModifyEndpoint' {mySQLSettings} -> mySQLSettings) (\s@ModifyEndpoint' {} a -> s {mySQLSettings = a} :: ModifyEndpoint)

-- | The password to be used to login to the endpoint database.
modifyEndpoint_password :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_password = Lens.lens (\ModifyEndpoint' {password} -> password) (\s@ModifyEndpoint' {} a -> s {password = a} :: ModifyEndpoint) Core.. Lens.mapping Core._Sensitive

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
modifyEndpoint_dmsTransferSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe DmsTransferSettings)
modifyEndpoint_dmsTransferSettings = Lens.lens (\ModifyEndpoint' {dmsTransferSettings} -> dmsTransferSettings) (\s@ModifyEndpoint' {} a -> s {dmsTransferSettings = a} :: ModifyEndpoint)

-- | The port used by the endpoint database.
modifyEndpoint_port :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Int)
modifyEndpoint_port = Lens.lens (\ModifyEndpoint' {port} -> port) (\s@ModifyEndpoint' {} a -> s {port = a} :: ModifyEndpoint)

-- | Undocumented member.
modifyEndpoint_redshiftSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe RedshiftSettings)
modifyEndpoint_redshiftSettings = Lens.lens (\ModifyEndpoint' {redshiftSettings} -> redshiftSettings) (\s@ModifyEndpoint' {} a -> s {redshiftSettings = a} :: ModifyEndpoint)

-- | The user name to be used to login to the endpoint database.
modifyEndpoint_username :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_username = Lens.lens (\ModifyEndpoint' {username} -> username) (\s@ModifyEndpoint' {} a -> s {username = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Apache Kafka endpoint. For more
-- information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_kafkaSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe KafkaSettings)
modifyEndpoint_kafkaSettings = Lens.lens (\ModifyEndpoint' {kafkaSettings} -> kafkaSettings) (\s@ModifyEndpoint' {} a -> s {kafkaSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source DocumentDB endpoint. For more
-- information about the available settings, see the configuration
-- properties section in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_docDbSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe DocDbSettings)
modifyEndpoint_docDbSettings = Lens.lens (\ModifyEndpoint' {docDbSettings} -> docDbSettings) (\s@ModifyEndpoint' {} a -> s {docDbSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_dynamoDbSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe DynamoDbSettings)
modifyEndpoint_dynamoDbSettings = Lens.lens (\ModifyEndpoint' {dynamoDbSettings} -> dynamoDbSettings) (\s@ModifyEndpoint' {} a -> s {dynamoDbSettings = a} :: ModifyEndpoint)

-- | Additional attributes associated with the connection. To reset this
-- parameter, pass the empty string (\"\") as an argument.
modifyEndpoint_extraConnectionAttributes :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_extraConnectionAttributes = Lens.lens (\ModifyEndpoint' {extraConnectionAttributes} -> extraConnectionAttributes) (\s@ModifyEndpoint' {} a -> s {extraConnectionAttributes = a} :: ModifyEndpoint)

-- | The database endpoint identifier. Identifiers must begin with a letter
-- and must contain only ASCII letters, digits, and hyphens. They can\'t
-- end with a hyphen or contain two consecutive hyphens.
modifyEndpoint_endpointIdentifier :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_endpointIdentifier = Lens.lens (\ModifyEndpoint' {endpointIdentifier} -> endpointIdentifier) (\s@ModifyEndpoint' {} a -> s {endpointIdentifier = a} :: ModifyEndpoint)

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data
-- Streams. For more information about the available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_kinesisSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe KinesisSettings)
modifyEndpoint_kinesisSettings = Lens.lens (\ModifyEndpoint' {kinesisSettings} -> kinesisSettings) (\s@ModifyEndpoint' {} a -> s {kinesisSettings = a} :: ModifyEndpoint)

-- | Settings in JSON format for the source and target SAP ASE endpoint. For
-- information about other available settings, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS>
-- and
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS>
-- in the /AWS Database Migration Service User Guide./
modifyEndpoint_sybaseSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe SybaseSettings)
modifyEndpoint_sybaseSettings = Lens.lens (\ModifyEndpoint' {sybaseSettings} -> sybaseSettings) (\s@ModifyEndpoint' {} a -> s {sybaseSettings = a} :: ModifyEndpoint)

-- | The name of the endpoint database.
modifyEndpoint_databaseName :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
modifyEndpoint_databaseName = Lens.lens (\ModifyEndpoint' {databaseName} -> databaseName) (\s@ModifyEndpoint' {} a -> s {databaseName = a} :: ModifyEndpoint)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
modifyEndpoint_endpointArn :: Lens.Lens' ModifyEndpoint Core.Text
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
            Core.<$> (x Core..?> "Endpoint")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyEndpoint

instance Core.NFData ModifyEndpoint

instance Core.ToHeaders ModifyEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.ModifyEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyEndpoint where
  toJSON ModifyEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SslMode" Core..=) Core.<$> sslMode,
            ("MongoDbSettings" Core..=) Core.<$> mongoDbSettings,
            ("NeptuneSettings" Core..=) Core.<$> neptuneSettings,
            ("EngineName" Core..=) Core.<$> engineName,
            ("ElasticsearchSettings" Core..=)
              Core.<$> elasticsearchSettings,
            ("ExternalTableDefinition" Core..=)
              Core.<$> externalTableDefinition,
            ("EndpointType" Core..=) Core.<$> endpointType,
            ("OracleSettings" Core..=) Core.<$> oracleSettings,
            ("PostgreSQLSettings" Core..=)
              Core.<$> postgreSQLSettings,
            ("ServiceAccessRoleArn" Core..=)
              Core.<$> serviceAccessRoleArn,
            ("CertificateArn" Core..=) Core.<$> certificateArn,
            ("S3Settings" Core..=) Core.<$> s3Settings,
            ("ServerName" Core..=) Core.<$> serverName,
            ("MicrosoftSQLServerSettings" Core..=)
              Core.<$> microsoftSQLServerSettings,
            ("IBMDb2Settings" Core..=) Core.<$> iBMDb2Settings,
            ("MySQLSettings" Core..=) Core.<$> mySQLSettings,
            ("Password" Core..=) Core.<$> password,
            ("DmsTransferSettings" Core..=)
              Core.<$> dmsTransferSettings,
            ("Port" Core..=) Core.<$> port,
            ("RedshiftSettings" Core..=)
              Core.<$> redshiftSettings,
            ("Username" Core..=) Core.<$> username,
            ("KafkaSettings" Core..=) Core.<$> kafkaSettings,
            ("DocDbSettings" Core..=) Core.<$> docDbSettings,
            ("DynamoDbSettings" Core..=)
              Core.<$> dynamoDbSettings,
            ("ExtraConnectionAttributes" Core..=)
              Core.<$> extraConnectionAttributes,
            ("EndpointIdentifier" Core..=)
              Core.<$> endpointIdentifier,
            ("KinesisSettings" Core..=) Core.<$> kinesisSettings,
            ("SybaseSettings" Core..=) Core.<$> sybaseSettings,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            Core.Just ("EndpointArn" Core..= endpointArn)
          ]
      )

instance Core.ToPath ModifyEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery ModifyEndpoint where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newModifyEndpointResponse' smart constructor.
data ModifyEndpointResponse = ModifyEndpointResponse'
  { -- | The modified endpoint.
    endpoint :: Core.Maybe Endpoint,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyEndpointResponse
newModifyEndpointResponse pHttpStatus_ =
  ModifyEndpointResponse'
    { endpoint = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The modified endpoint.
modifyEndpointResponse_endpoint :: Lens.Lens' ModifyEndpointResponse (Core.Maybe Endpoint)
modifyEndpointResponse_endpoint = Lens.lens (\ModifyEndpointResponse' {endpoint} -> endpoint) (\s@ModifyEndpointResponse' {} a -> s {endpoint = a} :: ModifyEndpointResponse)

-- | The response's http status code.
modifyEndpointResponse_httpStatus :: Lens.Lens' ModifyEndpointResponse Core.Int
modifyEndpointResponse_httpStatus = Lens.lens (\ModifyEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyEndpointResponse' {} a -> s {httpStatus = a} :: ModifyEndpointResponse)

instance Core.NFData ModifyEndpointResponse
