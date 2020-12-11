{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified endpoint.
module Network.AWS.DMS.ModifyEndpoint
  ( -- * Creating a request
    ModifyEndpoint (..),
    mkModifyEndpoint,

    -- ** Request lenses
    meDmsTransferSettings,
    meMySQLSettings,
    meServerName,
    meMicrosoftSQLServerSettings,
    meCertificateARN,
    meServiceAccessRoleARN,
    meDocDBSettings,
    mePostgreSQLSettings,
    meExtraConnectionAttributes,
    meKafkaSettings,
    meOracleSettings,
    meEndpointType,
    meRedshiftSettings,
    meElasticsearchSettings,
    meUsername,
    meExternalTableDefinition,
    meEngineName,
    meNeptuneSettings,
    meIBMDB2Settings,
    meMongoDBSettings,
    meSSLMode,
    mePassword,
    meSybaseSettings,
    meDatabaseName,
    meS3Settings,
    meKinesisSettings,
    meEndpointIdentifier,
    meDynamoDBSettings,
    mePort,
    meEndpointARN,

    -- * Destructuring the response
    ModifyEndpointResponse (..),
    mkModifyEndpointResponse,

    -- ** Response lenses
    mersEndpoint,
    mersResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyEndpoint' smart constructor.
data ModifyEndpoint = ModifyEndpoint'
  { dmsTransferSettings ::
      Lude.Maybe DmsTransferSettings,
    mySQLSettings :: Lude.Maybe MySQLSettings,
    serverName :: Lude.Maybe Lude.Text,
    microsoftSQLServerSettings ::
      Lude.Maybe MicrosoftSQLServerSettings,
    certificateARN :: Lude.Maybe Lude.Text,
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    docDBSettings :: Lude.Maybe DocDBSettings,
    postgreSQLSettings :: Lude.Maybe PostgreSQLSettings,
    extraConnectionAttributes :: Lude.Maybe Lude.Text,
    kafkaSettings :: Lude.Maybe KafkaSettings,
    oracleSettings :: Lude.Maybe OracleSettings,
    endpointType :: Lude.Maybe ReplicationEndpointTypeValue,
    redshiftSettings :: Lude.Maybe RedshiftSettings,
    elasticsearchSettings :: Lude.Maybe ElasticsearchSettings,
    username :: Lude.Maybe Lude.Text,
    externalTableDefinition :: Lude.Maybe Lude.Text,
    engineName :: Lude.Maybe Lude.Text,
    neptuneSettings :: Lude.Maybe NeptuneSettings,
    iBMDB2Settings :: Lude.Maybe IBMDB2Settings,
    mongoDBSettings :: Lude.Maybe MongoDBSettings,
    sslMode :: Lude.Maybe DmsSSLModeValue,
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sybaseSettings :: Lude.Maybe SybaseSettings,
    databaseName :: Lude.Maybe Lude.Text,
    s3Settings :: Lude.Maybe S3Settings,
    kinesisSettings :: Lude.Maybe KinesisSettings,
    endpointIdentifier :: Lude.Maybe Lude.Text,
    dynamoDBSettings :: Lude.Maybe DynamoDBSettings,
    port :: Lude.Maybe Lude.Int,
    endpointARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyEndpoint' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the certificate used for SSL connection.
-- * 'databaseName' - The name of the endpoint database.
-- * 'dmsTransferSettings' - The settings in JSON format for the DMS transfer type of source endpoint.
--
-- Attributes include the following:
--
--     * serviceAccessRoleArn - The AWS Identity and Access Management (IAM) role that has permission to access the Amazon S3 bucket.
--
--
--     * BucketName - The name of the S3 bucket to use.
--
--
--     * compressionType - An optional parameter to use GZIP to compress the target files. Either set this parameter to NONE (the default) or don't use it to leave the files uncompressed.
--
--
-- Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string ,BucketName=string,CompressionType=string@
-- JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @
-- * 'docDBSettings' - Settings in JSON format for the source DocumentDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
-- * 'dynamoDBSettings' - Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
-- * 'elasticsearchSettings' - Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
-- * 'endpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
-- * 'endpointType' - The type of endpoint. Valid values are @source@ and @target@ .
-- * 'engineName' - The type of engine for the endpoint. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
-- * 'externalTableDefinition' - The external table definition.
-- * 'extraConnectionAttributes' - Additional attributes associated with the connection. To reset this parameter, pass the empty string ("") as an argument.
-- * 'iBMDB2Settings' - Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'kafkaSettings' - Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
-- * 'kinesisSettings' - Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
-- * 'microsoftSQLServerSettings' - Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'mongoDBSettings' - Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
-- * 'mySQLSettings' - Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'neptuneSettings' - Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
-- * 'oracleSettings' - Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'password' - The password to be used to login to the endpoint database.
-- * 'port' - The port used by the endpoint database.
-- * 'postgreSQLSettings' - Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'redshiftSettings' - Undocumented field.
-- * 's3Settings' - Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'serverName' - The name of the server where the endpoint database resides.
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) for the service access role you want to use to modify the endpoint.
-- * 'sslMode' - The SSL mode used to connect to the endpoint. The default value is @none@ .
-- * 'sybaseSettings' - Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'username' - The user name to be used to login to the endpoint database.
mkModifyEndpoint ::
  -- | 'endpointARN'
  Lude.Text ->
  ModifyEndpoint
mkModifyEndpoint pEndpointARN_ =
  ModifyEndpoint'
    { dmsTransferSettings = Lude.Nothing,
      mySQLSettings = Lude.Nothing,
      serverName = Lude.Nothing,
      microsoftSQLServerSettings = Lude.Nothing,
      certificateARN = Lude.Nothing,
      serviceAccessRoleARN = Lude.Nothing,
      docDBSettings = Lude.Nothing,
      postgreSQLSettings = Lude.Nothing,
      extraConnectionAttributes = Lude.Nothing,
      kafkaSettings = Lude.Nothing,
      oracleSettings = Lude.Nothing,
      endpointType = Lude.Nothing,
      redshiftSettings = Lude.Nothing,
      elasticsearchSettings = Lude.Nothing,
      username = Lude.Nothing,
      externalTableDefinition = Lude.Nothing,
      engineName = Lude.Nothing,
      neptuneSettings = Lude.Nothing,
      iBMDB2Settings = Lude.Nothing,
      mongoDBSettings = Lude.Nothing,
      sslMode = Lude.Nothing,
      password = Lude.Nothing,
      sybaseSettings = Lude.Nothing,
      databaseName = Lude.Nothing,
      s3Settings = Lude.Nothing,
      kinesisSettings = Lude.Nothing,
      endpointIdentifier = Lude.Nothing,
      dynamoDBSettings = Lude.Nothing,
      port = Lude.Nothing,
      endpointARN = pEndpointARN_
    }

-- | The settings in JSON format for the DMS transfer type of source endpoint.
--
-- Attributes include the following:
--
--     * serviceAccessRoleArn - The AWS Identity and Access Management (IAM) role that has permission to access the Amazon S3 bucket.
--
--
--     * BucketName - The name of the S3 bucket to use.
--
--
--     * compressionType - An optional parameter to use GZIP to compress the target files. Either set this parameter to NONE (the default) or don't use it to leave the files uncompressed.
--
--
-- Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string ,BucketName=string,CompressionType=string@
-- JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @
--
-- /Note:/ Consider using 'dmsTransferSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meDmsTransferSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe DmsTransferSettings)
meDmsTransferSettings = Lens.lens (dmsTransferSettings :: ModifyEndpoint -> Lude.Maybe DmsTransferSettings) (\s a -> s {dmsTransferSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meDmsTransferSettings "Use generic-lens or generic-optics with 'dmsTransferSettings' instead." #-}

-- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'mySQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meMySQLSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe MySQLSettings)
meMySQLSettings = Lens.lens (mySQLSettings :: ModifyEndpoint -> Lude.Maybe MySQLSettings) (\s a -> s {mySQLSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meMySQLSettings "Use generic-lens or generic-optics with 'mySQLSettings' instead." #-}

-- | The name of the server where the endpoint database resides.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meServerName :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meServerName = Lens.lens (serverName :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: ModifyEndpoint)
{-# DEPRECATED meServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'microsoftSQLServerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meMicrosoftSQLServerSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe MicrosoftSQLServerSettings)
meMicrosoftSQLServerSettings = Lens.lens (microsoftSQLServerSettings :: ModifyEndpoint -> Lude.Maybe MicrosoftSQLServerSettings) (\s a -> s {microsoftSQLServerSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meMicrosoftSQLServerSettings "Use generic-lens or generic-optics with 'microsoftSQLServerSettings' instead." #-}

-- | The Amazon Resource Name (ARN) of the certificate used for SSL connection.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meCertificateARN :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meCertificateARN = Lens.lens (certificateARN :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: ModifyEndpoint)
{-# DEPRECATED meCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The Amazon Resource Name (ARN) for the service access role you want to use to modify the endpoint.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meServiceAccessRoleARN :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: ModifyEndpoint)
{-# DEPRECATED meServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | Settings in JSON format for the source DocumentDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'docDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meDocDBSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe DocDBSettings)
meDocDBSettings = Lens.lens (docDBSettings :: ModifyEndpoint -> Lude.Maybe DocDBSettings) (\s a -> s {docDBSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meDocDBSettings "Use generic-lens or generic-optics with 'docDBSettings' instead." #-}

-- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'postgreSQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mePostgreSQLSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe PostgreSQLSettings)
mePostgreSQLSettings = Lens.lens (postgreSQLSettings :: ModifyEndpoint -> Lude.Maybe PostgreSQLSettings) (\s a -> s {postgreSQLSettings = a} :: ModifyEndpoint)
{-# DEPRECATED mePostgreSQLSettings "Use generic-lens or generic-optics with 'postgreSQLSettings' instead." #-}

-- | Additional attributes associated with the connection. To reset this parameter, pass the empty string ("") as an argument.
--
-- /Note:/ Consider using 'extraConnectionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meExtraConnectionAttributes :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meExtraConnectionAttributes = Lens.lens (extraConnectionAttributes :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {extraConnectionAttributes = a} :: ModifyEndpoint)
{-# DEPRECATED meExtraConnectionAttributes "Use generic-lens or generic-optics with 'extraConnectionAttributes' instead." #-}

-- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'kafkaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meKafkaSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe KafkaSettings)
meKafkaSettings = Lens.lens (kafkaSettings :: ModifyEndpoint -> Lude.Maybe KafkaSettings) (\s a -> s {kafkaSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meKafkaSettings "Use generic-lens or generic-optics with 'kafkaSettings' instead." #-}

-- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'oracleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meOracleSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe OracleSettings)
meOracleSettings = Lens.lens (oracleSettings :: ModifyEndpoint -> Lude.Maybe OracleSettings) (\s a -> s {oracleSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meOracleSettings "Use generic-lens or generic-optics with 'oracleSettings' instead." #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEndpointType :: Lens.Lens' ModifyEndpoint (Lude.Maybe ReplicationEndpointTypeValue)
meEndpointType = Lens.lens (endpointType :: ModifyEndpoint -> Lude.Maybe ReplicationEndpointTypeValue) (\s a -> s {endpointType = a} :: ModifyEndpoint)
{-# DEPRECATED meEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meRedshiftSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe RedshiftSettings)
meRedshiftSettings = Lens.lens (redshiftSettings :: ModifyEndpoint -> Lude.Maybe RedshiftSettings) (\s a -> s {redshiftSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meRedshiftSettings "Use generic-lens or generic-optics with 'redshiftSettings' instead." #-}

-- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'elasticsearchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meElasticsearchSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe ElasticsearchSettings)
meElasticsearchSettings = Lens.lens (elasticsearchSettings :: ModifyEndpoint -> Lude.Maybe ElasticsearchSettings) (\s a -> s {elasticsearchSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meElasticsearchSettings "Use generic-lens or generic-optics with 'elasticsearchSettings' instead." #-}

-- | The user name to be used to login to the endpoint database.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meUsername :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meUsername = Lens.lens (username :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: ModifyEndpoint)
{-# DEPRECATED meUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The external table definition.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meExternalTableDefinition :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meExternalTableDefinition = Lens.lens (externalTableDefinition :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {externalTableDefinition = a} :: ModifyEndpoint)
{-# DEPRECATED meExternalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead." #-}

-- | The type of engine for the endpoint. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEngineName :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meEngineName = Lens.lens (engineName :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {engineName = a} :: ModifyEndpoint)
{-# DEPRECATED meEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'neptuneSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meNeptuneSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe NeptuneSettings)
meNeptuneSettings = Lens.lens (neptuneSettings :: ModifyEndpoint -> Lude.Maybe NeptuneSettings) (\s a -> s {neptuneSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meNeptuneSettings "Use generic-lens or generic-optics with 'neptuneSettings' instead." #-}

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'iBMDB2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meIBMDB2Settings :: Lens.Lens' ModifyEndpoint (Lude.Maybe IBMDB2Settings)
meIBMDB2Settings = Lens.lens (iBMDB2Settings :: ModifyEndpoint -> Lude.Maybe IBMDB2Settings) (\s a -> s {iBMDB2Settings = a} :: ModifyEndpoint)
{-# DEPRECATED meIBMDB2Settings "Use generic-lens or generic-optics with 'iBMDB2Settings' instead." #-}

-- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'mongoDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meMongoDBSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe MongoDBSettings)
meMongoDBSettings = Lens.lens (mongoDBSettings :: ModifyEndpoint -> Lude.Maybe MongoDBSettings) (\s a -> s {mongoDBSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meMongoDBSettings "Use generic-lens or generic-optics with 'mongoDBSettings' instead." #-}

-- | The SSL mode used to connect to the endpoint. The default value is @none@ .
--
-- /Note:/ Consider using 'sslMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSSLMode :: Lens.Lens' ModifyEndpoint (Lude.Maybe DmsSSLModeValue)
meSSLMode = Lens.lens (sslMode :: ModifyEndpoint -> Lude.Maybe DmsSSLModeValue) (\s a -> s {sslMode = a} :: ModifyEndpoint)
{-# DEPRECATED meSSLMode "Use generic-lens or generic-optics with 'sslMode' instead." #-}

-- | The password to be used to login to the endpoint database.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mePassword :: Lens.Lens' ModifyEndpoint (Lude.Maybe (Lude.Sensitive Lude.Text))
mePassword = Lens.lens (password :: ModifyEndpoint -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: ModifyEndpoint)
{-# DEPRECATED mePassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'sybaseSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSybaseSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe SybaseSettings)
meSybaseSettings = Lens.lens (sybaseSettings :: ModifyEndpoint -> Lude.Maybe SybaseSettings) (\s a -> s {sybaseSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meSybaseSettings "Use generic-lens or generic-optics with 'sybaseSettings' instead." #-}

-- | The name of the endpoint database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meDatabaseName :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meDatabaseName = Lens.lens (databaseName :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: ModifyEndpoint)
{-# DEPRECATED meDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meS3Settings :: Lens.Lens' ModifyEndpoint (Lude.Maybe S3Settings)
meS3Settings = Lens.lens (s3Settings :: ModifyEndpoint -> Lude.Maybe S3Settings) (\s a -> s {s3Settings = a} :: ModifyEndpoint)
{-# DEPRECATED meS3Settings "Use generic-lens or generic-optics with 's3Settings' instead." #-}

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'kinesisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meKinesisSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe KinesisSettings)
meKinesisSettings = Lens.lens (kinesisSettings :: ModifyEndpoint -> Lude.Maybe KinesisSettings) (\s a -> s {kinesisSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meKinesisSettings "Use generic-lens or generic-optics with 'kinesisSettings' instead." #-}

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEndpointIdentifier :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Text)
meEndpointIdentifier = Lens.lens (endpointIdentifier :: ModifyEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointIdentifier = a} :: ModifyEndpoint)
{-# DEPRECATED meEndpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead." #-}

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'dynamoDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meDynamoDBSettings :: Lens.Lens' ModifyEndpoint (Lude.Maybe DynamoDBSettings)
meDynamoDBSettings = Lens.lens (dynamoDBSettings :: ModifyEndpoint -> Lude.Maybe DynamoDBSettings) (\s a -> s {dynamoDBSettings = a} :: ModifyEndpoint)
{-# DEPRECATED meDynamoDBSettings "Use generic-lens or generic-optics with 'dynamoDBSettings' instead." #-}

-- | The port used by the endpoint database.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mePort :: Lens.Lens' ModifyEndpoint (Lude.Maybe Lude.Int)
mePort = Lens.lens (port :: ModifyEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: ModifyEndpoint)
{-# DEPRECATED mePort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEndpointARN :: Lens.Lens' ModifyEndpoint Lude.Text
meEndpointARN = Lens.lens (endpointARN :: ModifyEndpoint -> Lude.Text) (\s a -> s {endpointARN = a} :: ModifyEndpoint)
{-# DEPRECATED meEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest ModifyEndpoint where
  type Rs ModifyEndpoint = ModifyEndpointResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyEndpointResponse'
            Lude.<$> (x Lude..?> "Endpoint") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.ModifyEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyEndpoint where
  toJSON ModifyEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DmsTransferSettings" Lude..=) Lude.<$> dmsTransferSettings,
            ("MySQLSettings" Lude..=) Lude.<$> mySQLSettings,
            ("ServerName" Lude..=) Lude.<$> serverName,
            ("MicrosoftSQLServerSettings" Lude..=)
              Lude.<$> microsoftSQLServerSettings,
            ("CertificateArn" Lude..=) Lude.<$> certificateARN,
            ("ServiceAccessRoleArn" Lude..=) Lude.<$> serviceAccessRoleARN,
            ("DocDbSettings" Lude..=) Lude.<$> docDBSettings,
            ("PostgreSQLSettings" Lude..=) Lude.<$> postgreSQLSettings,
            ("ExtraConnectionAttributes" Lude..=)
              Lude.<$> extraConnectionAttributes,
            ("KafkaSettings" Lude..=) Lude.<$> kafkaSettings,
            ("OracleSettings" Lude..=) Lude.<$> oracleSettings,
            ("EndpointType" Lude..=) Lude.<$> endpointType,
            ("RedshiftSettings" Lude..=) Lude.<$> redshiftSettings,
            ("ElasticsearchSettings" Lude..=) Lude.<$> elasticsearchSettings,
            ("Username" Lude..=) Lude.<$> username,
            ("ExternalTableDefinition" Lude..=)
              Lude.<$> externalTableDefinition,
            ("EngineName" Lude..=) Lude.<$> engineName,
            ("NeptuneSettings" Lude..=) Lude.<$> neptuneSettings,
            ("IBMDb2Settings" Lude..=) Lude.<$> iBMDB2Settings,
            ("MongoDbSettings" Lude..=) Lude.<$> mongoDBSettings,
            ("SslMode" Lude..=) Lude.<$> sslMode,
            ("Password" Lude..=) Lude.<$> password,
            ("SybaseSettings" Lude..=) Lude.<$> sybaseSettings,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("S3Settings" Lude..=) Lude.<$> s3Settings,
            ("KinesisSettings" Lude..=) Lude.<$> kinesisSettings,
            ("EndpointIdentifier" Lude..=) Lude.<$> endpointIdentifier,
            ("DynamoDbSettings" Lude..=) Lude.<$> dynamoDBSettings,
            ("Port" Lude..=) Lude.<$> port,
            Lude.Just ("EndpointArn" Lude..= endpointARN)
          ]
      )

instance Lude.ToPath ModifyEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyEndpoint where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkModifyEndpointResponse' smart constructor.
data ModifyEndpointResponse = ModifyEndpointResponse'
  { endpoint ::
      Lude.Maybe Endpoint,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpoint' - The modified endpoint.
-- * 'responseStatus' - The response status code.
mkModifyEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyEndpointResponse
mkModifyEndpointResponse pResponseStatus_ =
  ModifyEndpointResponse'
    { endpoint = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The modified endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mersEndpoint :: Lens.Lens' ModifyEndpointResponse (Lude.Maybe Endpoint)
mersEndpoint = Lens.lens (endpoint :: ModifyEndpointResponse -> Lude.Maybe Endpoint) (\s a -> s {endpoint = a} :: ModifyEndpointResponse)
{-# DEPRECATED mersEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mersResponseStatus :: Lens.Lens' ModifyEndpointResponse Lude.Int
mersResponseStatus = Lens.lens (responseStatus :: ModifyEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyEndpointResponse)
{-# DEPRECATED mersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
