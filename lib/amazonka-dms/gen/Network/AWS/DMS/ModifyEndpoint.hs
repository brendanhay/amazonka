{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyEndpoint (..)
    , mkModifyEndpoint
    -- ** Request lenses
    , meEndpointArn
    , meCertificateArn
    , meDatabaseName
    , meDmsTransferSettings
    , meDocDbSettings
    , meDynamoDbSettings
    , meElasticsearchSettings
    , meEndpointIdentifier
    , meEndpointType
    , meEngineName
    , meExternalTableDefinition
    , meExtraConnectionAttributes
    , meIBMDb2Settings
    , meKafkaSettings
    , meKinesisSettings
    , meMicrosoftSQLServerSettings
    , meMongoDbSettings
    , meMySQLSettings
    , meNeptuneSettings
    , meOracleSettings
    , mePassword
    , mePort
    , mePostgreSQLSettings
    , meRedshiftSettings
    , meS3Settings
    , meServerName
    , meServiceAccessRoleArn
    , meSslMode
    , meSybaseSettings
    , meUsername

    -- * Destructuring the response
    , ModifyEndpointResponse (..)
    , mkModifyEndpointResponse
    -- ** Response lenses
    , merrsEndpoint
    , merrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyEndpoint' smart constructor.
data ModifyEndpoint = ModifyEndpoint'
  { endpointArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
  , certificateArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the certificate used for SSL connection.
  , databaseName :: Core.Maybe Core.Text
    -- ^ The name of the endpoint database.
  , dmsTransferSettings :: Core.Maybe Types.DmsTransferSettings
    -- ^ The settings in JSON format for the DMS transfer type of source endpoint. 
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
  , docDbSettings :: Core.Maybe Types.DocDbSettings
    -- ^ Settings in JSON format for the source DocumentDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
  , dynamoDbSettings :: Core.Maybe Types.DynamoDbSettings
    -- ^ Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./ 
  , elasticsearchSettings :: Core.Maybe Types.ElasticsearchSettings
    -- ^ Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , endpointIdentifier :: Core.Maybe Core.Text
    -- ^ The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
  , endpointType :: Core.Maybe Types.ReplicationEndpointTypeValue
    -- ^ The type of endpoint. Valid values are @source@ and @target@ .
  , engineName :: Core.Maybe Core.Text
    -- ^ The type of engine for the endpoint. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
  , externalTableDefinition :: Core.Maybe Core.Text
    -- ^ The external table definition.
  , extraConnectionAttributes :: Core.Maybe Core.Text
    -- ^ Additional attributes associated with the connection. To reset this parameter, pass the empty string ("") as an argument.
  , iBMDb2Settings :: Core.Maybe Types.IBMDb2Settings
    -- ^ Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , kafkaSettings :: Core.Maybe Types.KafkaSettings
    -- ^ Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
  , kinesisSettings :: Core.Maybe Types.KinesisSettings
    -- ^ Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
  , microsoftSQLServerSettings :: Core.Maybe Types.MicrosoftSQLServerSettings
    -- ^ Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , mongoDbSettings :: Core.Maybe Types.MongoDbSettings
    -- ^ Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
  , mySQLSettings :: Core.Maybe Types.MySQLSettings
    -- ^ Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , neptuneSettings :: Core.Maybe Types.NeptuneSettings
    -- ^ Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./ 
  , oracleSettings :: Core.Maybe Types.OracleSettings
    -- ^ Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , password :: Core.Maybe Types.Password
    -- ^ The password to be used to login to the endpoint database.
  , port :: Core.Maybe Core.Int
    -- ^ The port used by the endpoint database.
  , postgreSQLSettings :: Core.Maybe Types.PostgreSQLSettings
    -- ^ Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , redshiftSettings :: Core.Maybe Types.RedshiftSettings
  , s3Settings :: Core.Maybe Types.S3Settings
    -- ^ Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , serverName :: Core.Maybe Core.Text
    -- ^ The name of the server where the endpoint database resides.
  , serviceAccessRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the service access role you want to use to modify the endpoint. 
  , sslMode :: Core.Maybe Types.DmsSslModeValue
    -- ^ The SSL mode used to connect to the endpoint. The default value is @none@ .
  , sybaseSettings :: Core.Maybe Types.SybaseSettings
    -- ^ Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , username :: Core.Maybe Core.Text
    -- ^ The user name to be used to login to the endpoint database.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyEndpoint' value with any optional fields omitted.
mkModifyEndpoint
    :: Core.Text -- ^ 'endpointArn'
    -> ModifyEndpoint
mkModifyEndpoint endpointArn
  = ModifyEndpoint'{endpointArn, certificateArn = Core.Nothing,
                    databaseName = Core.Nothing, dmsTransferSettings = Core.Nothing,
                    docDbSettings = Core.Nothing, dynamoDbSettings = Core.Nothing,
                    elasticsearchSettings = Core.Nothing,
                    endpointIdentifier = Core.Nothing, endpointType = Core.Nothing,
                    engineName = Core.Nothing, externalTableDefinition = Core.Nothing,
                    extraConnectionAttributes = Core.Nothing,
                    iBMDb2Settings = Core.Nothing, kafkaSettings = Core.Nothing,
                    kinesisSettings = Core.Nothing,
                    microsoftSQLServerSettings = Core.Nothing,
                    mongoDbSettings = Core.Nothing, mySQLSettings = Core.Nothing,
                    neptuneSettings = Core.Nothing, oracleSettings = Core.Nothing,
                    password = Core.Nothing, port = Core.Nothing,
                    postgreSQLSettings = Core.Nothing, redshiftSettings = Core.Nothing,
                    s3Settings = Core.Nothing, serverName = Core.Nothing,
                    serviceAccessRoleArn = Core.Nothing, sslMode = Core.Nothing,
                    sybaseSettings = Core.Nothing, username = Core.Nothing}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEndpointArn :: Lens.Lens' ModifyEndpoint Core.Text
meEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE meEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the certificate used for SSL connection.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meCertificateArn :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE meCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The name of the endpoint database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meDatabaseName :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE meDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

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
meDmsTransferSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.DmsTransferSettings)
meDmsTransferSettings = Lens.field @"dmsTransferSettings"
{-# INLINEABLE meDmsTransferSettings #-}
{-# DEPRECATED dmsTransferSettings "Use generic-lens or generic-optics with 'dmsTransferSettings' instead"  #-}

-- | Settings in JSON format for the source DocumentDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DocumentDB.html Using DocumentDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'docDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meDocDbSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.DocDbSettings)
meDocDbSettings = Lens.field @"docDbSettings"
{-# INLINEABLE meDocDbSettings #-}
{-# DEPRECATED docDbSettings "Use generic-lens or generic-optics with 'docDbSettings' instead"  #-}

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'dynamoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meDynamoDbSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.DynamoDbSettings)
meDynamoDbSettings = Lens.field @"dynamoDbSettings"
{-# INLINEABLE meDynamoDbSettings #-}
{-# DEPRECATED dynamoDbSettings "Use generic-lens or generic-optics with 'dynamoDbSettings' instead"  #-}

-- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'elasticsearchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meElasticsearchSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.ElasticsearchSettings)
meElasticsearchSettings = Lens.field @"elasticsearchSettings"
{-# INLINEABLE meElasticsearchSettings #-}
{-# DEPRECATED elasticsearchSettings "Use generic-lens or generic-optics with 'elasticsearchSettings' instead"  #-}

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEndpointIdentifier :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meEndpointIdentifier = Lens.field @"endpointIdentifier"
{-# INLINEABLE meEndpointIdentifier #-}
{-# DEPRECATED endpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead"  #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEndpointType :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.ReplicationEndpointTypeValue)
meEndpointType = Lens.field @"endpointType"
{-# INLINEABLE meEndpointType #-}
{-# DEPRECATED endpointType "Use generic-lens or generic-optics with 'endpointType' instead"  #-}

-- | The type of engine for the endpoint. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meEngineName :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meEngineName = Lens.field @"engineName"
{-# INLINEABLE meEngineName #-}
{-# DEPRECATED engineName "Use generic-lens or generic-optics with 'engineName' instead"  #-}

-- | The external table definition.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meExternalTableDefinition :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meExternalTableDefinition = Lens.field @"externalTableDefinition"
{-# INLINEABLE meExternalTableDefinition #-}
{-# DEPRECATED externalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead"  #-}

-- | Additional attributes associated with the connection. To reset this parameter, pass the empty string ("") as an argument.
--
-- /Note:/ Consider using 'extraConnectionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meExtraConnectionAttributes :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meExtraConnectionAttributes = Lens.field @"extraConnectionAttributes"
{-# INLINEABLE meExtraConnectionAttributes #-}
{-# DEPRECATED extraConnectionAttributes "Use generic-lens or generic-optics with 'extraConnectionAttributes' instead"  #-}

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.ConnectionAttrib Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'iBMDb2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meIBMDb2Settings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.IBMDb2Settings)
meIBMDb2Settings = Lens.field @"iBMDb2Settings"
{-# INLINEABLE meIBMDb2Settings #-}
{-# DEPRECATED iBMDb2Settings "Use generic-lens or generic-optics with 'iBMDb2Settings' instead"  #-}

-- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'kafkaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meKafkaSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.KafkaSettings)
meKafkaSettings = Lens.field @"kafkaSettings"
{-# INLINEABLE meKafkaSettings #-}
{-# DEPRECATED kafkaSettings "Use generic-lens or generic-optics with 'kafkaSettings' instead"  #-}

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'kinesisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meKinesisSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.KinesisSettings)
meKinesisSettings = Lens.field @"kinesisSettings"
{-# INLINEABLE meKinesisSettings #-}
{-# DEPRECATED kinesisSettings "Use generic-lens or generic-optics with 'kinesisSettings' instead"  #-}

-- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.ConnectionAttrib Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'microsoftSQLServerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meMicrosoftSQLServerSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.MicrosoftSQLServerSettings)
meMicrosoftSQLServerSettings = Lens.field @"microsoftSQLServerSettings"
{-# INLINEABLE meMicrosoftSQLServerSettings #-}
{-# DEPRECATED microsoftSQLServerSettings "Use generic-lens or generic-optics with 'microsoftSQLServerSettings' instead"  #-}

-- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see the configuration properties section in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'mongoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meMongoDbSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.MongoDbSettings)
meMongoDbSettings = Lens.field @"mongoDbSettings"
{-# INLINEABLE meMongoDbSettings #-}
{-# DEPRECATED mongoDbSettings "Use generic-lens or generic-optics with 'mongoDbSettings' instead"  #-}

-- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.ConnectionAttrib Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.ConnectionAttrib Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'mySQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meMySQLSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.MySQLSettings)
meMySQLSettings = Lens.field @"mySQLSettings"
{-# INLINEABLE meMySQLSettings #-}
{-# DEPRECATED mySQLSettings "Use generic-lens or generic-optics with 'mySQLSettings' instead"  #-}

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'neptuneSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meNeptuneSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.NeptuneSettings)
meNeptuneSettings = Lens.field @"neptuneSettings"
{-# INLINEABLE meNeptuneSettings #-}
{-# DEPRECATED neptuneSettings "Use generic-lens or generic-optics with 'neptuneSettings' instead"  #-}

-- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.ConnectionAttrib Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'oracleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meOracleSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.OracleSettings)
meOracleSettings = Lens.field @"oracleSettings"
{-# INLINEABLE meOracleSettings #-}
{-# DEPRECATED oracleSettings "Use generic-lens or generic-optics with 'oracleSettings' instead"  #-}

-- | The password to be used to login to the endpoint database.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mePassword :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.Password)
mePassword = Lens.field @"password"
{-# INLINEABLE mePassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The port used by the endpoint database.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mePort :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Int)
mePort = Lens.field @"port"
{-# INLINEABLE mePort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.ConnectionAttrib Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'postgreSQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mePostgreSQLSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.PostgreSQLSettings)
mePostgreSQLSettings = Lens.field @"postgreSQLSettings"
{-# INLINEABLE mePostgreSQLSettings #-}
{-# DEPRECATED postgreSQLSettings "Use generic-lens or generic-optics with 'postgreSQLSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meRedshiftSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.RedshiftSettings)
meRedshiftSettings = Lens.field @"redshiftSettings"
{-# INLINEABLE meRedshiftSettings #-}
{-# DEPRECATED redshiftSettings "Use generic-lens or generic-optics with 'redshiftSettings' instead"  #-}

-- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meS3Settings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.S3Settings)
meS3Settings = Lens.field @"s3Settings"
{-# INLINEABLE meS3Settings #-}
{-# DEPRECATED s3Settings "Use generic-lens or generic-optics with 's3Settings' instead"  #-}

-- | The name of the server where the endpoint database resides.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meServerName :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meServerName = Lens.field @"serverName"
{-# INLINEABLE meServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The Amazon Resource Name (ARN) for the service access role you want to use to modify the endpoint. 
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meServiceAccessRoleArn :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# INLINEABLE meServiceAccessRoleArn #-}
{-# DEPRECATED serviceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead"  #-}

-- | The SSL mode used to connect to the endpoint. The default value is @none@ .
--
-- /Note:/ Consider using 'sslMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSslMode :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.DmsSslModeValue)
meSslMode = Lens.field @"sslMode"
{-# INLINEABLE meSslMode #-}
{-# DEPRECATED sslMode "Use generic-lens or generic-optics with 'sslMode' instead"  #-}

-- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.ConnectionAttrib Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'sybaseSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSybaseSettings :: Lens.Lens' ModifyEndpoint (Core.Maybe Types.SybaseSettings)
meSybaseSettings = Lens.field @"sybaseSettings"
{-# INLINEABLE meSybaseSettings #-}
{-# DEPRECATED sybaseSettings "Use generic-lens or generic-optics with 'sybaseSettings' instead"  #-}

-- | The user name to be used to login to the endpoint database.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meUsername :: Lens.Lens' ModifyEndpoint (Core.Maybe Core.Text)
meUsername = Lens.field @"username"
{-# INLINEABLE meUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery ModifyEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyEndpoint where
        toHeaders ModifyEndpoint{..}
          = Core.pure ("X-Amz-Target", "AmazonDMSv20160101.ModifyEndpoint")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyEndpoint where
        toJSON ModifyEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointArn" Core..= endpointArn),
                  ("CertificateArn" Core..=) Core.<$> certificateArn,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("DmsTransferSettings" Core..=) Core.<$> dmsTransferSettings,
                  ("DocDbSettings" Core..=) Core.<$> docDbSettings,
                  ("DynamoDbSettings" Core..=) Core.<$> dynamoDbSettings,
                  ("ElasticsearchSettings" Core..=) Core.<$> elasticsearchSettings,
                  ("EndpointIdentifier" Core..=) Core.<$> endpointIdentifier,
                  ("EndpointType" Core..=) Core.<$> endpointType,
                  ("EngineName" Core..=) Core.<$> engineName,
                  ("ExternalTableDefinition" Core..=) Core.<$>
                    externalTableDefinition,
                  ("ExtraConnectionAttributes" Core..=) Core.<$>
                    extraConnectionAttributes,
                  ("IBMDb2Settings" Core..=) Core.<$> iBMDb2Settings,
                  ("KafkaSettings" Core..=) Core.<$> kafkaSettings,
                  ("KinesisSettings" Core..=) Core.<$> kinesisSettings,
                  ("MicrosoftSQLServerSettings" Core..=) Core.<$>
                    microsoftSQLServerSettings,
                  ("MongoDbSettings" Core..=) Core.<$> mongoDbSettings,
                  ("MySQLSettings" Core..=) Core.<$> mySQLSettings,
                  ("NeptuneSettings" Core..=) Core.<$> neptuneSettings,
                  ("OracleSettings" Core..=) Core.<$> oracleSettings,
                  ("Password" Core..=) Core.<$> password,
                  ("Port" Core..=) Core.<$> port,
                  ("PostgreSQLSettings" Core..=) Core.<$> postgreSQLSettings,
                  ("RedshiftSettings" Core..=) Core.<$> redshiftSettings,
                  ("S3Settings" Core..=) Core.<$> s3Settings,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("ServiceAccessRoleArn" Core..=) Core.<$> serviceAccessRoleArn,
                  ("SslMode" Core..=) Core.<$> sslMode,
                  ("SybaseSettings" Core..=) Core.<$> sybaseSettings,
                  ("Username" Core..=) Core.<$> username])

instance Core.AWSRequest ModifyEndpoint where
        type Rs ModifyEndpoint = ModifyEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyEndpointResponse' Core.<$>
                   (x Core..:? "Endpoint") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkModifyEndpointResponse' smart constructor.
data ModifyEndpointResponse = ModifyEndpointResponse'
  { endpoint :: Core.Maybe Types.Endpoint
    -- ^ The modified endpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyEndpointResponse' value with any optional fields omitted.
mkModifyEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyEndpointResponse
mkModifyEndpointResponse responseStatus
  = ModifyEndpointResponse'{endpoint = Core.Nothing, responseStatus}

-- | The modified endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
merrsEndpoint :: Lens.Lens' ModifyEndpointResponse (Core.Maybe Types.Endpoint)
merrsEndpoint = Lens.field @"endpoint"
{-# INLINEABLE merrsEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
merrsResponseStatus :: Lens.Lens' ModifyEndpointResponse Core.Int
merrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE merrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
