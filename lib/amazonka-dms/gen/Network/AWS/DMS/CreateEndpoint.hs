{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CreateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the provided settings.
module Network.AWS.DMS.CreateEndpoint
    (
    -- * Creating a request
      CreateEndpoint (..)
    , mkCreateEndpoint
    -- ** Request lenses
    , ceEndpointIdentifier
    , ceEndpointType
    , ceEngineName
    , ceCertificateArn
    , ceDatabaseName
    , ceDmsTransferSettings
    , ceDocDbSettings
    , ceDynamoDbSettings
    , ceElasticsearchSettings
    , ceExternalTableDefinition
    , ceExtraConnectionAttributes
    , ceIBMDb2Settings
    , ceKafkaSettings
    , ceKinesisSettings
    , ceKmsKeyId
    , ceMicrosoftSQLServerSettings
    , ceMongoDbSettings
    , ceMySQLSettings
    , ceNeptuneSettings
    , ceOracleSettings
    , cePassword
    , cePort
    , cePostgreSQLSettings
    , ceRedshiftSettings
    , ceResourceIdentifier
    , ceS3Settings
    , ceServerName
    , ceServiceAccessRoleArn
    , ceSslMode
    , ceSybaseSettings
    , ceTags
    , ceUsername

    -- * Destructuring the response
    , CreateEndpointResponse (..)
    , mkCreateEndpointResponse
    -- ** Response lenses
    , cerrsEndpoint
    , cerrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { endpointIdentifier :: Core.Text
    -- ^ The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
  , endpointType :: Types.ReplicationEndpointTypeValue
    -- ^ The type of endpoint. Valid values are @source@ and @target@ .
  , engineName :: Core.Text
    -- ^ The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
  , certificateArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the certificate.
  , databaseName :: Core.Maybe Core.Text
    -- ^ The name of the endpoint database.
  , dmsTransferSettings :: Core.Maybe Types.DmsTransferSettings
    -- ^ The settings in JSON format for the DMS transfer type of source endpoint. 
--
-- Possible settings include the following:
--
--     * @ServiceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.
--
--
--     * @BucketName@ - The name of the S3 bucket to use.
--
--
--     * @CompressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value.
--
--
-- Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@ 
-- JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @ 
  , docDbSettings :: Core.Maybe Types.DocDbSettings
  , dynamoDbSettings :: Core.Maybe Types.DynamoDbSettings
    -- ^ Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./ 
  , elasticsearchSettings :: Core.Maybe Types.ElasticsearchSettings
    -- ^ Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
  , externalTableDefinition :: Core.Maybe Core.Text
    -- ^ The external table definition. 
  , extraConnectionAttributes :: Core.Maybe Core.Text
    -- ^ Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./ 
  , iBMDb2Settings :: Core.Maybe Types.IBMDb2Settings
    -- ^ Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , kafkaSettings :: Core.Maybe Types.KafkaSettings
    -- ^ Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
  , kinesisSettings :: Core.Maybe Types.KinesisSettings
    -- ^ Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
  , microsoftSQLServerSettings :: Core.Maybe Types.MicrosoftSQLServerSettings
    -- ^ Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , mongoDbSettings :: Core.Maybe Types.MongoDbSettings
    -- ^ Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
  , mySQLSettings :: Core.Maybe Types.MySQLSettings
    -- ^ Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , neptuneSettings :: Core.Maybe Types.NeptuneSettings
    -- ^ Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./ 
  , oracleSettings :: Core.Maybe Types.OracleSettings
    -- ^ Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , password :: Core.Maybe Types.Password
    -- ^ The password to be used to log in to the endpoint database.
  , port :: Core.Maybe Core.Int
    -- ^ The port used by the endpoint database.
  , postgreSQLSettings :: Core.Maybe Types.PostgreSQLSettings
    -- ^ Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , redshiftSettings :: Core.Maybe Types.RedshiftSettings
  , resourceIdentifier :: Core.Maybe Core.Text
    -- ^ A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
  , s3Settings :: Core.Maybe Types.S3Settings
    -- ^ Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , serverName :: Core.Maybe Core.Text
    -- ^ The name of the server where the endpoint database resides.
  , serviceAccessRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint. 
  , sslMode :: Core.Maybe Types.DmsSslModeValue
    -- ^ The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@ 
  , sybaseSettings :: Core.Maybe Types.SybaseSettings
    -- ^ Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags to be assigned to the endpoint.
  , username :: Core.Maybe Core.Text
    -- ^ The user name to be used to log in to the endpoint database.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpoint' value with any optional fields omitted.
mkCreateEndpoint
    :: Core.Text -- ^ 'endpointIdentifier'
    -> Types.ReplicationEndpointTypeValue -- ^ 'endpointType'
    -> Core.Text -- ^ 'engineName'
    -> CreateEndpoint
mkCreateEndpoint endpointIdentifier endpointType engineName
  = CreateEndpoint'{endpointIdentifier, endpointType, engineName,
                    certificateArn = Core.Nothing, databaseName = Core.Nothing,
                    dmsTransferSettings = Core.Nothing, docDbSettings = Core.Nothing,
                    dynamoDbSettings = Core.Nothing,
                    elasticsearchSettings = Core.Nothing,
                    externalTableDefinition = Core.Nothing,
                    extraConnectionAttributes = Core.Nothing,
                    iBMDb2Settings = Core.Nothing, kafkaSettings = Core.Nothing,
                    kinesisSettings = Core.Nothing, kmsKeyId = Core.Nothing,
                    microsoftSQLServerSettings = Core.Nothing,
                    mongoDbSettings = Core.Nothing, mySQLSettings = Core.Nothing,
                    neptuneSettings = Core.Nothing, oracleSettings = Core.Nothing,
                    password = Core.Nothing, port = Core.Nothing,
                    postgreSQLSettings = Core.Nothing, redshiftSettings = Core.Nothing,
                    resourceIdentifier = Core.Nothing, s3Settings = Core.Nothing,
                    serverName = Core.Nothing, serviceAccessRoleArn = Core.Nothing,
                    sslMode = Core.Nothing, sybaseSettings = Core.Nothing,
                    tags = Core.Nothing, username = Core.Nothing}

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointIdentifier :: Lens.Lens' CreateEndpoint Core.Text
ceEndpointIdentifier = Lens.field @"endpointIdentifier"
{-# INLINEABLE ceEndpointIdentifier #-}
{-# DEPRECATED endpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead"  #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointType :: Lens.Lens' CreateEndpoint Types.ReplicationEndpointTypeValue
ceEndpointType = Lens.field @"endpointType"
{-# INLINEABLE ceEndpointType #-}
{-# DEPRECATED endpointType "Use generic-lens or generic-optics with 'endpointType' instead"  #-}

-- | The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEngineName :: Lens.Lens' CreateEndpoint Core.Text
ceEngineName = Lens.field @"engineName"
{-# INLINEABLE ceEngineName #-}
{-# DEPRECATED engineName "Use generic-lens or generic-optics with 'engineName' instead"  #-}

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceCertificateArn :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE ceCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The name of the endpoint database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDatabaseName :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE ceDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The settings in JSON format for the DMS transfer type of source endpoint. 
--
-- Possible settings include the following:
--
--     * @ServiceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.
--
--
--     * @BucketName@ - The name of the S3 bucket to use.
--
--
--     * @CompressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value.
--
--
-- Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@ 
-- JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @ 
--
-- /Note:/ Consider using 'dmsTransferSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDmsTransferSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DmsTransferSettings)
ceDmsTransferSettings = Lens.field @"dmsTransferSettings"
{-# INLINEABLE ceDmsTransferSettings #-}
{-# DEPRECATED dmsTransferSettings "Use generic-lens or generic-optics with 'dmsTransferSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'docDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDocDbSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DocDbSettings)
ceDocDbSettings = Lens.field @"docDbSettings"
{-# INLINEABLE ceDocDbSettings #-}
{-# DEPRECATED docDbSettings "Use generic-lens or generic-optics with 'docDbSettings' instead"  #-}

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'dynamoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDynamoDbSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DynamoDbSettings)
ceDynamoDbSettings = Lens.field @"dynamoDbSettings"
{-# INLINEABLE ceDynamoDbSettings #-}
{-# DEPRECATED dynamoDbSettings "Use generic-lens or generic-optics with 'dynamoDbSettings' instead"  #-}

-- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- /Note:/ Consider using 'elasticsearchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceElasticsearchSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ElasticsearchSettings)
ceElasticsearchSettings = Lens.field @"elasticsearchSettings"
{-# INLINEABLE ceElasticsearchSettings #-}
{-# DEPRECATED elasticsearchSettings "Use generic-lens or generic-optics with 'elasticsearchSettings' instead"  #-}

-- | The external table definition. 
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceExternalTableDefinition :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceExternalTableDefinition = Lens.field @"externalTableDefinition"
{-# INLINEABLE ceExternalTableDefinition #-}
{-# DEPRECATED externalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead"  #-}

-- | Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'extraConnectionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceExtraConnectionAttributes :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceExtraConnectionAttributes = Lens.field @"extraConnectionAttributes"
{-# INLINEABLE ceExtraConnectionAttributes #-}
{-# DEPRECATED extraConnectionAttributes "Use generic-lens or generic-optics with 'extraConnectionAttributes' instead"  #-}

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'iBMDb2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceIBMDb2Settings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.IBMDb2Settings)
ceIBMDb2Settings = Lens.field @"iBMDb2Settings"
{-# INLINEABLE ceIBMDb2Settings #-}
{-# DEPRECATED iBMDb2Settings "Use generic-lens or generic-optics with 'iBMDb2Settings' instead"  #-}

-- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'kafkaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKafkaSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.KafkaSettings)
ceKafkaSettings = Lens.field @"kafkaSettings"
{-# INLINEABLE ceKafkaSettings #-}
{-# DEPRECATED kafkaSettings "Use generic-lens or generic-optics with 'kafkaSettings' instead"  #-}

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'kinesisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKinesisSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.KinesisSettings)
ceKinesisSettings = Lens.field @"kinesisSettings"
{-# INLINEABLE ceKinesisSettings #-}
{-# DEPRECATED kinesisSettings "Use generic-lens or generic-optics with 'kinesisSettings' instead"  #-}

-- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKmsKeyId :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ceKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'microsoftSQLServerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMicrosoftSQLServerSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.MicrosoftSQLServerSettings)
ceMicrosoftSQLServerSettings = Lens.field @"microsoftSQLServerSettings"
{-# INLINEABLE ceMicrosoftSQLServerSettings #-}
{-# DEPRECATED microsoftSQLServerSettings "Use generic-lens or generic-optics with 'microsoftSQLServerSettings' instead"  #-}

-- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'mongoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMongoDbSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.MongoDbSettings)
ceMongoDbSettings = Lens.field @"mongoDbSettings"
{-# INLINEABLE ceMongoDbSettings #-}
{-# DEPRECATED mongoDbSettings "Use generic-lens or generic-optics with 'mongoDbSettings' instead"  #-}

-- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'mySQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMySQLSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.MySQLSettings)
ceMySQLSettings = Lens.field @"mySQLSettings"
{-# INLINEABLE ceMySQLSettings #-}
{-# DEPRECATED mySQLSettings "Use generic-lens or generic-optics with 'mySQLSettings' instead"  #-}

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'neptuneSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceNeptuneSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.NeptuneSettings)
ceNeptuneSettings = Lens.field @"neptuneSettings"
{-# INLINEABLE ceNeptuneSettings #-}
{-# DEPRECATED neptuneSettings "Use generic-lens or generic-optics with 'neptuneSettings' instead"  #-}

-- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'oracleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceOracleSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.OracleSettings)
ceOracleSettings = Lens.field @"oracleSettings"
{-# INLINEABLE ceOracleSettings #-}
{-# DEPRECATED oracleSettings "Use generic-lens or generic-optics with 'oracleSettings' instead"  #-}

-- | The password to be used to log in to the endpoint database.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePassword :: Lens.Lens' CreateEndpoint (Core.Maybe Types.Password)
cePassword = Lens.field @"password"
{-# INLINEABLE cePassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The port used by the endpoint database.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePort :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Int)
cePort = Lens.field @"port"
{-# INLINEABLE cePort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'postgreSQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePostgreSQLSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.PostgreSQLSettings)
cePostgreSQLSettings = Lens.field @"postgreSQLSettings"
{-# INLINEABLE cePostgreSQLSettings #-}
{-# DEPRECATED postgreSQLSettings "Use generic-lens or generic-optics with 'postgreSQLSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceRedshiftSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.RedshiftSettings)
ceRedshiftSettings = Lens.field @"redshiftSettings"
{-# INLINEABLE ceRedshiftSettings #-}
{-# DEPRECATED redshiftSettings "Use generic-lens or generic-optics with 'redshiftSettings' instead"  #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceResourceIdentifier :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE ceResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

-- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceS3Settings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.S3Settings)
ceS3Settings = Lens.field @"s3Settings"
{-# INLINEABLE ceS3Settings #-}
{-# DEPRECATED s3Settings "Use generic-lens or generic-optics with 's3Settings' instead"  #-}

-- | The name of the server where the endpoint database resides.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceServerName :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceServerName = Lens.field @"serverName"
{-# INLINEABLE ceServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint. 
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceServiceAccessRoleArn :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# INLINEABLE ceServiceAccessRoleArn #-}
{-# DEPRECATED serviceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead"  #-}

-- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@ 
--
-- /Note:/ Consider using 'sslMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSslMode :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DmsSslModeValue)
ceSslMode = Lens.field @"sslMode"
{-# INLINEABLE ceSslMode #-}
{-# DEPRECATED sslMode "Use generic-lens or generic-optics with 'sslMode' instead"  #-}

-- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'sybaseSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSybaseSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.SybaseSettings)
ceSybaseSettings = Lens.field @"sybaseSettings"
{-# INLINEABLE ceSybaseSettings #-}
{-# DEPRECATED sybaseSettings "Use generic-lens or generic-optics with 'sybaseSettings' instead"  #-}

-- | One or more tags to be assigned to the endpoint.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Core.Maybe [Types.Tag])
ceTags = Lens.field @"tags"
{-# INLINEABLE ceTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The user name to be used to log in to the endpoint database.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceUsername :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Text)
ceUsername = Lens.field @"username"
{-# INLINEABLE ceUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery CreateEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateEndpoint where
        toHeaders CreateEndpoint{..}
          = Core.pure ("X-Amz-Target", "AmazonDMSv20160101.CreateEndpoint")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateEndpoint where
        toJSON CreateEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointIdentifier" Core..= endpointIdentifier),
                  Core.Just ("EndpointType" Core..= endpointType),
                  Core.Just ("EngineName" Core..= engineName),
                  ("CertificateArn" Core..=) Core.<$> certificateArn,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("DmsTransferSettings" Core..=) Core.<$> dmsTransferSettings,
                  ("DocDbSettings" Core..=) Core.<$> docDbSettings,
                  ("DynamoDbSettings" Core..=) Core.<$> dynamoDbSettings,
                  ("ElasticsearchSettings" Core..=) Core.<$> elasticsearchSettings,
                  ("ExternalTableDefinition" Core..=) Core.<$>
                    externalTableDefinition,
                  ("ExtraConnectionAttributes" Core..=) Core.<$>
                    extraConnectionAttributes,
                  ("IBMDb2Settings" Core..=) Core.<$> iBMDb2Settings,
                  ("KafkaSettings" Core..=) Core.<$> kafkaSettings,
                  ("KinesisSettings" Core..=) Core.<$> kinesisSettings,
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
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
                  ("ResourceIdentifier" Core..=) Core.<$> resourceIdentifier,
                  ("S3Settings" Core..=) Core.<$> s3Settings,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("ServiceAccessRoleArn" Core..=) Core.<$> serviceAccessRoleArn,
                  ("SslMode" Core..=) Core.<$> sslMode,
                  ("SybaseSettings" Core..=) Core.<$> sybaseSettings,
                  ("Tags" Core..=) Core.<$> tags,
                  ("Username" Core..=) Core.<$> username])

instance Core.AWSRequest CreateEndpoint where
        type Rs CreateEndpoint = CreateEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateEndpointResponse' Core.<$>
                   (x Core..:? "Endpoint") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { endpoint :: Core.Maybe Types.Endpoint
    -- ^ The endpoint that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpointResponse' value with any optional fields omitted.
mkCreateEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateEndpointResponse
mkCreateEndpointResponse responseStatus
  = CreateEndpointResponse'{endpoint = Core.Nothing, responseStatus}

-- | The endpoint that was created.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsEndpoint :: Lens.Lens' CreateEndpointResponse (Core.Maybe Types.Endpoint)
cerrsEndpoint = Lens.field @"endpoint"
{-# INLINEABLE cerrsEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsResponseStatus :: Lens.Lens' CreateEndpointResponse Core.Int
cerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
