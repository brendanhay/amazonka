{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Endpoint
  ( Endpoint (..)
  -- * Smart constructor
  , mkEndpoint
  -- * Lenses
  , eCertificateArn
  , eDatabaseName
  , eDmsTransferSettings
  , eDocDbSettings
  , eDynamoDbSettings
  , eElasticsearchSettings
  , eEndpointArn
  , eEndpointIdentifier
  , eEndpointType
  , eEngineDisplayName
  , eEngineName
  , eExternalId
  , eExternalTableDefinition
  , eExtraConnectionAttributes
  , eIBMDb2Settings
  , eKafkaSettings
  , eKinesisSettings
  , eKmsKeyId
  , eMicrosoftSQLServerSettings
  , eMongoDbSettings
  , eMySQLSettings
  , eNeptuneSettings
  , eOracleSettings
  , ePort
  , ePostgreSQLSettings
  , eRedshiftSettings
  , eS3Settings
  , eServerName
  , eServiceAccessRoleArn
  , eSslMode
  , eStatus
  , eSybaseSettings
  , eUsername
  ) where

import qualified Network.AWS.DMS.Types.DmsSslModeValue as Types
import qualified Network.AWS.DMS.Types.DmsTransferSettings as Types
import qualified Network.AWS.DMS.Types.DocDbSettings as Types
import qualified Network.AWS.DMS.Types.DynamoDbSettings as Types
import qualified Network.AWS.DMS.Types.ElasticsearchSettings as Types
import qualified Network.AWS.DMS.Types.IBMDb2Settings as Types
import qualified Network.AWS.DMS.Types.KafkaSettings as Types
import qualified Network.AWS.DMS.Types.KinesisSettings as Types
import qualified Network.AWS.DMS.Types.MicrosoftSQLServerSettings as Types
import qualified Network.AWS.DMS.Types.MongoDbSettings as Types
import qualified Network.AWS.DMS.Types.MySQLSettings as Types
import qualified Network.AWS.DMS.Types.NeptuneSettings as Types
import qualified Network.AWS.DMS.Types.OracleSettings as Types
import qualified Network.AWS.DMS.Types.PostgreSQLSettings as Types
import qualified Network.AWS.DMS.Types.RedshiftSettings as Types
import qualified Network.AWS.DMS.Types.ReplicationEndpointTypeValue as Types
import qualified Network.AWS.DMS.Types.S3Settings as Types
import qualified Network.AWS.DMS.Types.SybaseSettings as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an endpoint of a database instance in response to operations such as the following:
--
--
--     * @CreateEndpoint@ 
--
--
--     * @DescribeEndpoint@ 
--
--
--     * @DescribeEndpointTypes@ 
--
--
--     * @ModifyEndpoint@ 
--
--
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { certificateArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
  , databaseName :: Core.Maybe Core.Text
    -- ^ The name of the database at the endpoint.
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
    -- ^ The settings for the DynamoDB target endpoint. For more information, see the @DynamoDBSettings@ structure.
  , elasticsearchSettings :: Core.Maybe Types.ElasticsearchSettings
    -- ^ The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
  , endpointArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
  , endpointIdentifier :: Core.Maybe Core.Text
    -- ^ The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
  , endpointType :: Core.Maybe Types.ReplicationEndpointTypeValue
    -- ^ The type of endpoint. Valid values are @source@ and @target@ .
  , engineDisplayName :: Core.Maybe Core.Text
    -- ^ The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
  , engineName :: Core.Maybe Core.Text
    -- ^ The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
  , externalId :: Core.Maybe Core.Text
    -- ^ Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account. 
  , externalTableDefinition :: Core.Maybe Core.Text
    -- ^ The external table definition.
  , extraConnectionAttributes :: Core.Maybe Core.Text
    -- ^ Additional connection attributes used to connect to the endpoint.
  , iBMDb2Settings :: Core.Maybe Types.IBMDb2Settings
    -- ^ The settings for the IBM Db2 LUW source endpoint. For more information, see the @IBMDb2Settings@ structure. 
  , kafkaSettings :: Core.Maybe Types.KafkaSettings
    -- ^ The settings for the Apache Kafka target endpoint. For more information, see the @KafkaSettings@ structure.
  , kinesisSettings :: Core.Maybe Types.KinesisSettings
    -- ^ The settings for the Amazon Kinesis target endpoint. For more information, see the @KinesisSettings@ structure.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
  , microsoftSQLServerSettings :: Core.Maybe Types.MicrosoftSQLServerSettings
    -- ^ The settings for the Microsoft SQL Server source and target endpoint. For more information, see the @MicrosoftSQLServerSettings@ structure.
  , mongoDbSettings :: Core.Maybe Types.MongoDbSettings
    -- ^ The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
  , mySQLSettings :: Core.Maybe Types.MySQLSettings
    -- ^ The settings for the MySQL source and target endpoint. For more information, see the @MySQLSettings@ structure.
  , neptuneSettings :: Core.Maybe Types.NeptuneSettings
    -- ^ The settings for the Amazon Neptune target endpoint. For more information, see the @NeptuneSettings@ structure.
  , oracleSettings :: Core.Maybe Types.OracleSettings
    -- ^ The settings for the Oracle source and target endpoint. For more information, see the @OracleSettings@ structure.
  , port :: Core.Maybe Core.Int
    -- ^ The port value used to access the endpoint.
  , postgreSQLSettings :: Core.Maybe Types.PostgreSQLSettings
    -- ^ The settings for the PostgreSQL source and target endpoint. For more information, see the @PostgreSQLSettings@ structure.
  , redshiftSettings :: Core.Maybe Types.RedshiftSettings
    -- ^ Settings for the Amazon Redshift endpoint.
  , s3Settings :: Core.Maybe Types.S3Settings
    -- ^ The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
  , serverName :: Core.Maybe Core.Text
    -- ^ The name of the server at the endpoint.
  , serviceAccessRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) used by the service access IAM role.
  , sslMode :: Core.Maybe Types.DmsSslModeValue
    -- ^ The SSL mode used to connect to the endpoint. The default value is @none@ .
  , status :: Core.Maybe Core.Text
    -- ^ The status of the endpoint.
  , sybaseSettings :: Core.Maybe Types.SybaseSettings
    -- ^ The settings for the SAP ASE source and target endpoint. For more information, see the @SybaseSettings@ structure.
  , username :: Core.Maybe Core.Text
    -- ^ The user name used to connect to the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Endpoint' value with any optional fields omitted.
mkEndpoint
    :: Endpoint
mkEndpoint
  = Endpoint'{certificateArn = Core.Nothing,
              databaseName = Core.Nothing, dmsTransferSettings = Core.Nothing,
              docDbSettings = Core.Nothing, dynamoDbSettings = Core.Nothing,
              elasticsearchSettings = Core.Nothing, endpointArn = Core.Nothing,
              endpointIdentifier = Core.Nothing, endpointType = Core.Nothing,
              engineDisplayName = Core.Nothing, engineName = Core.Nothing,
              externalId = Core.Nothing, externalTableDefinition = Core.Nothing,
              extraConnectionAttributes = Core.Nothing,
              iBMDb2Settings = Core.Nothing, kafkaSettings = Core.Nothing,
              kinesisSettings = Core.Nothing, kmsKeyId = Core.Nothing,
              microsoftSQLServerSettings = Core.Nothing,
              mongoDbSettings = Core.Nothing, mySQLSettings = Core.Nothing,
              neptuneSettings = Core.Nothing, oracleSettings = Core.Nothing,
              port = Core.Nothing, postgreSQLSettings = Core.Nothing,
              redshiftSettings = Core.Nothing, s3Settings = Core.Nothing,
              serverName = Core.Nothing, serviceAccessRoleArn = Core.Nothing,
              sslMode = Core.Nothing, status = Core.Nothing,
              sybaseSettings = Core.Nothing, username = Core.Nothing}

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCertificateArn :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE eCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The name of the database at the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDatabaseName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE eDatabaseName #-}
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
eDmsTransferSettings :: Lens.Lens' Endpoint (Core.Maybe Types.DmsTransferSettings)
eDmsTransferSettings = Lens.field @"dmsTransferSettings"
{-# INLINEABLE eDmsTransferSettings #-}
{-# DEPRECATED dmsTransferSettings "Use generic-lens or generic-optics with 'dmsTransferSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'docDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDocDbSettings :: Lens.Lens' Endpoint (Core.Maybe Types.DocDbSettings)
eDocDbSettings = Lens.field @"docDbSettings"
{-# INLINEABLE eDocDbSettings #-}
{-# DEPRECATED docDbSettings "Use generic-lens or generic-optics with 'docDbSettings' instead"  #-}

-- | The settings for the DynamoDB target endpoint. For more information, see the @DynamoDBSettings@ structure.
--
-- /Note:/ Consider using 'dynamoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDynamoDbSettings :: Lens.Lens' Endpoint (Core.Maybe Types.DynamoDbSettings)
eDynamoDbSettings = Lens.field @"dynamoDbSettings"
{-# INLINEABLE eDynamoDbSettings #-}
{-# DEPRECATED dynamoDbSettings "Use generic-lens or generic-optics with 'dynamoDbSettings' instead"  #-}

-- | The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
--
-- /Note:/ Consider using 'elasticsearchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eElasticsearchSettings :: Lens.Lens' Endpoint (Core.Maybe Types.ElasticsearchSettings)
eElasticsearchSettings = Lens.field @"elasticsearchSettings"
{-# INLINEABLE eElasticsearchSettings #-}
{-# DEPRECATED elasticsearchSettings "Use generic-lens or generic-optics with 'elasticsearchSettings' instead"  #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointArn :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE eEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointIdentifier :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eEndpointIdentifier = Lens.field @"endpointIdentifier"
{-# INLINEABLE eEndpointIdentifier #-}
{-# DEPRECATED endpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead"  #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointType :: Lens.Lens' Endpoint (Core.Maybe Types.ReplicationEndpointTypeValue)
eEndpointType = Lens.field @"endpointType"
{-# INLINEABLE eEndpointType #-}
{-# DEPRECATED endpointType "Use generic-lens or generic-optics with 'endpointType' instead"  #-}

-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- /Note:/ Consider using 'engineDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEngineDisplayName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eEngineDisplayName = Lens.field @"engineDisplayName"
{-# INLINEABLE eEngineDisplayName #-}
{-# DEPRECATED engineDisplayName "Use generic-lens or generic-optics with 'engineDisplayName' instead"  #-}

-- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEngineName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eEngineName = Lens.field @"engineName"
{-# INLINEABLE eEngineName #-}
{-# DEPRECATED engineName "Use generic-lens or generic-optics with 'engineName' instead"  #-}

-- | Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account. 
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExternalId :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eExternalId = Lens.field @"externalId"
{-# INLINEABLE eExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

-- | The external table definition.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExternalTableDefinition :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eExternalTableDefinition = Lens.field @"externalTableDefinition"
{-# INLINEABLE eExternalTableDefinition #-}
{-# DEPRECATED externalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead"  #-}

-- | Additional connection attributes used to connect to the endpoint.
--
-- /Note:/ Consider using 'extraConnectionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExtraConnectionAttributes :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eExtraConnectionAttributes = Lens.field @"extraConnectionAttributes"
{-# INLINEABLE eExtraConnectionAttributes #-}
{-# DEPRECATED extraConnectionAttributes "Use generic-lens or generic-optics with 'extraConnectionAttributes' instead"  #-}

-- | The settings for the IBM Db2 LUW source endpoint. For more information, see the @IBMDb2Settings@ structure. 
--
-- /Note:/ Consider using 'iBMDb2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eIBMDb2Settings :: Lens.Lens' Endpoint (Core.Maybe Types.IBMDb2Settings)
eIBMDb2Settings = Lens.field @"iBMDb2Settings"
{-# INLINEABLE eIBMDb2Settings #-}
{-# DEPRECATED iBMDb2Settings "Use generic-lens or generic-optics with 'iBMDb2Settings' instead"  #-}

-- | The settings for the Apache Kafka target endpoint. For more information, see the @KafkaSettings@ structure.
--
-- /Note:/ Consider using 'kafkaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKafkaSettings :: Lens.Lens' Endpoint (Core.Maybe Types.KafkaSettings)
eKafkaSettings = Lens.field @"kafkaSettings"
{-# INLINEABLE eKafkaSettings #-}
{-# DEPRECATED kafkaSettings "Use generic-lens or generic-optics with 'kafkaSettings' instead"  #-}

-- | The settings for the Amazon Kinesis target endpoint. For more information, see the @KinesisSettings@ structure.
--
-- /Note:/ Consider using 'kinesisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKinesisSettings :: Lens.Lens' Endpoint (Core.Maybe Types.KinesisSettings)
eKinesisSettings = Lens.field @"kinesisSettings"
{-# INLINEABLE eKinesisSettings #-}
{-# DEPRECATED kinesisSettings "Use generic-lens or generic-optics with 'kinesisSettings' instead"  #-}

-- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKmsKeyId :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE eKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The settings for the Microsoft SQL Server source and target endpoint. For more information, see the @MicrosoftSQLServerSettings@ structure.
--
-- /Note:/ Consider using 'microsoftSQLServerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMicrosoftSQLServerSettings :: Lens.Lens' Endpoint (Core.Maybe Types.MicrosoftSQLServerSettings)
eMicrosoftSQLServerSettings = Lens.field @"microsoftSQLServerSettings"
{-# INLINEABLE eMicrosoftSQLServerSettings #-}
{-# DEPRECATED microsoftSQLServerSettings "Use generic-lens or generic-optics with 'microsoftSQLServerSettings' instead"  #-}

-- | The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
--
-- /Note:/ Consider using 'mongoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMongoDbSettings :: Lens.Lens' Endpoint (Core.Maybe Types.MongoDbSettings)
eMongoDbSettings = Lens.field @"mongoDbSettings"
{-# INLINEABLE eMongoDbSettings #-}
{-# DEPRECATED mongoDbSettings "Use generic-lens or generic-optics with 'mongoDbSettings' instead"  #-}

-- | The settings for the MySQL source and target endpoint. For more information, see the @MySQLSettings@ structure.
--
-- /Note:/ Consider using 'mySQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMySQLSettings :: Lens.Lens' Endpoint (Core.Maybe Types.MySQLSettings)
eMySQLSettings = Lens.field @"mySQLSettings"
{-# INLINEABLE eMySQLSettings #-}
{-# DEPRECATED mySQLSettings "Use generic-lens or generic-optics with 'mySQLSettings' instead"  #-}

-- | The settings for the Amazon Neptune target endpoint. For more information, see the @NeptuneSettings@ structure.
--
-- /Note:/ Consider using 'neptuneSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eNeptuneSettings :: Lens.Lens' Endpoint (Core.Maybe Types.NeptuneSettings)
eNeptuneSettings = Lens.field @"neptuneSettings"
{-# INLINEABLE eNeptuneSettings #-}
{-# DEPRECATED neptuneSettings "Use generic-lens or generic-optics with 'neptuneSettings' instead"  #-}

-- | The settings for the Oracle source and target endpoint. For more information, see the @OracleSettings@ structure.
--
-- /Note:/ Consider using 'oracleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOracleSettings :: Lens.Lens' Endpoint (Core.Maybe Types.OracleSettings)
eOracleSettings = Lens.field @"oracleSettings"
{-# INLINEABLE eOracleSettings #-}
{-# DEPRECATED oracleSettings "Use generic-lens or generic-optics with 'oracleSettings' instead"  #-}

-- | The port value used to access the endpoint.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
ePort = Lens.field @"port"
{-# INLINEABLE ePort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The settings for the PostgreSQL source and target endpoint. For more information, see the @PostgreSQLSettings@ structure.
--
-- /Note:/ Consider using 'postgreSQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePostgreSQLSettings :: Lens.Lens' Endpoint (Core.Maybe Types.PostgreSQLSettings)
ePostgreSQLSettings = Lens.field @"postgreSQLSettings"
{-# INLINEABLE ePostgreSQLSettings #-}
{-# DEPRECATED postgreSQLSettings "Use generic-lens or generic-optics with 'postgreSQLSettings' instead"  #-}

-- | Settings for the Amazon Redshift endpoint.
--
-- /Note:/ Consider using 'redshiftSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRedshiftSettings :: Lens.Lens' Endpoint (Core.Maybe Types.RedshiftSettings)
eRedshiftSettings = Lens.field @"redshiftSettings"
{-# INLINEABLE eRedshiftSettings #-}
{-# DEPRECATED redshiftSettings "Use generic-lens or generic-optics with 'redshiftSettings' instead"  #-}

-- | The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eS3Settings :: Lens.Lens' Endpoint (Core.Maybe Types.S3Settings)
eS3Settings = Lens.field @"s3Settings"
{-# INLINEABLE eS3Settings #-}
{-# DEPRECATED s3Settings "Use generic-lens or generic-optics with 's3Settings' instead"  #-}

-- | The name of the server at the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eServerName :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eServerName = Lens.field @"serverName"
{-# INLINEABLE eServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eServiceAccessRoleArn :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# INLINEABLE eServiceAccessRoleArn #-}
{-# DEPRECATED serviceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead"  #-}

-- | The SSL mode used to connect to the endpoint. The default value is @none@ .
--
-- /Note:/ Consider using 'sslMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSslMode :: Lens.Lens' Endpoint (Core.Maybe Types.DmsSslModeValue)
eSslMode = Lens.field @"sslMode"
{-# INLINEABLE eSslMode #-}
{-# DEPRECATED sslMode "Use generic-lens or generic-optics with 'sslMode' instead"  #-}

-- | The status of the endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatus :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eStatus = Lens.field @"status"
{-# INLINEABLE eStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The settings for the SAP ASE source and target endpoint. For more information, see the @SybaseSettings@ structure.
--
-- /Note:/ Consider using 'sybaseSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSybaseSettings :: Lens.Lens' Endpoint (Core.Maybe Types.SybaseSettings)
eSybaseSettings = Lens.field @"sybaseSettings"
{-# INLINEABLE eSybaseSettings #-}
{-# DEPRECATED sybaseSettings "Use generic-lens or generic-optics with 'sybaseSettings' instead"  #-}

-- | The user name used to connect to the endpoint.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eUsername :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
eUsername = Lens.field @"username"
{-# INLINEABLE eUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON Endpoint where
        parseJSON
          = Core.withObject "Endpoint" Core.$
              \ x ->
                Endpoint' Core.<$>
                  (x Core..:? "CertificateArn") Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "DmsTransferSettings"
                    Core.<*> x Core..:? "DocDbSettings"
                    Core.<*> x Core..:? "DynamoDbSettings"
                    Core.<*> x Core..:? "ElasticsearchSettings"
                    Core.<*> x Core..:? "EndpointArn"
                    Core.<*> x Core..:? "EndpointIdentifier"
                    Core.<*> x Core..:? "EndpointType"
                    Core.<*> x Core..:? "EngineDisplayName"
                    Core.<*> x Core..:? "EngineName"
                    Core.<*> x Core..:? "ExternalId"
                    Core.<*> x Core..:? "ExternalTableDefinition"
                    Core.<*> x Core..:? "ExtraConnectionAttributes"
                    Core.<*> x Core..:? "IBMDb2Settings"
                    Core.<*> x Core..:? "KafkaSettings"
                    Core.<*> x Core..:? "KinesisSettings"
                    Core.<*> x Core..:? "KmsKeyId"
                    Core.<*> x Core..:? "MicrosoftSQLServerSettings"
                    Core.<*> x Core..:? "MongoDbSettings"
                    Core.<*> x Core..:? "MySQLSettings"
                    Core.<*> x Core..:? "NeptuneSettings"
                    Core.<*> x Core..:? "OracleSettings"
                    Core.<*> x Core..:? "Port"
                    Core.<*> x Core..:? "PostgreSQLSettings"
                    Core.<*> x Core..:? "RedshiftSettings"
                    Core.<*> x Core..:? "S3Settings"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "ServiceAccessRoleArn"
                    Core.<*> x Core..:? "SslMode"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "SybaseSettings"
                    Core.<*> x Core..:? "Username"
