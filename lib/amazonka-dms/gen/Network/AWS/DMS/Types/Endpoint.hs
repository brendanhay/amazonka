-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eStatus,
    eDmsTransferSettings,
    eMySQLSettings,
    eServerName,
    eMicrosoftSQLServerSettings,
    eCertificateARN,
    eServiceAccessRoleARN,
    eDocDBSettings,
    eEngineDisplayName,
    ePostgreSQLSettings,
    eExtraConnectionAttributes,
    eKafkaSettings,
    eOracleSettings,
    eEndpointType,
    eRedshiftSettings,
    eElasticsearchSettings,
    eUsername,
    eExternalTableDefinition,
    eEngineName,
    eNeptuneSettings,
    eIBMDB2Settings,
    eKMSKeyId,
    eMongoDBSettings,
    eSSLMode,
    eSybaseSettings,
    eDatabaseName,
    eS3Settings,
    eKinesisSettings,
    eEndpointIdentifier,
    eExternalId,
    eDynamoDBSettings,
    eEndpointARN,
    ePort,
  )
where

import Network.AWS.DMS.Types.DmsSSLModeValue
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DocDBSettings
import Network.AWS.DMS.Types.DynamoDBSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.IBMDB2Settings
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MicrosoftSQLServerSettings
import Network.AWS.DMS.Types.MongoDBSettings
import Network.AWS.DMS.Types.MySQLSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.OracleSettings
import Network.AWS.DMS.Types.PostgreSQLSettings
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import Network.AWS.DMS.Types.S3Settings
import Network.AWS.DMS.Types.SybaseSettings
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { status :: Lude.Maybe Lude.Text,
    dmsTransferSettings :: Lude.Maybe DmsTransferSettings,
    mySQLSettings :: Lude.Maybe MySQLSettings,
    serverName :: Lude.Maybe Lude.Text,
    microsoftSQLServerSettings ::
      Lude.Maybe MicrosoftSQLServerSettings,
    certificateARN :: Lude.Maybe Lude.Text,
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    docDBSettings :: Lude.Maybe DocDBSettings,
    engineDisplayName :: Lude.Maybe Lude.Text,
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
    kmsKeyId :: Lude.Maybe Lude.Text,
    mongoDBSettings :: Lude.Maybe MongoDBSettings,
    sslMode :: Lude.Maybe DmsSSLModeValue,
    sybaseSettings :: Lude.Maybe SybaseSettings,
    databaseName :: Lude.Maybe Lude.Text,
    s3Settings :: Lude.Maybe S3Settings,
    kinesisSettings :: Lude.Maybe KinesisSettings,
    endpointIdentifier :: Lude.Maybe Lude.Text,
    externalId :: Lude.Maybe Lude.Text,
    dynamoDBSettings :: Lude.Maybe DynamoDBSettings,
    endpointARN :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
-- * 'databaseName' - The name of the database at the endpoint.
-- * 'dmsTransferSettings' - The settings in JSON format for the DMS transfer type of source endpoint.
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
-- * 'docDBSettings' - Undocumented field.
-- * 'dynamoDBSettings' - The settings for the DynamoDB target endpoint. For more information, see the @DynamoDBSettings@ structure.
-- * 'elasticsearchSettings' - The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
-- * 'endpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
-- * 'endpointType' - The type of endpoint. Valid values are @source@ and @target@ .
-- * 'engineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
-- * 'engineName' - The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
-- * 'externalId' - Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account.
-- * 'externalTableDefinition' - The external table definition.
-- * 'extraConnectionAttributes' - Additional connection attributes used to connect to the endpoint.
-- * 'iBMDB2Settings' - The settings for the IBM Db2 LUW source endpoint. For more information, see the @IBMDb2Settings@ structure.
-- * 'kafkaSettings' - The settings for the Apache Kafka target endpoint. For more information, see the @KafkaSettings@ structure.
-- * 'kinesisSettings' - The settings for the Amazon Kinesis target endpoint. For more information, see the @KinesisSettings@ structure.
-- * 'kmsKeyId' - An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'microsoftSQLServerSettings' - The settings for the Microsoft SQL Server source and target endpoint. For more information, see the @MicrosoftSQLServerSettings@ structure.
-- * 'mongoDBSettings' - The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
-- * 'mySQLSettings' - The settings for the MySQL source and target endpoint. For more information, see the @MySQLSettings@ structure.
-- * 'neptuneSettings' - The settings for the Amazon Neptune target endpoint. For more information, see the @NeptuneSettings@ structure.
-- * 'oracleSettings' - The settings for the Oracle source and target endpoint. For more information, see the @OracleSettings@ structure.
-- * 'port' - The port value used to access the endpoint.
-- * 'postgreSQLSettings' - The settings for the PostgreSQL source and target endpoint. For more information, see the @PostgreSQLSettings@ structure.
-- * 'redshiftSettings' - Settings for the Amazon Redshift endpoint.
-- * 's3Settings' - The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
-- * 'serverName' - The name of the server at the endpoint.
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role.
-- * 'sslMode' - The SSL mode used to connect to the endpoint. The default value is @none@ .
-- * 'status' - The status of the endpoint.
-- * 'sybaseSettings' - The settings for the SAP ASE source and target endpoint. For more information, see the @SybaseSettings@ structure.
-- * 'username' - The user name used to connect to the endpoint.
mkEndpoint ::
  Endpoint
mkEndpoint =
  Endpoint'
    { status = Lude.Nothing,
      dmsTransferSettings = Lude.Nothing,
      mySQLSettings = Lude.Nothing,
      serverName = Lude.Nothing,
      microsoftSQLServerSettings = Lude.Nothing,
      certificateARN = Lude.Nothing,
      serviceAccessRoleARN = Lude.Nothing,
      docDBSettings = Lude.Nothing,
      engineDisplayName = Lude.Nothing,
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
      kmsKeyId = Lude.Nothing,
      mongoDBSettings = Lude.Nothing,
      sslMode = Lude.Nothing,
      sybaseSettings = Lude.Nothing,
      databaseName = Lude.Nothing,
      s3Settings = Lude.Nothing,
      kinesisSettings = Lude.Nothing,
      endpointIdentifier = Lude.Nothing,
      externalId = Lude.Nothing,
      dynamoDBSettings = Lude.Nothing,
      endpointARN = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The status of the endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatus :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eStatus = Lens.lens (status :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Endpoint)
{-# DEPRECATED eStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
eDmsTransferSettings :: Lens.Lens' Endpoint (Lude.Maybe DmsTransferSettings)
eDmsTransferSettings = Lens.lens (dmsTransferSettings :: Endpoint -> Lude.Maybe DmsTransferSettings) (\s a -> s {dmsTransferSettings = a} :: Endpoint)
{-# DEPRECATED eDmsTransferSettings "Use generic-lens or generic-optics with 'dmsTransferSettings' instead." #-}

-- | The settings for the MySQL source and target endpoint. For more information, see the @MySQLSettings@ structure.
--
-- /Note:/ Consider using 'mySQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMySQLSettings :: Lens.Lens' Endpoint (Lude.Maybe MySQLSettings)
eMySQLSettings = Lens.lens (mySQLSettings :: Endpoint -> Lude.Maybe MySQLSettings) (\s a -> s {mySQLSettings = a} :: Endpoint)
{-# DEPRECATED eMySQLSettings "Use generic-lens or generic-optics with 'mySQLSettings' instead." #-}

-- | The name of the server at the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eServerName :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eServerName = Lens.lens (serverName :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: Endpoint)
{-# DEPRECATED eServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The settings for the Microsoft SQL Server source and target endpoint. For more information, see the @MicrosoftSQLServerSettings@ structure.
--
-- /Note:/ Consider using 'microsoftSQLServerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMicrosoftSQLServerSettings :: Lens.Lens' Endpoint (Lude.Maybe MicrosoftSQLServerSettings)
eMicrosoftSQLServerSettings = Lens.lens (microsoftSQLServerSettings :: Endpoint -> Lude.Maybe MicrosoftSQLServerSettings) (\s a -> s {microsoftSQLServerSettings = a} :: Endpoint)
{-# DEPRECATED eMicrosoftSQLServerSettings "Use generic-lens or generic-optics with 'microsoftSQLServerSettings' instead." #-}

-- | The Amazon Resource Name (ARN) used for SSL connection to the endpoint.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCertificateARN :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eCertificateARN = Lens.lens (certificateARN :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: Endpoint)
{-# DEPRECATED eCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eServiceAccessRoleARN :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: Endpoint)
{-# DEPRECATED eServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'docDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDocDBSettings :: Lens.Lens' Endpoint (Lude.Maybe DocDBSettings)
eDocDBSettings = Lens.lens (docDBSettings :: Endpoint -> Lude.Maybe DocDBSettings) (\s a -> s {docDBSettings = a} :: Endpoint)
{-# DEPRECATED eDocDBSettings "Use generic-lens or generic-optics with 'docDBSettings' instead." #-}

-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- /Note:/ Consider using 'engineDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEngineDisplayName :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eEngineDisplayName = Lens.lens (engineDisplayName :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {engineDisplayName = a} :: Endpoint)
{-# DEPRECATED eEngineDisplayName "Use generic-lens or generic-optics with 'engineDisplayName' instead." #-}

-- | The settings for the PostgreSQL source and target endpoint. For more information, see the @PostgreSQLSettings@ structure.
--
-- /Note:/ Consider using 'postgreSQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePostgreSQLSettings :: Lens.Lens' Endpoint (Lude.Maybe PostgreSQLSettings)
ePostgreSQLSettings = Lens.lens (postgreSQLSettings :: Endpoint -> Lude.Maybe PostgreSQLSettings) (\s a -> s {postgreSQLSettings = a} :: Endpoint)
{-# DEPRECATED ePostgreSQLSettings "Use generic-lens or generic-optics with 'postgreSQLSettings' instead." #-}

-- | Additional connection attributes used to connect to the endpoint.
--
-- /Note:/ Consider using 'extraConnectionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExtraConnectionAttributes :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eExtraConnectionAttributes = Lens.lens (extraConnectionAttributes :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {extraConnectionAttributes = a} :: Endpoint)
{-# DEPRECATED eExtraConnectionAttributes "Use generic-lens or generic-optics with 'extraConnectionAttributes' instead." #-}

-- | The settings for the Apache Kafka target endpoint. For more information, see the @KafkaSettings@ structure.
--
-- /Note:/ Consider using 'kafkaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKafkaSettings :: Lens.Lens' Endpoint (Lude.Maybe KafkaSettings)
eKafkaSettings = Lens.lens (kafkaSettings :: Endpoint -> Lude.Maybe KafkaSettings) (\s a -> s {kafkaSettings = a} :: Endpoint)
{-# DEPRECATED eKafkaSettings "Use generic-lens or generic-optics with 'kafkaSettings' instead." #-}

-- | The settings for the Oracle source and target endpoint. For more information, see the @OracleSettings@ structure.
--
-- /Note:/ Consider using 'oracleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOracleSettings :: Lens.Lens' Endpoint (Lude.Maybe OracleSettings)
eOracleSettings = Lens.lens (oracleSettings :: Endpoint -> Lude.Maybe OracleSettings) (\s a -> s {oracleSettings = a} :: Endpoint)
{-# DEPRECATED eOracleSettings "Use generic-lens or generic-optics with 'oracleSettings' instead." #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointType :: Lens.Lens' Endpoint (Lude.Maybe ReplicationEndpointTypeValue)
eEndpointType = Lens.lens (endpointType :: Endpoint -> Lude.Maybe ReplicationEndpointTypeValue) (\s a -> s {endpointType = a} :: Endpoint)
{-# DEPRECATED eEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | Settings for the Amazon Redshift endpoint.
--
-- /Note:/ Consider using 'redshiftSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRedshiftSettings :: Lens.Lens' Endpoint (Lude.Maybe RedshiftSettings)
eRedshiftSettings = Lens.lens (redshiftSettings :: Endpoint -> Lude.Maybe RedshiftSettings) (\s a -> s {redshiftSettings = a} :: Endpoint)
{-# DEPRECATED eRedshiftSettings "Use generic-lens or generic-optics with 'redshiftSettings' instead." #-}

-- | The settings for the Elasticsearch source endpoint. For more information, see the @ElasticsearchSettings@ structure.
--
-- /Note:/ Consider using 'elasticsearchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eElasticsearchSettings :: Lens.Lens' Endpoint (Lude.Maybe ElasticsearchSettings)
eElasticsearchSettings = Lens.lens (elasticsearchSettings :: Endpoint -> Lude.Maybe ElasticsearchSettings) (\s a -> s {elasticsearchSettings = a} :: Endpoint)
{-# DEPRECATED eElasticsearchSettings "Use generic-lens or generic-optics with 'elasticsearchSettings' instead." #-}

-- | The user name used to connect to the endpoint.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eUsername :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eUsername = Lens.lens (username :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: Endpoint)
{-# DEPRECATED eUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The external table definition.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExternalTableDefinition :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eExternalTableDefinition = Lens.lens (externalTableDefinition :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {externalTableDefinition = a} :: Endpoint)
{-# DEPRECATED eExternalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead." #-}

-- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEngineName :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eEngineName = Lens.lens (engineName :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {engineName = a} :: Endpoint)
{-# DEPRECATED eEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | The settings for the Amazon Neptune target endpoint. For more information, see the @NeptuneSettings@ structure.
--
-- /Note:/ Consider using 'neptuneSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eNeptuneSettings :: Lens.Lens' Endpoint (Lude.Maybe NeptuneSettings)
eNeptuneSettings = Lens.lens (neptuneSettings :: Endpoint -> Lude.Maybe NeptuneSettings) (\s a -> s {neptuneSettings = a} :: Endpoint)
{-# DEPRECATED eNeptuneSettings "Use generic-lens or generic-optics with 'neptuneSettings' instead." #-}

-- | The settings for the IBM Db2 LUW source endpoint. For more information, see the @IBMDb2Settings@ structure.
--
-- /Note:/ Consider using 'iBMDB2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eIBMDB2Settings :: Lens.Lens' Endpoint (Lude.Maybe IBMDB2Settings)
eIBMDB2Settings = Lens.lens (iBMDB2Settings :: Endpoint -> Lude.Maybe IBMDB2Settings) (\s a -> s {iBMDB2Settings = a} :: Endpoint)
{-# DEPRECATED eIBMDB2Settings "Use generic-lens or generic-optics with 'iBMDB2Settings' instead." #-}

-- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSKeyId :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eKMSKeyId = Lens.lens (kmsKeyId :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Endpoint)
{-# DEPRECATED eKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The settings for the MongoDB source endpoint. For more information, see the @MongoDbSettings@ structure.
--
-- /Note:/ Consider using 'mongoDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMongoDBSettings :: Lens.Lens' Endpoint (Lude.Maybe MongoDBSettings)
eMongoDBSettings = Lens.lens (mongoDBSettings :: Endpoint -> Lude.Maybe MongoDBSettings) (\s a -> s {mongoDBSettings = a} :: Endpoint)
{-# DEPRECATED eMongoDBSettings "Use generic-lens or generic-optics with 'mongoDBSettings' instead." #-}

-- | The SSL mode used to connect to the endpoint. The default value is @none@ .
--
-- /Note:/ Consider using 'sslMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSSLMode :: Lens.Lens' Endpoint (Lude.Maybe DmsSSLModeValue)
eSSLMode = Lens.lens (sslMode :: Endpoint -> Lude.Maybe DmsSSLModeValue) (\s a -> s {sslMode = a} :: Endpoint)
{-# DEPRECATED eSSLMode "Use generic-lens or generic-optics with 'sslMode' instead." #-}

-- | The settings for the SAP ASE source and target endpoint. For more information, see the @SybaseSettings@ structure.
--
-- /Note:/ Consider using 'sybaseSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSybaseSettings :: Lens.Lens' Endpoint (Lude.Maybe SybaseSettings)
eSybaseSettings = Lens.lens (sybaseSettings :: Endpoint -> Lude.Maybe SybaseSettings) (\s a -> s {sybaseSettings = a} :: Endpoint)
{-# DEPRECATED eSybaseSettings "Use generic-lens or generic-optics with 'sybaseSettings' instead." #-}

-- | The name of the database at the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDatabaseName :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eDatabaseName = Lens.lens (databaseName :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: Endpoint)
{-# DEPRECATED eDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The settings for the S3 target endpoint. For more information, see the @S3Settings@ structure.
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eS3Settings :: Lens.Lens' Endpoint (Lude.Maybe S3Settings)
eS3Settings = Lens.lens (s3Settings :: Endpoint -> Lude.Maybe S3Settings) (\s a -> s {s3Settings = a} :: Endpoint)
{-# DEPRECATED eS3Settings "Use generic-lens or generic-optics with 's3Settings' instead." #-}

-- | The settings for the Amazon Kinesis target endpoint. For more information, see the @KinesisSettings@ structure.
--
-- /Note:/ Consider using 'kinesisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKinesisSettings :: Lens.Lens' Endpoint (Lude.Maybe KinesisSettings)
eKinesisSettings = Lens.lens (kinesisSettings :: Endpoint -> Lude.Maybe KinesisSettings) (\s a -> s {kinesisSettings = a} :: Endpoint)
{-# DEPRECATED eKinesisSettings "Use generic-lens or generic-optics with 'kinesisSettings' instead." #-}

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointIdentifier :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eEndpointIdentifier = Lens.lens (endpointIdentifier :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointIdentifier = a} :: Endpoint)
{-# DEPRECATED eEndpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead." #-}

-- | Value returned by a call to CreateEndpoint that can be used for cross-account validation. Use it on a subsequent call to CreateEndpoint to create the endpoint with a cross-account.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExternalId :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eExternalId = Lens.lens (externalId :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: Endpoint)
{-# DEPRECATED eExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The settings for the DynamoDB target endpoint. For more information, see the @DynamoDBSettings@ structure.
--
-- /Note:/ Consider using 'dynamoDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDynamoDBSettings :: Lens.Lens' Endpoint (Lude.Maybe DynamoDBSettings)
eDynamoDBSettings = Lens.lens (dynamoDBSettings :: Endpoint -> Lude.Maybe DynamoDBSettings) (\s a -> s {dynamoDBSettings = a} :: Endpoint)
{-# DEPRECATED eDynamoDBSettings "Use generic-lens or generic-optics with 'dynamoDBSettings' instead." #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndpointARN :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eEndpointARN = Lens.lens (endpointARN :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: Endpoint)
{-# DEPRECATED eEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The port value used to access the endpoint.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Lude.Maybe Lude.Int)
ePort = Lens.lens (port :: Endpoint -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: Endpoint)
{-# DEPRECATED ePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON Endpoint where
  parseJSON =
    Lude.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "DmsTransferSettings")
            Lude.<*> (x Lude..:? "MySQLSettings")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "MicrosoftSQLServerSettings")
            Lude.<*> (x Lude..:? "CertificateArn")
            Lude.<*> (x Lude..:? "ServiceAccessRoleArn")
            Lude.<*> (x Lude..:? "DocDbSettings")
            Lude.<*> (x Lude..:? "EngineDisplayName")
            Lude.<*> (x Lude..:? "PostgreSQLSettings")
            Lude.<*> (x Lude..:? "ExtraConnectionAttributes")
            Lude.<*> (x Lude..:? "KafkaSettings")
            Lude.<*> (x Lude..:? "OracleSettings")
            Lude.<*> (x Lude..:? "EndpointType")
            Lude.<*> (x Lude..:? "RedshiftSettings")
            Lude.<*> (x Lude..:? "ElasticsearchSettings")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "ExternalTableDefinition")
            Lude.<*> (x Lude..:? "EngineName")
            Lude.<*> (x Lude..:? "NeptuneSettings")
            Lude.<*> (x Lude..:? "IBMDb2Settings")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "MongoDbSettings")
            Lude.<*> (x Lude..:? "SslMode")
            Lude.<*> (x Lude..:? "SybaseSettings")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "S3Settings")
            Lude.<*> (x Lude..:? "KinesisSettings")
            Lude.<*> (x Lude..:? "EndpointIdentifier")
            Lude.<*> (x Lude..:? "ExternalId")
            Lude.<*> (x Lude..:? "DynamoDbSettings")
            Lude.<*> (x Lude..:? "EndpointArn")
            Lude.<*> (x Lude..:? "Port")
      )
