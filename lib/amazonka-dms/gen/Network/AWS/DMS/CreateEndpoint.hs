{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateEndpoint (..),
    mkCreateEndpoint,

    -- ** Request lenses
    ceDmsTransferSettings,
    ceMySQLSettings,
    ceServerName,
    ceMicrosoftSQLServerSettings,
    ceCertificateARN,
    ceServiceAccessRoleARN,
    ceDocDBSettings,
    cePostgreSQLSettings,
    ceExtraConnectionAttributes,
    ceKafkaSettings,
    ceOracleSettings,
    ceEndpointType,
    ceRedshiftSettings,
    ceElasticsearchSettings,
    ceUsername,
    ceExternalTableDefinition,
    ceEngineName,
    ceNeptuneSettings,
    ceIBMDB2Settings,
    ceKMSKeyId,
    ceMongoDBSettings,
    ceSSLMode,
    cePassword,
    ceSybaseSettings,
    ceDatabaseName,
    ceS3Settings,
    ceKinesisSettings,
    ceEndpointIdentifier,
    ceDynamoDBSettings,
    ceResourceIdentifier,
    ceTags,
    cePort,

    -- * Destructuring the response
    CreateEndpointResponse (..),
    mkCreateEndpointResponse,

    -- ** Response lenses
    cersEndpoint,
    cersResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | The settings in JSON format for the DMS transfer type of source endpoint.
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
    dmsTransferSettings :: Lude.Maybe DmsTransferSettings,
    -- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    mySQLSettings :: Lude.Maybe MySQLSettings,
    -- | The name of the server where the endpoint database resides.
    serverName :: Lude.Maybe Lude.Text,
    -- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    microsoftSQLServerSettings :: Lude.Maybe MicrosoftSQLServerSettings,
    -- | The Amazon Resource Name (ARN) for the certificate.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint.
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    docDBSettings :: Lude.Maybe DocDBSettings,
    -- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    postgreSQLSettings :: Lude.Maybe PostgreSQLSettings,
    -- | Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./
    extraConnectionAttributes :: Lude.Maybe Lude.Text,
    -- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
    kafkaSettings :: Lude.Maybe KafkaSettings,
    -- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    oracleSettings :: Lude.Maybe OracleSettings,
    -- | The type of endpoint. Valid values are @source@ and @target@ .
    endpointType :: ReplicationEndpointTypeValue,
    redshiftSettings :: Lude.Maybe RedshiftSettings,
    -- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
    elasticsearchSettings :: Lude.Maybe ElasticsearchSettings,
    -- | The user name to be used to log in to the endpoint database.
    username :: Lude.Maybe Lude.Text,
    -- | The external table definition.
    externalTableDefinition :: Lude.Maybe Lude.Text,
    -- | The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
    engineName :: Lude.Text,
    -- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
    neptuneSettings :: Lude.Maybe NeptuneSettings,
    -- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
    iBMDB2Settings :: Lude.Maybe IBMDB2Settings,
    -- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
    --
    -- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
    -- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
    mongoDBSettings :: Lude.Maybe MongoDBSettings,
    -- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@
    sslMode :: Lude.Maybe DmsSSLModeValue,
    -- | The password to be used to log in to the endpoint database.
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    sybaseSettings :: Lude.Maybe SybaseSettings,
    -- | The name of the endpoint database.
    databaseName :: Lude.Maybe Lude.Text,
    -- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
    s3Settings :: Lude.Maybe S3Settings,
    -- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
    kinesisSettings :: Lude.Maybe KinesisSettings,
    -- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
    endpointIdentifier :: Lude.Text,
    -- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
    dynamoDBSettings :: Lude.Maybe DynamoDBSettings,
    -- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
    resourceIdentifier :: Lude.Maybe Lude.Text,
    -- | One or more tags to be assigned to the endpoint.
    tags :: Lude.Maybe [Tag],
    -- | The port used by the endpoint database.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
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
-- * 'mySQLSettings' - Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'serverName' - The name of the server where the endpoint database resides.
-- * 'microsoftSQLServerSettings' - Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'certificateARN' - The Amazon Resource Name (ARN) for the certificate.
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint.
-- * 'docDBSettings' -
-- * 'postgreSQLSettings' - Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'extraConnectionAttributes' - Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./
-- * 'kafkaSettings' - Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
-- * 'oracleSettings' - Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'endpointType' - The type of endpoint. Valid values are @source@ and @target@ .
-- * 'redshiftSettings' -
-- * 'elasticsearchSettings' - Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
-- * 'username' - The user name to be used to log in to the endpoint database.
-- * 'externalTableDefinition' - The external table definition.
-- * 'engineName' - The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
-- * 'neptuneSettings' - Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
-- * 'iBMDB2Settings' - Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'kmsKeyId' - An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'mongoDBSettings' - Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
-- * 'sslMode' - The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@
-- * 'password' - The password to be used to log in to the endpoint database.
-- * 'sybaseSettings' - Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'databaseName' - The name of the endpoint database.
-- * 's3Settings' - Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
-- * 'kinesisSettings' - Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
-- * 'endpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
-- * 'dynamoDBSettings' - Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
-- * 'resourceIdentifier' - A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
-- * 'tags' - One or more tags to be assigned to the endpoint.
-- * 'port' - The port used by the endpoint database.
mkCreateEndpoint ::
  -- | 'endpointType'
  ReplicationEndpointTypeValue ->
  -- | 'engineName'
  Lude.Text ->
  -- | 'endpointIdentifier'
  Lude.Text ->
  CreateEndpoint
mkCreateEndpoint pEndpointType_ pEngineName_ pEndpointIdentifier_ =
  CreateEndpoint'
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
      endpointType = pEndpointType_,
      redshiftSettings = Lude.Nothing,
      elasticsearchSettings = Lude.Nothing,
      username = Lude.Nothing,
      externalTableDefinition = Lude.Nothing,
      engineName = pEngineName_,
      neptuneSettings = Lude.Nothing,
      iBMDB2Settings = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      mongoDBSettings = Lude.Nothing,
      sslMode = Lude.Nothing,
      password = Lude.Nothing,
      sybaseSettings = Lude.Nothing,
      databaseName = Lude.Nothing,
      s3Settings = Lude.Nothing,
      kinesisSettings = Lude.Nothing,
      endpointIdentifier = pEndpointIdentifier_,
      dynamoDBSettings = Lude.Nothing,
      resourceIdentifier = Lude.Nothing,
      tags = Lude.Nothing,
      port = Lude.Nothing
    }

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
ceDmsTransferSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe DmsTransferSettings)
ceDmsTransferSettings = Lens.lens (dmsTransferSettings :: CreateEndpoint -> Lude.Maybe DmsTransferSettings) (\s a -> s {dmsTransferSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceDmsTransferSettings "Use generic-lens or generic-optics with 'dmsTransferSettings' instead." #-}

-- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'mySQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMySQLSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe MySQLSettings)
ceMySQLSettings = Lens.lens (mySQLSettings :: CreateEndpoint -> Lude.Maybe MySQLSettings) (\s a -> s {mySQLSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceMySQLSettings "Use generic-lens or generic-optics with 'mySQLSettings' instead." #-}

-- | The name of the server where the endpoint database resides.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceServerName :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceServerName = Lens.lens (serverName :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: CreateEndpoint)
{-# DEPRECATED ceServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'microsoftSQLServerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMicrosoftSQLServerSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe MicrosoftSQLServerSettings)
ceMicrosoftSQLServerSettings = Lens.lens (microsoftSQLServerSettings :: CreateEndpoint -> Lude.Maybe MicrosoftSQLServerSettings) (\s a -> s {microsoftSQLServerSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceMicrosoftSQLServerSettings "Use generic-lens or generic-optics with 'microsoftSQLServerSettings' instead." #-}

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceCertificateARN :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceCertificateARN = Lens.lens (certificateARN :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CreateEndpoint)
{-# DEPRECATED ceCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceServiceAccessRoleARN :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: CreateEndpoint)
{-# DEPRECATED ceServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'docDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDocDBSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe DocDBSettings)
ceDocDBSettings = Lens.lens (docDBSettings :: CreateEndpoint -> Lude.Maybe DocDBSettings) (\s a -> s {docDBSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceDocDBSettings "Use generic-lens or generic-optics with 'docDBSettings' instead." #-}

-- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'postgreSQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePostgreSQLSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe PostgreSQLSettings)
cePostgreSQLSettings = Lens.lens (postgreSQLSettings :: CreateEndpoint -> Lude.Maybe PostgreSQLSettings) (\s a -> s {postgreSQLSettings = a} :: CreateEndpoint)
{-# DEPRECATED cePostgreSQLSettings "Use generic-lens or generic-optics with 'postgreSQLSettings' instead." #-}

-- | Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'extraConnectionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceExtraConnectionAttributes :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceExtraConnectionAttributes = Lens.lens (extraConnectionAttributes :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {extraConnectionAttributes = a} :: CreateEndpoint)
{-# DEPRECATED ceExtraConnectionAttributes "Use generic-lens or generic-optics with 'extraConnectionAttributes' instead." #-}

-- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'kafkaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKafkaSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe KafkaSettings)
ceKafkaSettings = Lens.lens (kafkaSettings :: CreateEndpoint -> Lude.Maybe KafkaSettings) (\s a -> s {kafkaSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceKafkaSettings "Use generic-lens or generic-optics with 'kafkaSettings' instead." #-}

-- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'oracleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceOracleSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe OracleSettings)
ceOracleSettings = Lens.lens (oracleSettings :: CreateEndpoint -> Lude.Maybe OracleSettings) (\s a -> s {oracleSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceOracleSettings "Use generic-lens or generic-optics with 'oracleSettings' instead." #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointType :: Lens.Lens' CreateEndpoint ReplicationEndpointTypeValue
ceEndpointType = Lens.lens (endpointType :: CreateEndpoint -> ReplicationEndpointTypeValue) (\s a -> s {endpointType = a} :: CreateEndpoint)
{-# DEPRECATED ceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceRedshiftSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe RedshiftSettings)
ceRedshiftSettings = Lens.lens (redshiftSettings :: CreateEndpoint -> Lude.Maybe RedshiftSettings) (\s a -> s {redshiftSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceRedshiftSettings "Use generic-lens or generic-optics with 'redshiftSettings' instead." #-}

-- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- /Note:/ Consider using 'elasticsearchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceElasticsearchSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe ElasticsearchSettings)
ceElasticsearchSettings = Lens.lens (elasticsearchSettings :: CreateEndpoint -> Lude.Maybe ElasticsearchSettings) (\s a -> s {elasticsearchSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceElasticsearchSettings "Use generic-lens or generic-optics with 'elasticsearchSettings' instead." #-}

-- | The user name to be used to log in to the endpoint database.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceUsername :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceUsername = Lens.lens (username :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: CreateEndpoint)
{-# DEPRECATED ceUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The external table definition.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceExternalTableDefinition :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceExternalTableDefinition = Lens.lens (externalTableDefinition :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {externalTableDefinition = a} :: CreateEndpoint)
{-# DEPRECATED ceExternalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead." #-}

-- | The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEngineName :: Lens.Lens' CreateEndpoint Lude.Text
ceEngineName = Lens.lens (engineName :: CreateEndpoint -> Lude.Text) (\s a -> s {engineName = a} :: CreateEndpoint)
{-# DEPRECATED ceEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'neptuneSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceNeptuneSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe NeptuneSettings)
ceNeptuneSettings = Lens.lens (neptuneSettings :: CreateEndpoint -> Lude.Maybe NeptuneSettings) (\s a -> s {neptuneSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceNeptuneSettings "Use generic-lens or generic-optics with 'neptuneSettings' instead." #-}

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'iBMDB2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceIBMDB2Settings :: Lens.Lens' CreateEndpoint (Lude.Maybe IBMDB2Settings)
ceIBMDB2Settings = Lens.lens (iBMDB2Settings :: CreateEndpoint -> Lude.Maybe IBMDB2Settings) (\s a -> s {iBMDB2Settings = a} :: CreateEndpoint)
{-# DEPRECATED ceIBMDB2Settings "Use generic-lens or generic-optics with 'iBMDB2Settings' instead." #-}

-- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKMSKeyId :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceKMSKeyId = Lens.lens (kmsKeyId :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateEndpoint)
{-# DEPRECATED ceKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'mongoDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMongoDBSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe MongoDBSettings)
ceMongoDBSettings = Lens.lens (mongoDBSettings :: CreateEndpoint -> Lude.Maybe MongoDBSettings) (\s a -> s {mongoDBSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceMongoDBSettings "Use generic-lens or generic-optics with 'mongoDBSettings' instead." #-}

-- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@
--
-- /Note:/ Consider using 'sslMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSSLMode :: Lens.Lens' CreateEndpoint (Lude.Maybe DmsSSLModeValue)
ceSSLMode = Lens.lens (sslMode :: CreateEndpoint -> Lude.Maybe DmsSSLModeValue) (\s a -> s {sslMode = a} :: CreateEndpoint)
{-# DEPRECATED ceSSLMode "Use generic-lens or generic-optics with 'sslMode' instead." #-}

-- | The password to be used to log in to the endpoint database.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePassword :: Lens.Lens' CreateEndpoint (Lude.Maybe (Lude.Sensitive Lude.Text))
cePassword = Lens.lens (password :: CreateEndpoint -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: CreateEndpoint)
{-# DEPRECATED cePassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'sybaseSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSybaseSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe SybaseSettings)
ceSybaseSettings = Lens.lens (sybaseSettings :: CreateEndpoint -> Lude.Maybe SybaseSettings) (\s a -> s {sybaseSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceSybaseSettings "Use generic-lens or generic-optics with 'sybaseSettings' instead." #-}

-- | The name of the endpoint database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDatabaseName :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceDatabaseName = Lens.lens (databaseName :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: CreateEndpoint)
{-# DEPRECATED ceDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceS3Settings :: Lens.Lens' CreateEndpoint (Lude.Maybe S3Settings)
ceS3Settings = Lens.lens (s3Settings :: CreateEndpoint -> Lude.Maybe S3Settings) (\s a -> s {s3Settings = a} :: CreateEndpoint)
{-# DEPRECATED ceS3Settings "Use generic-lens or generic-optics with 's3Settings' instead." #-}

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'kinesisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKinesisSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe KinesisSettings)
ceKinesisSettings = Lens.lens (kinesisSettings :: CreateEndpoint -> Lude.Maybe KinesisSettings) (\s a -> s {kinesisSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceKinesisSettings "Use generic-lens or generic-optics with 'kinesisSettings' instead." #-}

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointIdentifier :: Lens.Lens' CreateEndpoint Lude.Text
ceEndpointIdentifier = Lens.lens (endpointIdentifier :: CreateEndpoint -> Lude.Text) (\s a -> s {endpointIdentifier = a} :: CreateEndpoint)
{-# DEPRECATED ceEndpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead." #-}

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'dynamoDBSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDynamoDBSettings :: Lens.Lens' CreateEndpoint (Lude.Maybe DynamoDBSettings)
ceDynamoDBSettings = Lens.lens (dynamoDBSettings :: CreateEndpoint -> Lude.Maybe DynamoDBSettings) (\s a -> s {dynamoDBSettings = a} :: CreateEndpoint)
{-# DEPRECATED ceDynamoDBSettings "Use generic-lens or generic-optics with 'dynamoDBSettings' instead." #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceResourceIdentifier :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Text)
ceResourceIdentifier = Lens.lens (resourceIdentifier :: CreateEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {resourceIdentifier = a} :: CreateEndpoint)
{-# DEPRECATED ceResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | One or more tags to be assigned to the endpoint.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Lude.Maybe [Tag])
ceTags = Lens.lens (tags :: CreateEndpoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEndpoint)
{-# DEPRECATED ceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port used by the endpoint database.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePort :: Lens.Lens' CreateEndpoint (Lude.Maybe Lude.Int)
cePort = Lens.lens (port :: CreateEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CreateEndpoint)
{-# DEPRECATED cePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Lude.<$> (x Lude..?> "Endpoint") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.CreateEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
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
            Lude.Just ("EndpointType" Lude..= endpointType),
            ("RedshiftSettings" Lude..=) Lude.<$> redshiftSettings,
            ("ElasticsearchSettings" Lude..=) Lude.<$> elasticsearchSettings,
            ("Username" Lude..=) Lude.<$> username,
            ("ExternalTableDefinition" Lude..=)
              Lude.<$> externalTableDefinition,
            Lude.Just ("EngineName" Lude..= engineName),
            ("NeptuneSettings" Lude..=) Lude.<$> neptuneSettings,
            ("IBMDb2Settings" Lude..=) Lude.<$> iBMDB2Settings,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("MongoDbSettings" Lude..=) Lude.<$> mongoDBSettings,
            ("SslMode" Lude..=) Lude.<$> sslMode,
            ("Password" Lude..=) Lude.<$> password,
            ("SybaseSettings" Lude..=) Lude.<$> sybaseSettings,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("S3Settings" Lude..=) Lude.<$> s3Settings,
            ("KinesisSettings" Lude..=) Lude.<$> kinesisSettings,
            Lude.Just ("EndpointIdentifier" Lude..= endpointIdentifier),
            ("DynamoDbSettings" Lude..=) Lude.<$> dynamoDBSettings,
            ("ResourceIdentifier" Lude..=) Lude.<$> resourceIdentifier,
            ("Tags" Lude..=) Lude.<$> tags,
            ("Port" Lude..=) Lude.<$> port
          ]
      )

instance Lude.ToPath CreateEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEndpoint where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The endpoint that was created.
    endpoint :: Lude.Maybe Endpoint,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpoint' - The endpoint that was created.
-- * 'responseStatus' - The response status code.
mkCreateEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEndpointResponse
mkCreateEndpointResponse pResponseStatus_ =
  CreateEndpointResponse'
    { endpoint = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The endpoint that was created.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cersEndpoint :: Lens.Lens' CreateEndpointResponse (Lude.Maybe Endpoint)
cersEndpoint = Lens.lens (endpoint :: CreateEndpointResponse -> Lude.Maybe Endpoint) (\s a -> s {endpoint = a} :: CreateEndpointResponse)
{-# DEPRECATED cersEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cersResponseStatus :: Lens.Lens' CreateEndpointResponse Lude.Int
cersResponseStatus = Lens.lens (responseStatus :: CreateEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEndpointResponse)
{-# DEPRECATED cersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
