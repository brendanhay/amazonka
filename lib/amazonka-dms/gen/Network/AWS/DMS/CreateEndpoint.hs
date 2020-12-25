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
    ceEndpointIdentifier,
    ceEndpointType,
    ceEngineName,
    ceCertificateArn,
    ceDatabaseName,
    ceDmsTransferSettings,
    ceDocDbSettings,
    ceDynamoDbSettings,
    ceElasticsearchSettings,
    ceExternalTableDefinition,
    ceExtraConnectionAttributes,
    ceIBMDb2Settings,
    ceKafkaSettings,
    ceKinesisSettings,
    ceKmsKeyId,
    ceMicrosoftSQLServerSettings,
    ceMongoDbSettings,
    ceMySQLSettings,
    ceNeptuneSettings,
    ceOracleSettings,
    cePassword,
    cePort,
    cePostgreSQLSettings,
    ceRedshiftSettings,
    ceResourceIdentifier,
    ceS3Settings,
    ceServerName,
    ceServiceAccessRoleArn,
    ceSslMode,
    ceSybaseSettings,
    ceTags,
    ceUsername,

    -- * Destructuring the response
    CreateEndpointResponse (..),
    mkCreateEndpointResponse,

    -- ** Response lenses
    cerrsEndpoint,
    cerrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
    endpointIdentifier :: Types.EndpointIdentifier,
    -- | The type of endpoint. Valid values are @source@ and @target@ .
    endpointType :: Types.ReplicationEndpointTypeValue,
    -- | The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
    engineName :: Types.EngineName,
    -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Core.Maybe Types.CertificateArn,
    -- | The name of the endpoint database.
    databaseName :: Core.Maybe Types.DatabaseName,
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
    dmsTransferSettings :: Core.Maybe Types.DmsTransferSettings,
    docDbSettings :: Core.Maybe Types.DocDbSettings,
    -- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
    dynamoDbSettings :: Core.Maybe Types.DynamoDbSettings,
    -- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
    elasticsearchSettings :: Core.Maybe Types.ElasticsearchSettings,
    -- | The external table definition.
    externalTableDefinition :: Core.Maybe Types.ExternalTableDefinition,
    -- | Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./
    extraConnectionAttributes :: Core.Maybe Types.ExtraConnectionAttributes,
    -- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
    iBMDb2Settings :: Core.Maybe Types.IBMDb2Settings,
    -- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
    kafkaSettings :: Core.Maybe Types.KafkaSettings,
    -- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
    kinesisSettings :: Core.Maybe Types.KinesisSettings,
    -- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
    --
    -- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
    -- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    microsoftSQLServerSettings :: Core.Maybe Types.MicrosoftSQLServerSettings,
    -- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
    mongoDbSettings :: Core.Maybe Types.MongoDbSettings,
    -- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    mySQLSettings :: Core.Maybe Types.MySQLSettings,
    -- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
    neptuneSettings :: Core.Maybe Types.NeptuneSettings,
    -- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    oracleSettings :: Core.Maybe Types.OracleSettings,
    -- | The password to be used to log in to the endpoint database.
    password :: Core.Maybe Types.Password,
    -- | The port used by the endpoint database.
    port :: Core.Maybe Core.Int,
    -- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    postgreSQLSettings :: Core.Maybe Types.PostgreSQLSettings,
    redshiftSettings :: Core.Maybe Types.RedshiftSettings,
    -- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
    resourceIdentifier :: Core.Maybe Types.ResourceIdentifier,
    -- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
    s3Settings :: Core.Maybe Types.S3Settings,
    -- | The name of the server where the endpoint database resides.
    serverName :: Core.Maybe Types.ServerName,
    -- | The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint.
    serviceAccessRoleArn :: Core.Maybe Types.ServiceAccessRoleArn,
    -- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@
    sslMode :: Core.Maybe Types.DmsSslModeValue,
    -- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
    sybaseSettings :: Core.Maybe Types.SybaseSettings,
    -- | One or more tags to be assigned to the endpoint.
    tags :: Core.Maybe [Types.Tag],
    -- | The user name to be used to log in to the endpoint database.
    username :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpoint' value with any optional fields omitted.
mkCreateEndpoint ::
  -- | 'endpointIdentifier'
  Types.EndpointIdentifier ->
  -- | 'endpointType'
  Types.ReplicationEndpointTypeValue ->
  -- | 'engineName'
  Types.EngineName ->
  CreateEndpoint
mkCreateEndpoint endpointIdentifier endpointType engineName =
  CreateEndpoint'
    { endpointIdentifier,
      endpointType,
      engineName,
      certificateArn = Core.Nothing,
      databaseName = Core.Nothing,
      dmsTransferSettings = Core.Nothing,
      docDbSettings = Core.Nothing,
      dynamoDbSettings = Core.Nothing,
      elasticsearchSettings = Core.Nothing,
      externalTableDefinition = Core.Nothing,
      extraConnectionAttributes = Core.Nothing,
      iBMDb2Settings = Core.Nothing,
      kafkaSettings = Core.Nothing,
      kinesisSettings = Core.Nothing,
      kmsKeyId = Core.Nothing,
      microsoftSQLServerSettings = Core.Nothing,
      mongoDbSettings = Core.Nothing,
      mySQLSettings = Core.Nothing,
      neptuneSettings = Core.Nothing,
      oracleSettings = Core.Nothing,
      password = Core.Nothing,
      port = Core.Nothing,
      postgreSQLSettings = Core.Nothing,
      redshiftSettings = Core.Nothing,
      resourceIdentifier = Core.Nothing,
      s3Settings = Core.Nothing,
      serverName = Core.Nothing,
      serviceAccessRoleArn = Core.Nothing,
      sslMode = Core.Nothing,
      sybaseSettings = Core.Nothing,
      tags = Core.Nothing,
      username = Core.Nothing
    }

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'endpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointIdentifier :: Lens.Lens' CreateEndpoint Types.EndpointIdentifier
ceEndpointIdentifier = Lens.field @"endpointIdentifier"
{-# DEPRECATED ceEndpointIdentifier "Use generic-lens or generic-optics with 'endpointIdentifier' instead." #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointType :: Lens.Lens' CreateEndpoint Types.ReplicationEndpointTypeValue
ceEndpointType = Lens.field @"endpointType"
{-# DEPRECATED ceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEngineName :: Lens.Lens' CreateEndpoint Types.EngineName
ceEngineName = Lens.field @"engineName"
{-# DEPRECATED ceEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceCertificateArn :: Lens.Lens' CreateEndpoint (Core.Maybe Types.CertificateArn)
ceCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED ceCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The name of the endpoint database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDatabaseName :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DatabaseName)
ceDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED ceDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

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
{-# DEPRECATED ceDmsTransferSettings "Use generic-lens or generic-optics with 'dmsTransferSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'docDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDocDbSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DocDbSettings)
ceDocDbSettings = Lens.field @"docDbSettings"
{-# DEPRECATED ceDocDbSettings "Use generic-lens or generic-optics with 'docDbSettings' instead." #-}

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'dynamoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDynamoDbSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DynamoDbSettings)
ceDynamoDbSettings = Lens.field @"dynamoDbSettings"
{-# DEPRECATED ceDynamoDbSettings "Use generic-lens or generic-optics with 'dynamoDbSettings' instead." #-}

-- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- /Note:/ Consider using 'elasticsearchSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceElasticsearchSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ElasticsearchSettings)
ceElasticsearchSettings = Lens.field @"elasticsearchSettings"
{-# DEPRECATED ceElasticsearchSettings "Use generic-lens or generic-optics with 'elasticsearchSettings' instead." #-}

-- | The external table definition.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceExternalTableDefinition :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ExternalTableDefinition)
ceExternalTableDefinition = Lens.field @"externalTableDefinition"
{-# DEPRECATED ceExternalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead." #-}

-- | Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'extraConnectionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceExtraConnectionAttributes :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ExtraConnectionAttributes)
ceExtraConnectionAttributes = Lens.field @"extraConnectionAttributes"
{-# DEPRECATED ceExtraConnectionAttributes "Use generic-lens or generic-optics with 'extraConnectionAttributes' instead." #-}

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'iBMDb2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceIBMDb2Settings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.IBMDb2Settings)
ceIBMDb2Settings = Lens.field @"iBMDb2Settings"
{-# DEPRECATED ceIBMDb2Settings "Use generic-lens or generic-optics with 'iBMDb2Settings' instead." #-}

-- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'kafkaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKafkaSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.KafkaSettings)
ceKafkaSettings = Lens.field @"kafkaSettings"
{-# DEPRECATED ceKafkaSettings "Use generic-lens or generic-optics with 'kafkaSettings' instead." #-}

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'kinesisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKinesisSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.KinesisSettings)
ceKinesisSettings = Lens.field @"kinesisSettings"
{-# DEPRECATED ceKinesisSettings "Use generic-lens or generic-optics with 'kinesisSettings' instead." #-}

-- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKmsKeyId :: Lens.Lens' CreateEndpoint (Core.Maybe Types.KmsKeyId)
ceKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED ceKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'microsoftSQLServerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMicrosoftSQLServerSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.MicrosoftSQLServerSettings)
ceMicrosoftSQLServerSettings = Lens.field @"microsoftSQLServerSettings"
{-# DEPRECATED ceMicrosoftSQLServerSettings "Use generic-lens or generic-optics with 'microsoftSQLServerSettings' instead." #-}

-- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'mongoDbSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMongoDbSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.MongoDbSettings)
ceMongoDbSettings = Lens.field @"mongoDbSettings"
{-# DEPRECATED ceMongoDbSettings "Use generic-lens or generic-optics with 'mongoDbSettings' instead." #-}

-- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'mySQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMySQLSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.MySQLSettings)
ceMySQLSettings = Lens.field @"mySQLSettings"
{-# DEPRECATED ceMySQLSettings "Use generic-lens or generic-optics with 'mySQLSettings' instead." #-}

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'neptuneSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceNeptuneSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.NeptuneSettings)
ceNeptuneSettings = Lens.field @"neptuneSettings"
{-# DEPRECATED ceNeptuneSettings "Use generic-lens or generic-optics with 'neptuneSettings' instead." #-}

-- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'oracleSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceOracleSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.OracleSettings)
ceOracleSettings = Lens.field @"oracleSettings"
{-# DEPRECATED ceOracleSettings "Use generic-lens or generic-optics with 'oracleSettings' instead." #-}

-- | The password to be used to log in to the endpoint database.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePassword :: Lens.Lens' CreateEndpoint (Core.Maybe Types.Password)
cePassword = Lens.field @"password"
{-# DEPRECATED cePassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The port used by the endpoint database.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePort :: Lens.Lens' CreateEndpoint (Core.Maybe Core.Int)
cePort = Lens.field @"port"
{-# DEPRECATED cePort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'postgreSQLSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cePostgreSQLSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.PostgreSQLSettings)
cePostgreSQLSettings = Lens.field @"postgreSQLSettings"
{-# DEPRECATED cePostgreSQLSettings "Use generic-lens or generic-optics with 'postgreSQLSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceRedshiftSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.RedshiftSettings)
ceRedshiftSettings = Lens.field @"redshiftSettings"
{-# DEPRECATED ceRedshiftSettings "Use generic-lens or generic-optics with 'redshiftSettings' instead." #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceResourceIdentifier :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ResourceIdentifier)
ceResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED ceResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceS3Settings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.S3Settings)
ceS3Settings = Lens.field @"s3Settings"
{-# DEPRECATED ceS3Settings "Use generic-lens or generic-optics with 's3Settings' instead." #-}

-- | The name of the server where the endpoint database resides.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceServerName :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ServerName)
ceServerName = Lens.field @"serverName"
{-# DEPRECATED ceServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint.
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceServiceAccessRoleArn :: Lens.Lens' CreateEndpoint (Core.Maybe Types.ServiceAccessRoleArn)
ceServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# DEPRECATED ceServiceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead." #-}

-- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@
--
-- /Note:/ Consider using 'sslMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSslMode :: Lens.Lens' CreateEndpoint (Core.Maybe Types.DmsSslModeValue)
ceSslMode = Lens.field @"sslMode"
{-# DEPRECATED ceSslMode "Use generic-lens or generic-optics with 'sslMode' instead." #-}

-- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'sybaseSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSybaseSettings :: Lens.Lens' CreateEndpoint (Core.Maybe Types.SybaseSettings)
ceSybaseSettings = Lens.field @"sybaseSettings"
{-# DEPRECATED ceSybaseSettings "Use generic-lens or generic-optics with 'sybaseSettings' instead." #-}

-- | One or more tags to be assigned to the endpoint.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Core.Maybe [Types.Tag])
ceTags = Lens.field @"tags"
{-# DEPRECATED ceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The user name to be used to log in to the endpoint database.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceUsername :: Lens.Lens' CreateEndpoint (Core.Maybe Types.Username)
ceUsername = Lens.field @"username"
{-# DEPRECATED ceUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON CreateEndpoint where
  toJSON CreateEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointIdentifier" Core..= endpointIdentifier),
            Core.Just ("EndpointType" Core..= endpointType),
            Core.Just ("EngineName" Core..= engineName),
            ("CertificateArn" Core..=) Core.<$> certificateArn,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            ("DmsTransferSettings" Core..=) Core.<$> dmsTransferSettings,
            ("DocDbSettings" Core..=) Core.<$> docDbSettings,
            ("DynamoDbSettings" Core..=) Core.<$> dynamoDbSettings,
            ("ElasticsearchSettings" Core..=) Core.<$> elasticsearchSettings,
            ("ExternalTableDefinition" Core..=)
              Core.<$> externalTableDefinition,
            ("ExtraConnectionAttributes" Core..=)
              Core.<$> extraConnectionAttributes,
            ("IBMDb2Settings" Core..=) Core.<$> iBMDb2Settings,
            ("KafkaSettings" Core..=) Core.<$> kafkaSettings,
            ("KinesisSettings" Core..=) Core.<$> kinesisSettings,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("MicrosoftSQLServerSettings" Core..=)
              Core.<$> microsoftSQLServerSettings,
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
            ("Username" Core..=) Core.<$> username
          ]
      )

instance Core.AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDMSv20160101.CreateEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Core.<$> (x Core..:? "Endpoint") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The endpoint that was created.
    endpoint :: Core.Maybe Types.Endpoint,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpointResponse' value with any optional fields omitted.
mkCreateEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateEndpointResponse
mkCreateEndpointResponse responseStatus =
  CreateEndpointResponse' {endpoint = Core.Nothing, responseStatus}

-- | The endpoint that was created.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsEndpoint :: Lens.Lens' CreateEndpointResponse (Core.Maybe Types.Endpoint)
cerrsEndpoint = Lens.field @"endpoint"
{-# DEPRECATED cerrsEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsResponseStatus :: Lens.Lens' CreateEndpointResponse Core.Int
cerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
