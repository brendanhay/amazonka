{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    createEndpoint,
    CreateEndpoint,

    -- * Request Lenses
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
    ceRedshiftSettings,
    ceElasticsearchSettings,
    ceUsername,
    ceExternalTableDefinition,
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
    ceDynamoDBSettings,
    ceResourceIdentifier,
    ceTags,
    cePort,
    ceEndpointIdentifier,
    ceEndpointType,
    ceEngineName,

    -- * Destructuring the Response
    createEndpointResponse,
    CreateEndpointResponse,

    -- * Response Lenses
    cersEndpoint,
    cersResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { _ceDmsTransferSettings ::
      !(Maybe DmsTransferSettings),
    _ceMySQLSettings :: !(Maybe MySQLSettings),
    _ceServerName :: !(Maybe Text),
    _ceMicrosoftSQLServerSettings ::
      !(Maybe MicrosoftSQLServerSettings),
    _ceCertificateARN :: !(Maybe Text),
    _ceServiceAccessRoleARN :: !(Maybe Text),
    _ceDocDBSettings :: !(Maybe DocDBSettings),
    _cePostgreSQLSettings :: !(Maybe PostgreSQLSettings),
    _ceExtraConnectionAttributes :: !(Maybe Text),
    _ceKafkaSettings :: !(Maybe KafkaSettings),
    _ceOracleSettings :: !(Maybe OracleSettings),
    _ceRedshiftSettings :: !(Maybe RedshiftSettings),
    _ceElasticsearchSettings :: !(Maybe ElasticsearchSettings),
    _ceUsername :: !(Maybe Text),
    _ceExternalTableDefinition :: !(Maybe Text),
    _ceNeptuneSettings :: !(Maybe NeptuneSettings),
    _ceIBMDB2Settings :: !(Maybe IBMDB2Settings),
    _ceKMSKeyId :: !(Maybe Text),
    _ceMongoDBSettings :: !(Maybe MongoDBSettings),
    _ceSSLMode :: !(Maybe DmsSSLModeValue),
    _cePassword :: !(Maybe (Sensitive Text)),
    _ceSybaseSettings :: !(Maybe SybaseSettings),
    _ceDatabaseName :: !(Maybe Text),
    _ceS3Settings :: !(Maybe S3Settings),
    _ceKinesisSettings :: !(Maybe KinesisSettings),
    _ceDynamoDBSettings :: !(Maybe DynamoDBSettings),
    _ceResourceIdentifier :: !(Maybe Text),
    _ceTags :: !(Maybe [Tag]),
    _cePort :: !(Maybe Int),
    _ceEndpointIdentifier :: !Text,
    _ceEndpointType :: !ReplicationEndpointTypeValue,
    _ceEngineName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceDmsTransferSettings' - The settings in JSON format for the DMS transfer type of source endpoint.  Possible settings include the following:     * @ServiceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.     * @BucketName@ - The name of the S3 bucket to use.     * @CompressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value. Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@  JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @
--
-- * 'ceMySQLSettings' - Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- * 'ceServerName' - The name of the server where the endpoint database resides.
--
-- * 'ceMicrosoftSQLServerSettings' - Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- * 'ceCertificateARN' - The Amazon Resource Name (ARN) for the certificate.
--
-- * 'ceServiceAccessRoleARN' - The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint.
--
-- * 'ceDocDBSettings' - Undocumented member.
--
-- * 'cePostgreSQLSettings' - Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- * 'ceExtraConnectionAttributes' - Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./
--
-- * 'ceKafkaSettings' - Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- * 'ceOracleSettings' - Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- * 'ceRedshiftSettings' - Undocumented member.
--
-- * 'ceElasticsearchSettings' - Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- * 'ceUsername' - The user name to be used to log in to the endpoint database.
--
-- * 'ceExternalTableDefinition' - The external table definition.
--
-- * 'ceNeptuneSettings' - Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
--
-- * 'ceIBMDB2Settings' - Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- * 'ceKMSKeyId' - An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'ceMongoDBSettings' - Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- * 'ceSSLMode' - The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@
--
-- * 'cePassword' - The password to be used to log in to the endpoint database.
--
-- * 'ceSybaseSettings' - Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- * 'ceDatabaseName' - The name of the endpoint database.
--
-- * 'ceS3Settings' - Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
--
-- * 'ceKinesisSettings' - Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
--
-- * 'ceDynamoDBSettings' - Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
--
-- * 'ceResourceIdentifier' - A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- * 'ceTags' - One or more tags to be assigned to the endpoint.
--
-- * 'cePort' - The port used by the endpoint database.
--
-- * 'ceEndpointIdentifier' - The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
--
-- * 'ceEndpointType' - The type of endpoint. Valid values are @source@ and @target@ .
--
-- * 'ceEngineName' - The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
createEndpoint ::
  -- | 'ceEndpointIdentifier'
  Text ->
  -- | 'ceEndpointType'
  ReplicationEndpointTypeValue ->
  -- | 'ceEngineName'
  Text ->
  CreateEndpoint
createEndpoint pEndpointIdentifier_ pEndpointType_ pEngineName_ =
  CreateEndpoint'
    { _ceDmsTransferSettings = Nothing,
      _ceMySQLSettings = Nothing,
      _ceServerName = Nothing,
      _ceMicrosoftSQLServerSettings = Nothing,
      _ceCertificateARN = Nothing,
      _ceServiceAccessRoleARN = Nothing,
      _ceDocDBSettings = Nothing,
      _cePostgreSQLSettings = Nothing,
      _ceExtraConnectionAttributes = Nothing,
      _ceKafkaSettings = Nothing,
      _ceOracleSettings = Nothing,
      _ceRedshiftSettings = Nothing,
      _ceElasticsearchSettings = Nothing,
      _ceUsername = Nothing,
      _ceExternalTableDefinition = Nothing,
      _ceNeptuneSettings = Nothing,
      _ceIBMDB2Settings = Nothing,
      _ceKMSKeyId = Nothing,
      _ceMongoDBSettings = Nothing,
      _ceSSLMode = Nothing,
      _cePassword = Nothing,
      _ceSybaseSettings = Nothing,
      _ceDatabaseName = Nothing,
      _ceS3Settings = Nothing,
      _ceKinesisSettings = Nothing,
      _ceDynamoDBSettings = Nothing,
      _ceResourceIdentifier = Nothing,
      _ceTags = Nothing,
      _cePort = Nothing,
      _ceEndpointIdentifier = pEndpointIdentifier_,
      _ceEndpointType = pEndpointType_,
      _ceEngineName = pEngineName_
    }

-- | The settings in JSON format for the DMS transfer type of source endpoint.  Possible settings include the following:     * @ServiceAccessRoleArn@ - The IAM role that has permission to access the Amazon S3 bucket.     * @BucketName@ - The name of the S3 bucket to use.     * @CompressionType@ - An optional parameter to use GZIP to compress the target files. To use GZIP, set this value to @NONE@ (the default). To keep the files uncompressed, don't use this value. Shorthand syntax for these settings is as follows: @ServiceAccessRoleArn=string,BucketName=string,CompressionType=string@  JSON syntax for these settings is as follows: @{ "ServiceAccessRoleArn": "string", "BucketName": "string", "CompressionType": "none"|"gzip" } @
ceDmsTransferSettings :: Lens' CreateEndpoint (Maybe DmsTransferSettings)
ceDmsTransferSettings = lens _ceDmsTransferSettings (\s a -> s {_ceDmsTransferSettings = a})

-- | Settings in JSON format for the source and target MySQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MySQL.html Extra connection attributes when using MySQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.MySQL.html Extra connection attributes when using a MySQL-compatible database as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
ceMySQLSettings :: Lens' CreateEndpoint (Maybe MySQLSettings)
ceMySQLSettings = lens _ceMySQLSettings (\s a -> s {_ceMySQLSettings = a})

-- | The name of the server where the endpoint database resides.
ceServerName :: Lens' CreateEndpoint (Maybe Text)
ceServerName = lens _ceServerName (\s a -> s {_ceServerName = a})

-- | Settings in JSON format for the source and target Microsoft SQL Server endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SQLServer.html Extra connection attributes when using SQL Server as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SQLServer.html Extra connection attributes when using SQL Server as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
ceMicrosoftSQLServerSettings :: Lens' CreateEndpoint (Maybe MicrosoftSQLServerSettings)
ceMicrosoftSQLServerSettings = lens _ceMicrosoftSQLServerSettings (\s a -> s {_ceMicrosoftSQLServerSettings = a})

-- | The Amazon Resource Name (ARN) for the certificate.
ceCertificateARN :: Lens' CreateEndpoint (Maybe Text)
ceCertificateARN = lens _ceCertificateARN (\s a -> s {_ceCertificateARN = a})

-- | The Amazon Resource Name (ARN) for the service access role that you want to use to create the endpoint.
ceServiceAccessRoleARN :: Lens' CreateEndpoint (Maybe Text)
ceServiceAccessRoleARN = lens _ceServiceAccessRoleARN (\s a -> s {_ceServiceAccessRoleARN = a})

-- | Undocumented member.
ceDocDBSettings :: Lens' CreateEndpoint (Maybe DocDBSettings)
ceDocDBSettings = lens _ceDocDBSettings (\s a -> s {_ceDocDBSettings = a})

-- | Settings in JSON format for the source and target PostgreSQL endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html Extra connection attributes when using PostgreSQL as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.PostgreSQL.html Extra connection attributes when using PostgreSQL as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
cePostgreSQLSettings :: Lens' CreateEndpoint (Maybe PostgreSQLSettings)
cePostgreSQLSettings = lens _cePostgreSQLSettings (\s a -> s {_cePostgreSQLSettings = a})

-- | Additional attributes associated with the connection. Each attribute is specified as a name-value pair associated by an equal sign (=). Multiple attributes are separated by a semicolon (;) with no additional white space. For information on the attributes available for connecting your source or target endpoint, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Endpoints.html Working with AWS DMS Endpoints> in the /AWS Database Migration Service User Guide./
ceExtraConnectionAttributes :: Lens' CreateEndpoint (Maybe Text)
ceExtraConnectionAttributes = lens _ceExtraConnectionAttributes (\s a -> s {_ceExtraConnectionAttributes = a})

-- | Settings in JSON format for the target Apache Kafka endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
ceKafkaSettings :: Lens' CreateEndpoint (Maybe KafkaSettings)
ceKafkaSettings = lens _ceKafkaSettings (\s a -> s {_ceKafkaSettings = a})

-- | Settings in JSON format for the source and target Oracle endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html Extra connection attributes when using Oracle as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Oracle.html Extra connection attributes when using Oracle as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
ceOracleSettings :: Lens' CreateEndpoint (Maybe OracleSettings)
ceOracleSettings = lens _ceOracleSettings (\s a -> s {_ceOracleSettings = a})

-- | Undocumented member.
ceRedshiftSettings :: Lens' CreateEndpoint (Maybe RedshiftSettings)
ceRedshiftSettings = lens _ceRedshiftSettings (\s a -> s {_ceRedshiftSettings = a})

-- | Settings in JSON format for the target Elasticsearch endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Elasticsearch.html#CHAP_Target.Elasticsearch.Configuration Extra Connection Attributes When Using Elasticsearch as a Target for AWS DMS> in the /AWS Database Migration Service User Guide/ .
ceElasticsearchSettings :: Lens' CreateEndpoint (Maybe ElasticsearchSettings)
ceElasticsearchSettings = lens _ceElasticsearchSettings (\s a -> s {_ceElasticsearchSettings = a})

-- | The user name to be used to log in to the endpoint database.
ceUsername :: Lens' CreateEndpoint (Maybe Text)
ceUsername = lens _ceUsername (\s a -> s {_ceUsername = a})

-- | The external table definition.
ceExternalTableDefinition :: Lens' CreateEndpoint (Maybe Text)
ceExternalTableDefinition = lens _ceExternalTableDefinition (\s a -> s {_ceExternalTableDefinition = a})

-- | Settings in JSON format for the target Amazon Neptune endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.EndpointSettings Specifying Endpoint Settings for Amazon Neptune as a Target> in the /AWS Database Migration Service User Guide./
ceNeptuneSettings :: Lens' CreateEndpoint (Maybe NeptuneSettings)
ceNeptuneSettings = lens _ceNeptuneSettings (\s a -> s {_ceNeptuneSettings = a})

-- | Settings in JSON format for the source IBM Db2 LUW endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.DB2.html Extra connection attributes when using Db2 LUW as a source for AWS DMS> in the /AWS Database Migration Service User Guide./
ceIBMDB2Settings :: Lens' CreateEndpoint (Maybe IBMDB2Settings)
ceIBMDB2Settings = lens _ceIBMDB2Settings (\s a -> s {_ceIBMDB2Settings = a})

-- | An AWS KMS key identifier that is used to encrypt the connection parameters for the endpoint. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
ceKMSKeyId :: Lens' CreateEndpoint (Maybe Text)
ceKMSKeyId = lens _ceKMSKeyId (\s a -> s {_ceKMSKeyId = a})

-- | Settings in JSON format for the source MongoDB endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.MongoDB.html#CHAP_Source.MongoDB.Configuration Using MongoDB as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
ceMongoDBSettings :: Lens' CreateEndpoint (Maybe MongoDBSettings)
ceMongoDBSettings = lens _ceMongoDBSettings (\s a -> s {_ceMongoDBSettings = a})

-- | The Secure Sockets Layer (SSL) mode to use for the SSL connection. The default is @none@
ceSSLMode :: Lens' CreateEndpoint (Maybe DmsSSLModeValue)
ceSSLMode = lens _ceSSLMode (\s a -> s {_ceSSLMode = a})

-- | The password to be used to log in to the endpoint database.
cePassword :: Lens' CreateEndpoint (Maybe Text)
cePassword = lens _cePassword (\s a -> s {_cePassword = a}) . mapping _Sensitive

-- | Settings in JSON format for the source and target SAP ASE endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.SAP.html Extra connection attributes when using SAP ASE as a source for AWS DMS> and <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.SAP.html Extra connection attributes when using SAP ASE as a target for AWS DMS> in the /AWS Database Migration Service User Guide./
ceSybaseSettings :: Lens' CreateEndpoint (Maybe SybaseSettings)
ceSybaseSettings = lens _ceSybaseSettings (\s a -> s {_ceSybaseSettings = a})

-- | The name of the endpoint database.
ceDatabaseName :: Lens' CreateEndpoint (Maybe Text)
ceDatabaseName = lens _ceDatabaseName (\s a -> s {_ceDatabaseName = a})

-- | Settings in JSON format for the target Amazon S3 endpoint. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring Extra Connection Attributes When Using Amazon S3 as a Target for AWS DMS> in the /AWS Database Migration Service User Guide./
ceS3Settings :: Lens' CreateEndpoint (Maybe S3Settings)
ceS3Settings = lens _ceS3Settings (\s a -> s {_ceS3Settings = a})

-- | Settings in JSON format for the target endpoint for Amazon Kinesis Data Streams. For more information about the available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kinesis.html Using Amazon Kinesis Data Streams as a Target for AWS Database Migration Service> in the /AWS Database Migration Service User Guide./
ceKinesisSettings :: Lens' CreateEndpoint (Maybe KinesisSettings)
ceKinesisSettings = lens _ceKinesisSettings (\s a -> s {_ceKinesisSettings = a})

-- | Settings in JSON format for the target Amazon DynamoDB endpoint. For information about other available settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.DynamoDB.html Using Object Mapping to Migrate Data to DynamoDB> in the /AWS Database Migration Service User Guide./
ceDynamoDBSettings :: Lens' CreateEndpoint (Maybe DynamoDBSettings)
ceDynamoDBSettings = lens _ceDynamoDBSettings (\s a -> s {_ceDynamoDBSettings = a})

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
ceResourceIdentifier :: Lens' CreateEndpoint (Maybe Text)
ceResourceIdentifier = lens _ceResourceIdentifier (\s a -> s {_ceResourceIdentifier = a})

-- | One or more tags to be assigned to the endpoint.
ceTags :: Lens' CreateEndpoint [Tag]
ceTags = lens _ceTags (\s a -> s {_ceTags = a}) . _Default . _Coerce

-- | The port used by the endpoint database.
cePort :: Lens' CreateEndpoint (Maybe Int)
cePort = lens _cePort (\s a -> s {_cePort = a})

-- | The database endpoint identifier. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen, or contain two consecutive hyphens.
ceEndpointIdentifier :: Lens' CreateEndpoint Text
ceEndpointIdentifier = lens _ceEndpointIdentifier (\s a -> s {_ceEndpointIdentifier = a})

-- | The type of endpoint. Valid values are @source@ and @target@ .
ceEndpointType :: Lens' CreateEndpoint ReplicationEndpointTypeValue
ceEndpointType = lens _ceEndpointType (\s a -> s {_ceEndpointType = a})

-- | The type of engine for the endpoint. Valid values, depending on the @EndpointType@ value, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"docdb"@ , @"sqlserver"@ , and @"neptune"@ .
ceEngineName :: Lens' CreateEndpoint Text
ceEngineName = lens _ceEngineName (\s a -> s {_ceEngineName = a})

instance AWSRequest CreateEndpoint where
  type Rs CreateEndpoint = CreateEndpointResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            <$> (x .?> "Endpoint") <*> (pure (fromEnum s))
      )

instance Hashable CreateEndpoint

instance NFData CreateEndpoint

instance ToHeaders CreateEndpoint where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonDMSv20160101.CreateEndpoint" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    object
      ( catMaybes
          [ ("DmsTransferSettings" .=) <$> _ceDmsTransferSettings,
            ("MySQLSettings" .=) <$> _ceMySQLSettings,
            ("ServerName" .=) <$> _ceServerName,
            ("MicrosoftSQLServerSettings" .=)
              <$> _ceMicrosoftSQLServerSettings,
            ("CertificateArn" .=) <$> _ceCertificateARN,
            ("ServiceAccessRoleArn" .=) <$> _ceServiceAccessRoleARN,
            ("DocDbSettings" .=) <$> _ceDocDBSettings,
            ("PostgreSQLSettings" .=) <$> _cePostgreSQLSettings,
            ("ExtraConnectionAttributes" .=) <$> _ceExtraConnectionAttributes,
            ("KafkaSettings" .=) <$> _ceKafkaSettings,
            ("OracleSettings" .=) <$> _ceOracleSettings,
            ("RedshiftSettings" .=) <$> _ceRedshiftSettings,
            ("ElasticsearchSettings" .=) <$> _ceElasticsearchSettings,
            ("Username" .=) <$> _ceUsername,
            ("ExternalTableDefinition" .=) <$> _ceExternalTableDefinition,
            ("NeptuneSettings" .=) <$> _ceNeptuneSettings,
            ("IBMDb2Settings" .=) <$> _ceIBMDB2Settings,
            ("KmsKeyId" .=) <$> _ceKMSKeyId,
            ("MongoDbSettings" .=) <$> _ceMongoDBSettings,
            ("SslMode" .=) <$> _ceSSLMode,
            ("Password" .=) <$> _cePassword,
            ("SybaseSettings" .=) <$> _ceSybaseSettings,
            ("DatabaseName" .=) <$> _ceDatabaseName,
            ("S3Settings" .=) <$> _ceS3Settings,
            ("KinesisSettings" .=) <$> _ceKinesisSettings,
            ("DynamoDbSettings" .=) <$> _ceDynamoDBSettings,
            ("ResourceIdentifier" .=) <$> _ceResourceIdentifier,
            ("Tags" .=) <$> _ceTags,
            ("Port" .=) <$> _cePort,
            Just ("EndpointIdentifier" .= _ceEndpointIdentifier),
            Just ("EndpointType" .= _ceEndpointType),
            Just ("EngineName" .= _ceEngineName)
          ]
      )

instance ToPath CreateEndpoint where
  toPath = const "/"

instance ToQuery CreateEndpoint where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'createEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { _cersEndpoint ::
      !(Maybe Endpoint),
    _cersResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cersEndpoint' - The endpoint that was created.
--
-- * 'cersResponseStatus' - -- | The response status code.
createEndpointResponse ::
  -- | 'cersResponseStatus'
  Int ->
  CreateEndpointResponse
createEndpointResponse pResponseStatus_ =
  CreateEndpointResponse'
    { _cersEndpoint = Nothing,
      _cersResponseStatus = pResponseStatus_
    }

-- | The endpoint that was created.
cersEndpoint :: Lens' CreateEndpointResponse (Maybe Endpoint)
cersEndpoint = lens _cersEndpoint (\s a -> s {_cersEndpoint = a})

-- | -- | The response status code.
cersResponseStatus :: Lens' CreateEndpointResponse Int
cersResponseStatus = lens _cersResponseStatus (\s a -> s {_cersResponseStatus = a})

instance NFData CreateEndpointResponse
