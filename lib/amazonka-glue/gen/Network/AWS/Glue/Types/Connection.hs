{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Connection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Connection where

import Network.AWS.Glue.Types.ConnectionPropertyKey
import Network.AWS.Glue.Types.ConnectionType
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines a connection to a data source.
--
--
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
  { _conCreationTime :: !(Maybe POSIX),
    _conLastUpdatedBy :: !(Maybe Text),
    _conConnectionProperties ::
      !(Maybe (Map ConnectionPropertyKey (Text))),
    _conLastUpdatedTime :: !(Maybe POSIX),
    _conMatchCriteria :: !(Maybe [Text]),
    _conPhysicalConnectionRequirements ::
      !(Maybe PhysicalConnectionRequirements),
    _conName :: !(Maybe Text),
    _conDescription :: !(Maybe Text),
    _conConnectionType :: !(Maybe ConnectionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'conCreationTime' - The time that this connection definition was created.
--
-- * 'conLastUpdatedBy' - The user, group, or role that last updated this connection definition.
--
-- * 'conConnectionProperties' - These key-value pairs define parameters for the connection:     * @HOST@ - The host URI: either the fully qualified domain name (FQDN) or the IPv4 address of the database host.     * @PORT@ - The port number, between 1024 and 65535, of the port on which the database host is listening for database connections.     * @USER_NAME@ - The name under which to log in to the database. The value string for @USER_NAME@ is "@USERNAME@ ".     * @PASSWORD@ - A password, if one is used, for the user name.     * @ENCRYPTED_PASSWORD@ - When you enable connection password protection by setting @ConnectionPasswordEncryption@ in the Data Catalog encryption settings, this field stores the encrypted password.     * @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon S3) path of the JAR file that contains the JDBC driver to use.     * @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.     * @JDBC_ENGINE@ - The name of the JDBC engine to use.     * @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.     * @CONFIG_FILES@ - (Reserved for future use.)     * @INSTANCE_ID@ - The instance ID to use.     * @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data source.     * @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying whether Secure Sockets Layer (SSL) with hostname matching is enforced for the JDBC connection on the client. The default is false.     * @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the customer's root certificate. AWS Glue uses this root certificate to validate the customer’s certificate when connecting to the customer database. AWS Glue only handles X.509 certificates. The certificate provided must be DER-encoded and supplied in Base64 encoding PEM format.     * @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@ . AWS Glue validates the Signature algorithm and Subject Public Key Algorithm for the customer certificate. The only permitted algorithms for the Signature algorithm are SHA256withRSA, SHA384withRSA or SHA512withRSA. For the Subject Public Key Algorithm, the key length must be at least 2048. You can set the value of this property to @true@ to skip AWS Glue’s validation of the customer certificate.     * @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which is used for domain match or distinguished name match to prevent a man-in-the-middle attack. In Oracle database, this is used as the @SSL_SERVER_CERT_DN@ ; in Microsoft SQL Server, this is used as the @hostNameInCertificate@ .     * @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC) data source.     * @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port pairs that are the addresses of the Apache Kafka brokers in a Kafka cluster to which a Kafka client will connect to and bootstrap itself.     * @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache Kafka connection. Default value is "true".     * @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file (.pem format). The default is an empty string.     * @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation of the CA cert file or not. AWS Glue validates for three algorithms: SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is "false".
--
-- * 'conLastUpdatedTime' - The last time that this connection definition was updated.
--
-- * 'conMatchCriteria' - A list of criteria that can be used in selecting this connection.
--
-- * 'conPhysicalConnectionRequirements' - A map of physical connection requirements, such as virtual private cloud (VPC) and @SecurityGroup@ , that are needed to make this connection successfully.
--
-- * 'conName' - The name of the connection definition.
--
-- * 'conDescription' - The description of the connection.
--
-- * 'conConnectionType' - The type of the connection. Currently, SFTP is not supported.
connection ::
  Connection
connection =
  Connection'
    { _conCreationTime = Nothing,
      _conLastUpdatedBy = Nothing,
      _conConnectionProperties = Nothing,
      _conLastUpdatedTime = Nothing,
      _conMatchCriteria = Nothing,
      _conPhysicalConnectionRequirements = Nothing,
      _conName = Nothing,
      _conDescription = Nothing,
      _conConnectionType = Nothing
    }

-- | The time that this connection definition was created.
conCreationTime :: Lens' Connection (Maybe UTCTime)
conCreationTime = lens _conCreationTime (\s a -> s {_conCreationTime = a}) . mapping _Time

-- | The user, group, or role that last updated this connection definition.
conLastUpdatedBy :: Lens' Connection (Maybe Text)
conLastUpdatedBy = lens _conLastUpdatedBy (\s a -> s {_conLastUpdatedBy = a})

-- | These key-value pairs define parameters for the connection:     * @HOST@ - The host URI: either the fully qualified domain name (FQDN) or the IPv4 address of the database host.     * @PORT@ - The port number, between 1024 and 65535, of the port on which the database host is listening for database connections.     * @USER_NAME@ - The name under which to log in to the database. The value string for @USER_NAME@ is "@USERNAME@ ".     * @PASSWORD@ - A password, if one is used, for the user name.     * @ENCRYPTED_PASSWORD@ - When you enable connection password protection by setting @ConnectionPasswordEncryption@ in the Data Catalog encryption settings, this field stores the encrypted password.     * @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon S3) path of the JAR file that contains the JDBC driver to use.     * @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.     * @JDBC_ENGINE@ - The name of the JDBC engine to use.     * @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.     * @CONFIG_FILES@ - (Reserved for future use.)     * @INSTANCE_ID@ - The instance ID to use.     * @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data source.     * @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying whether Secure Sockets Layer (SSL) with hostname matching is enforced for the JDBC connection on the client. The default is false.     * @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the customer's root certificate. AWS Glue uses this root certificate to validate the customer’s certificate when connecting to the customer database. AWS Glue only handles X.509 certificates. The certificate provided must be DER-encoded and supplied in Base64 encoding PEM format.     * @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@ . AWS Glue validates the Signature algorithm and Subject Public Key Algorithm for the customer certificate. The only permitted algorithms for the Signature algorithm are SHA256withRSA, SHA384withRSA or SHA512withRSA. For the Subject Public Key Algorithm, the key length must be at least 2048. You can set the value of this property to @true@ to skip AWS Glue’s validation of the customer certificate.     * @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which is used for domain match or distinguished name match to prevent a man-in-the-middle attack. In Oracle database, this is used as the @SSL_SERVER_CERT_DN@ ; in Microsoft SQL Server, this is used as the @hostNameInCertificate@ .     * @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC) data source.     * @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port pairs that are the addresses of the Apache Kafka brokers in a Kafka cluster to which a Kafka client will connect to and bootstrap itself.     * @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache Kafka connection. Default value is "true".     * @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file (.pem format). The default is an empty string.     * @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation of the CA cert file or not. AWS Glue validates for three algorithms: SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is "false".
conConnectionProperties :: Lens' Connection (HashMap ConnectionPropertyKey (Text))
conConnectionProperties = lens _conConnectionProperties (\s a -> s {_conConnectionProperties = a}) . _Default . _Map

-- | The last time that this connection definition was updated.
conLastUpdatedTime :: Lens' Connection (Maybe UTCTime)
conLastUpdatedTime = lens _conLastUpdatedTime (\s a -> s {_conLastUpdatedTime = a}) . mapping _Time

-- | A list of criteria that can be used in selecting this connection.
conMatchCriteria :: Lens' Connection [Text]
conMatchCriteria = lens _conMatchCriteria (\s a -> s {_conMatchCriteria = a}) . _Default . _Coerce

-- | A map of physical connection requirements, such as virtual private cloud (VPC) and @SecurityGroup@ , that are needed to make this connection successfully.
conPhysicalConnectionRequirements :: Lens' Connection (Maybe PhysicalConnectionRequirements)
conPhysicalConnectionRequirements = lens _conPhysicalConnectionRequirements (\s a -> s {_conPhysicalConnectionRequirements = a})

-- | The name of the connection definition.
conName :: Lens' Connection (Maybe Text)
conName = lens _conName (\s a -> s {_conName = a})

-- | The description of the connection.
conDescription :: Lens' Connection (Maybe Text)
conDescription = lens _conDescription (\s a -> s {_conDescription = a})

-- | The type of the connection. Currently, SFTP is not supported.
conConnectionType :: Lens' Connection (Maybe ConnectionType)
conConnectionType = lens _conConnectionType (\s a -> s {_conConnectionType = a})

instance FromJSON Connection where
  parseJSON =
    withObject
      "Connection"
      ( \x ->
          Connection'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LastUpdatedBy")
            <*> (x .:? "ConnectionProperties" .!= mempty)
            <*> (x .:? "LastUpdatedTime")
            <*> (x .:? "MatchCriteria" .!= mempty)
            <*> (x .:? "PhysicalConnectionRequirements")
            <*> (x .:? "Name")
            <*> (x .:? "Description")
            <*> (x .:? "ConnectionType")
      )

instance Hashable Connection

instance NFData Connection
