{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Connection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Connection
  ( Connection (..),

    -- * Smart constructor
    mkConnection,

    -- * Lenses
    cfCreationTime,
    cfLastUpdatedBy,
    cfConnectionProperties,
    cfLastUpdatedTime,
    cfMatchCriteria,
    cfPhysicalConnectionRequirements,
    cfName,
    cfDescription,
    cfConnectionType,
  )
where

import Network.AWS.Glue.Types.ConnectionPropertyKey
import Network.AWS.Glue.Types.ConnectionType
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a connection to a data source.
--
-- /See:/ 'mkConnection' smart constructor.
data Connection = Connection'
  { -- | The time that this connection definition was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The user, group, or role that last updated this connection definition.
    lastUpdatedBy :: Lude.Maybe Lude.Text,
    -- | These key-value pairs define parameters for the connection:
    --
    --
    --     * @HOST@ - The host URI: either the fully qualified domain name (FQDN) or the IPv4 address of the database host.
    --
    --
    --     * @PORT@ - The port number, between 1024 and 65535, of the port on which the database host is listening for database connections.
    --
    --
    --     * @USER_NAME@ - The name under which to log in to the database. The value string for @USER_NAME@ is "@USERNAME@ ".
    --
    --
    --     * @PASSWORD@ - A password, if one is used, for the user name.
    --
    --
    --     * @ENCRYPTED_PASSWORD@ - When you enable connection password protection by setting @ConnectionPasswordEncryption@ in the Data Catalog encryption settings, this field stores the encrypted password.
    --
    --
    --     * @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon S3) path of the JAR file that contains the JDBC driver to use.
    --
    --
    --     * @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.
    --
    --
    --     * @JDBC_ENGINE@ - The name of the JDBC engine to use.
    --
    --
    --     * @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.
    --
    --
    --     * @CONFIG_FILES@ - (Reserved for future use.)
    --
    --
    --     * @INSTANCE_ID@ - The instance ID to use.
    --
    --
    --     * @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data source.
    --
    --
    --     * @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying whether Secure Sockets Layer (SSL) with hostname matching is enforced for the JDBC connection on the client. The default is false.
    --
    --
    --     * @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the customer's root certificate. AWS Glue uses this root certificate to validate the customer’s certificate when connecting to the customer database. AWS Glue only handles X.509 certificates. The certificate provided must be DER-encoded and supplied in Base64 encoding PEM format.
    --
    --
    --     * @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@ . AWS Glue validates the Signature algorithm and Subject Public Key Algorithm for the customer certificate. The only permitted algorithms for the Signature algorithm are SHA256withRSA, SHA384withRSA or SHA512withRSA. For the Subject Public Key Algorithm, the key length must be at least 2048. You can set the value of this property to @true@ to skip AWS Glue’s validation of the customer certificate.
    --
    --
    --     * @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which is used for domain match or distinguished name match to prevent a man-in-the-middle attack. In Oracle database, this is used as the @SSL_SERVER_CERT_DN@ ; in Microsoft SQL Server, this is used as the @hostNameInCertificate@ .
    --
    --
    --     * @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC) data source.
    --
    --
    --     * @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port pairs that are the addresses of the Apache Kafka brokers in a Kafka cluster to which a Kafka client will connect to and bootstrap itself.
    --
    --
    --     * @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache Kafka connection. Default value is "true".
    --
    --
    --     * @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file (.pem format). The default is an empty string.
    --
    --
    --     * @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation of the CA cert file or not. AWS Glue validates for three algorithms: SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is "false".
    connectionProperties :: Lude.Maybe (Lude.HashMap ConnectionPropertyKey (Lude.Text)),
    -- | The last time that this connection definition was updated.
    lastUpdatedTime :: Lude.Maybe Lude.Timestamp,
    -- | A list of criteria that can be used in selecting this connection.
    matchCriteria :: Lude.Maybe [Lude.Text],
    -- | A map of physical connection requirements, such as virtual private cloud (VPC) and @SecurityGroup@ , that are needed to make this connection successfully.
    physicalConnectionRequirements :: Lude.Maybe PhysicalConnectionRequirements,
    -- | The name of the connection definition.
    name :: Lude.Maybe Lude.Text,
    -- | The description of the connection.
    description :: Lude.Maybe Lude.Text,
    -- | The type of the connection. Currently, SFTP is not supported.
    connectionType :: Lude.Maybe ConnectionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time that this connection definition was created.
-- * 'lastUpdatedBy' - The user, group, or role that last updated this connection definition.
-- * 'connectionProperties' - These key-value pairs define parameters for the connection:
--
--
--     * @HOST@ - The host URI: either the fully qualified domain name (FQDN) or the IPv4 address of the database host.
--
--
--     * @PORT@ - The port number, between 1024 and 65535, of the port on which the database host is listening for database connections.
--
--
--     * @USER_NAME@ - The name under which to log in to the database. The value string for @USER_NAME@ is "@USERNAME@ ".
--
--
--     * @PASSWORD@ - A password, if one is used, for the user name.
--
--
--     * @ENCRYPTED_PASSWORD@ - When you enable connection password protection by setting @ConnectionPasswordEncryption@ in the Data Catalog encryption settings, this field stores the encrypted password.
--
--
--     * @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon S3) path of the JAR file that contains the JDBC driver to use.
--
--
--     * @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.
--
--
--     * @JDBC_ENGINE@ - The name of the JDBC engine to use.
--
--
--     * @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.
--
--
--     * @CONFIG_FILES@ - (Reserved for future use.)
--
--
--     * @INSTANCE_ID@ - The instance ID to use.
--
--
--     * @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data source.
--
--
--     * @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying whether Secure Sockets Layer (SSL) with hostname matching is enforced for the JDBC connection on the client. The default is false.
--
--
--     * @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the customer's root certificate. AWS Glue uses this root certificate to validate the customer’s certificate when connecting to the customer database. AWS Glue only handles X.509 certificates. The certificate provided must be DER-encoded and supplied in Base64 encoding PEM format.
--
--
--     * @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@ . AWS Glue validates the Signature algorithm and Subject Public Key Algorithm for the customer certificate. The only permitted algorithms for the Signature algorithm are SHA256withRSA, SHA384withRSA or SHA512withRSA. For the Subject Public Key Algorithm, the key length must be at least 2048. You can set the value of this property to @true@ to skip AWS Glue’s validation of the customer certificate.
--
--
--     * @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which is used for domain match or distinguished name match to prevent a man-in-the-middle attack. In Oracle database, this is used as the @SSL_SERVER_CERT_DN@ ; in Microsoft SQL Server, this is used as the @hostNameInCertificate@ .
--
--
--     * @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC) data source.
--
--
--     * @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port pairs that are the addresses of the Apache Kafka brokers in a Kafka cluster to which a Kafka client will connect to and bootstrap itself.
--
--
--     * @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache Kafka connection. Default value is "true".
--
--
--     * @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file (.pem format). The default is an empty string.
--
--
--     * @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation of the CA cert file or not. AWS Glue validates for three algorithms: SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is "false".
--
--
-- * 'lastUpdatedTime' - The last time that this connection definition was updated.
-- * 'matchCriteria' - A list of criteria that can be used in selecting this connection.
-- * 'physicalConnectionRequirements' - A map of physical connection requirements, such as virtual private cloud (VPC) and @SecurityGroup@ , that are needed to make this connection successfully.
-- * 'name' - The name of the connection definition.
-- * 'description' - The description of the connection.
-- * 'connectionType' - The type of the connection. Currently, SFTP is not supported.
mkConnection ::
  Connection
mkConnection =
  Connection'
    { creationTime = Lude.Nothing,
      lastUpdatedBy = Lude.Nothing,
      connectionProperties = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing,
      matchCriteria = Lude.Nothing,
      physicalConnectionRequirements = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      connectionType = Lude.Nothing
    }

-- | The time that this connection definition was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCreationTime :: Lens.Lens' Connection (Lude.Maybe Lude.Timestamp)
cfCreationTime = Lens.lens (creationTime :: Connection -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Connection)
{-# DEPRECATED cfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The user, group, or role that last updated this connection definition.
--
-- /Note:/ Consider using 'lastUpdatedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLastUpdatedBy :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cfLastUpdatedBy = Lens.lens (lastUpdatedBy :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedBy = a} :: Connection)
{-# DEPRECATED cfLastUpdatedBy "Use generic-lens or generic-optics with 'lastUpdatedBy' instead." #-}

-- | These key-value pairs define parameters for the connection:
--
--
--     * @HOST@ - The host URI: either the fully qualified domain name (FQDN) or the IPv4 address of the database host.
--
--
--     * @PORT@ - The port number, between 1024 and 65535, of the port on which the database host is listening for database connections.
--
--
--     * @USER_NAME@ - The name under which to log in to the database. The value string for @USER_NAME@ is "@USERNAME@ ".
--
--
--     * @PASSWORD@ - A password, if one is used, for the user name.
--
--
--     * @ENCRYPTED_PASSWORD@ - When you enable connection password protection by setting @ConnectionPasswordEncryption@ in the Data Catalog encryption settings, this field stores the encrypted password.
--
--
--     * @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon S3) path of the JAR file that contains the JDBC driver to use.
--
--
--     * @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.
--
--
--     * @JDBC_ENGINE@ - The name of the JDBC engine to use.
--
--
--     * @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.
--
--
--     * @CONFIG_FILES@ - (Reserved for future use.)
--
--
--     * @INSTANCE_ID@ - The instance ID to use.
--
--
--     * @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data source.
--
--
--     * @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying whether Secure Sockets Layer (SSL) with hostname matching is enforced for the JDBC connection on the client. The default is false.
--
--
--     * @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the customer's root certificate. AWS Glue uses this root certificate to validate the customer’s certificate when connecting to the customer database. AWS Glue only handles X.509 certificates. The certificate provided must be DER-encoded and supplied in Base64 encoding PEM format.
--
--
--     * @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@ . AWS Glue validates the Signature algorithm and Subject Public Key Algorithm for the customer certificate. The only permitted algorithms for the Signature algorithm are SHA256withRSA, SHA384withRSA or SHA512withRSA. For the Subject Public Key Algorithm, the key length must be at least 2048. You can set the value of this property to @true@ to skip AWS Glue’s validation of the customer certificate.
--
--
--     * @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which is used for domain match or distinguished name match to prevent a man-in-the-middle attack. In Oracle database, this is used as the @SSL_SERVER_CERT_DN@ ; in Microsoft SQL Server, this is used as the @hostNameInCertificate@ .
--
--
--     * @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC) data source.
--
--
--     * @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port pairs that are the addresses of the Apache Kafka brokers in a Kafka cluster to which a Kafka client will connect to and bootstrap itself.
--
--
--     * @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache Kafka connection. Default value is "true".
--
--
--     * @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file (.pem format). The default is an empty string.
--
--
--     * @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation of the CA cert file or not. AWS Glue validates for three algorithms: SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is "false".
--
--
--
-- /Note:/ Consider using 'connectionProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfConnectionProperties :: Lens.Lens' Connection (Lude.Maybe (Lude.HashMap ConnectionPropertyKey (Lude.Text)))
cfConnectionProperties = Lens.lens (connectionProperties :: Connection -> Lude.Maybe (Lude.HashMap ConnectionPropertyKey (Lude.Text))) (\s a -> s {connectionProperties = a} :: Connection)
{-# DEPRECATED cfConnectionProperties "Use generic-lens or generic-optics with 'connectionProperties' instead." #-}

-- | The last time that this connection definition was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLastUpdatedTime :: Lens.Lens' Connection (Lude.Maybe Lude.Timestamp)
cfLastUpdatedTime = Lens.lens (lastUpdatedTime :: Connection -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: Connection)
{-# DEPRECATED cfLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | A list of criteria that can be used in selecting this connection.
--
-- /Note:/ Consider using 'matchCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMatchCriteria :: Lens.Lens' Connection (Lude.Maybe [Lude.Text])
cfMatchCriteria = Lens.lens (matchCriteria :: Connection -> Lude.Maybe [Lude.Text]) (\s a -> s {matchCriteria = a} :: Connection)
{-# DEPRECATED cfMatchCriteria "Use generic-lens or generic-optics with 'matchCriteria' instead." #-}

-- | A map of physical connection requirements, such as virtual private cloud (VPC) and @SecurityGroup@ , that are needed to make this connection successfully.
--
-- /Note:/ Consider using 'physicalConnectionRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPhysicalConnectionRequirements :: Lens.Lens' Connection (Lude.Maybe PhysicalConnectionRequirements)
cfPhysicalConnectionRequirements = Lens.lens (physicalConnectionRequirements :: Connection -> Lude.Maybe PhysicalConnectionRequirements) (\s a -> s {physicalConnectionRequirements = a} :: Connection)
{-# DEPRECATED cfPhysicalConnectionRequirements "Use generic-lens or generic-optics with 'physicalConnectionRequirements' instead." #-}

-- | The name of the connection definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cfName = Lens.lens (name :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Connection)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the connection.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cfDescription = Lens.lens (description :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Connection)
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The type of the connection. Currently, SFTP is not supported.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfConnectionType :: Lens.Lens' Connection (Lude.Maybe ConnectionType)
cfConnectionType = Lens.lens (connectionType :: Connection -> Lude.Maybe ConnectionType) (\s a -> s {connectionType = a} :: Connection)
{-# DEPRECATED cfConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

instance Lude.FromJSON Connection where
  parseJSON =
    Lude.withObject
      "Connection"
      ( \x ->
          Connection'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LastUpdatedBy")
            Lude.<*> (x Lude..:? "ConnectionProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "MatchCriteria" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PhysicalConnectionRequirements")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ConnectionType")
      )
