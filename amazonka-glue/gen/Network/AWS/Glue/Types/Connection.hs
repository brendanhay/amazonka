{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Connection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Connection where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.ConnectionPropertyKey
import Network.AWS.Glue.Types.ConnectionType
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines a connection to a data source.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | These key-value pairs define parameters for the connection:
    --
    -- -   @HOST@ - The host URI: either the fully qualified domain name (FQDN)
    --     or the IPv4 address of the database host.
    --
    -- -   @PORT@ - The port number, between 1024 and 65535, of the port on
    --     which the database host is listening for database connections.
    --
    -- -   @USER_NAME@ - The name under which to log in to the database. The
    --     value string for @USER_NAME@ is \"@USERNAME@\".
    --
    -- -   @PASSWORD@ - A password, if one is used, for the user name.
    --
    -- -   @ENCRYPTED_PASSWORD@ - When you enable connection password
    --     protection by setting @ConnectionPasswordEncryption@ in the Data
    --     Catalog encryption settings, this field stores the encrypted
    --     password.
    --
    -- -   @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon
    --     S3) path of the JAR file that contains the JDBC driver to use.
    --
    -- -   @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.
    --
    -- -   @JDBC_ENGINE@ - The name of the JDBC engine to use.
    --
    -- -   @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.
    --
    -- -   @CONFIG_FILES@ - (Reserved for future use.)
    --
    -- -   @INSTANCE_ID@ - The instance ID to use.
    --
    -- -   @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data
    --     source.
    --
    -- -   @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying
    --     whether Secure Sockets Layer (SSL) with hostname matching is
    --     enforced for the JDBC connection on the client. The default is
    --     false.
    --
    -- -   @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the
    --     customer\'s root certificate. AWS Glue uses this root certificate to
    --     validate the customer’s certificate when connecting to the customer
    --     database. AWS Glue only handles X.509 certificates. The certificate
    --     provided must be DER-encoded and supplied in Base64 encoding PEM
    --     format.
    --
    -- -   @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@.
    --     AWS Glue validates the Signature algorithm and Subject Public Key
    --     Algorithm for the customer certificate. The only permitted
    --     algorithms for the Signature algorithm are SHA256withRSA,
    --     SHA384withRSA or SHA512withRSA. For the Subject Public Key
    --     Algorithm, the key length must be at least 2048. You can set the
    --     value of this property to @true@ to skip AWS Glue’s validation of
    --     the customer certificate.
    --
    -- -   @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which
    --     is used for domain match or distinguished name match to prevent a
    --     man-in-the-middle attack. In Oracle database, this is used as the
    --     @SSL_SERVER_CERT_DN@; in Microsoft SQL Server, this is used as the
    --     @hostNameInCertificate@.
    --
    -- -   @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC)
    --     data source.
    --
    -- -   @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port
    --     pairs that are the addresses of the Apache Kafka brokers in a Kafka
    --     cluster to which a Kafka client will connect to and bootstrap
    --     itself.
    --
    -- -   @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache
    --     Kafka connection. Default value is \"true\".
    --
    -- -   @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file
    --     (.pem format). The default is an empty string.
    --
    -- -   @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation
    --     of the CA cert file or not. AWS Glue validates for three algorithms:
    --     SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is
    --     \"false\".
    --
    -- -   @SECRET_ID@ - The secret ID used for the secret manager of
    --     credentials.
    --
    -- -   @CONNECTOR_URL@ - The connector URL for a MARKETPLACE or CUSTOM
    --     connection.
    --
    -- -   @CONNECTOR_TYPE@ - The connector type for a MARKETPLACE or CUSTOM
    --     connection.
    --
    -- -   @CONNECTOR_CLASS_NAME@ - The connector class name for a MARKETPLACE
    --     or CUSTOM connection.
    connectionProperties :: Prelude.Maybe (Prelude.HashMap ConnectionPropertyKey Prelude.Text),
    -- | The time that this connection definition was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The type of the connection. Currently, SFTP is not supported.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | A map of physical connection requirements, such as virtual private cloud
    -- (VPC) and @SecurityGroup@, that are needed to make this connection
    -- successfully.
    physicalConnectionRequirements :: Prelude.Maybe PhysicalConnectionRequirements,
    -- | The name of the connection definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The user, group, or role that last updated this connection definition.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | The description of the connection.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of criteria that can be used in selecting this connection.
    matchCriteria :: Prelude.Maybe [Prelude.Text],
    -- | The last time that this connection definition was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Connection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionProperties', 'connection_connectionProperties' - These key-value pairs define parameters for the connection:
--
-- -   @HOST@ - The host URI: either the fully qualified domain name (FQDN)
--     or the IPv4 address of the database host.
--
-- -   @PORT@ - The port number, between 1024 and 65535, of the port on
--     which the database host is listening for database connections.
--
-- -   @USER_NAME@ - The name under which to log in to the database. The
--     value string for @USER_NAME@ is \"@USERNAME@\".
--
-- -   @PASSWORD@ - A password, if one is used, for the user name.
--
-- -   @ENCRYPTED_PASSWORD@ - When you enable connection password
--     protection by setting @ConnectionPasswordEncryption@ in the Data
--     Catalog encryption settings, this field stores the encrypted
--     password.
--
-- -   @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon
--     S3) path of the JAR file that contains the JDBC driver to use.
--
-- -   @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.
--
-- -   @JDBC_ENGINE@ - The name of the JDBC engine to use.
--
-- -   @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.
--
-- -   @CONFIG_FILES@ - (Reserved for future use.)
--
-- -   @INSTANCE_ID@ - The instance ID to use.
--
-- -   @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data
--     source.
--
-- -   @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying
--     whether Secure Sockets Layer (SSL) with hostname matching is
--     enforced for the JDBC connection on the client. The default is
--     false.
--
-- -   @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the
--     customer\'s root certificate. AWS Glue uses this root certificate to
--     validate the customer’s certificate when connecting to the customer
--     database. AWS Glue only handles X.509 certificates. The certificate
--     provided must be DER-encoded and supplied in Base64 encoding PEM
--     format.
--
-- -   @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@.
--     AWS Glue validates the Signature algorithm and Subject Public Key
--     Algorithm for the customer certificate. The only permitted
--     algorithms for the Signature algorithm are SHA256withRSA,
--     SHA384withRSA or SHA512withRSA. For the Subject Public Key
--     Algorithm, the key length must be at least 2048. You can set the
--     value of this property to @true@ to skip AWS Glue’s validation of
--     the customer certificate.
--
-- -   @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which
--     is used for domain match or distinguished name match to prevent a
--     man-in-the-middle attack. In Oracle database, this is used as the
--     @SSL_SERVER_CERT_DN@; in Microsoft SQL Server, this is used as the
--     @hostNameInCertificate@.
--
-- -   @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC)
--     data source.
--
-- -   @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port
--     pairs that are the addresses of the Apache Kafka brokers in a Kafka
--     cluster to which a Kafka client will connect to and bootstrap
--     itself.
--
-- -   @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache
--     Kafka connection. Default value is \"true\".
--
-- -   @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file
--     (.pem format). The default is an empty string.
--
-- -   @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation
--     of the CA cert file or not. AWS Glue validates for three algorithms:
--     SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is
--     \"false\".
--
-- -   @SECRET_ID@ - The secret ID used for the secret manager of
--     credentials.
--
-- -   @CONNECTOR_URL@ - The connector URL for a MARKETPLACE or CUSTOM
--     connection.
--
-- -   @CONNECTOR_TYPE@ - The connector type for a MARKETPLACE or CUSTOM
--     connection.
--
-- -   @CONNECTOR_CLASS_NAME@ - The connector class name for a MARKETPLACE
--     or CUSTOM connection.
--
-- 'creationTime', 'connection_creationTime' - The time that this connection definition was created.
--
-- 'connectionType', 'connection_connectionType' - The type of the connection. Currently, SFTP is not supported.
--
-- 'physicalConnectionRequirements', 'connection_physicalConnectionRequirements' - A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to make this connection
-- successfully.
--
-- 'name', 'connection_name' - The name of the connection definition.
--
-- 'lastUpdatedBy', 'connection_lastUpdatedBy' - The user, group, or role that last updated this connection definition.
--
-- 'description', 'connection_description' - The description of the connection.
--
-- 'matchCriteria', 'connection_matchCriteria' - A list of criteria that can be used in selecting this connection.
--
-- 'lastUpdatedTime', 'connection_lastUpdatedTime' - The last time that this connection definition was updated.
newConnection ::
  Connection
newConnection =
  Connection'
    { connectionProperties = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      physicalConnectionRequirements = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing,
      description = Prelude.Nothing,
      matchCriteria = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | These key-value pairs define parameters for the connection:
--
-- -   @HOST@ - The host URI: either the fully qualified domain name (FQDN)
--     or the IPv4 address of the database host.
--
-- -   @PORT@ - The port number, between 1024 and 65535, of the port on
--     which the database host is listening for database connections.
--
-- -   @USER_NAME@ - The name under which to log in to the database. The
--     value string for @USER_NAME@ is \"@USERNAME@\".
--
-- -   @PASSWORD@ - A password, if one is used, for the user name.
--
-- -   @ENCRYPTED_PASSWORD@ - When you enable connection password
--     protection by setting @ConnectionPasswordEncryption@ in the Data
--     Catalog encryption settings, this field stores the encrypted
--     password.
--
-- -   @JDBC_DRIVER_JAR_URI@ - The Amazon Simple Storage Service (Amazon
--     S3) path of the JAR file that contains the JDBC driver to use.
--
-- -   @JDBC_DRIVER_CLASS_NAME@ - The class name of the JDBC driver to use.
--
-- -   @JDBC_ENGINE@ - The name of the JDBC engine to use.
--
-- -   @JDBC_ENGINE_VERSION@ - The version of the JDBC engine to use.
--
-- -   @CONFIG_FILES@ - (Reserved for future use.)
--
-- -   @INSTANCE_ID@ - The instance ID to use.
--
-- -   @JDBC_CONNECTION_URL@ - The URL for connecting to a JDBC data
--     source.
--
-- -   @JDBC_ENFORCE_SSL@ - A Boolean string (true, false) specifying
--     whether Secure Sockets Layer (SSL) with hostname matching is
--     enforced for the JDBC connection on the client. The default is
--     false.
--
-- -   @CUSTOM_JDBC_CERT@ - An Amazon S3 location specifying the
--     customer\'s root certificate. AWS Glue uses this root certificate to
--     validate the customer’s certificate when connecting to the customer
--     database. AWS Glue only handles X.509 certificates. The certificate
--     provided must be DER-encoded and supplied in Base64 encoding PEM
--     format.
--
-- -   @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@.
--     AWS Glue validates the Signature algorithm and Subject Public Key
--     Algorithm for the customer certificate. The only permitted
--     algorithms for the Signature algorithm are SHA256withRSA,
--     SHA384withRSA or SHA512withRSA. For the Subject Public Key
--     Algorithm, the key length must be at least 2048. You can set the
--     value of this property to @true@ to skip AWS Glue’s validation of
--     the customer certificate.
--
-- -   @CUSTOM_JDBC_CERT_STRING@ - A custom JDBC certificate string which
--     is used for domain match or distinguished name match to prevent a
--     man-in-the-middle attack. In Oracle database, this is used as the
--     @SSL_SERVER_CERT_DN@; in Microsoft SQL Server, this is used as the
--     @hostNameInCertificate@.
--
-- -   @CONNECTION_URL@ - The URL for connecting to a general (non-JDBC)
--     data source.
--
-- -   @KAFKA_BOOTSTRAP_SERVERS@ - A comma-separated list of host and port
--     pairs that are the addresses of the Apache Kafka brokers in a Kafka
--     cluster to which a Kafka client will connect to and bootstrap
--     itself.
--
-- -   @KAFKA_SSL_ENABLED@ - Whether to enable or disable SSL on an Apache
--     Kafka connection. Default value is \"true\".
--
-- -   @KAFKA_CUSTOM_CERT@ - The Amazon S3 URL for the private CA cert file
--     (.pem format). The default is an empty string.
--
-- -   @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@ - Whether to skip the validation
--     of the CA cert file or not. AWS Glue validates for three algorithms:
--     SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is
--     \"false\".
--
-- -   @SECRET_ID@ - The secret ID used for the secret manager of
--     credentials.
--
-- -   @CONNECTOR_URL@ - The connector URL for a MARKETPLACE or CUSTOM
--     connection.
--
-- -   @CONNECTOR_TYPE@ - The connector type for a MARKETPLACE or CUSTOM
--     connection.
--
-- -   @CONNECTOR_CLASS_NAME@ - The connector class name for a MARKETPLACE
--     or CUSTOM connection.
connection_connectionProperties :: Lens.Lens' Connection (Prelude.Maybe (Prelude.HashMap ConnectionPropertyKey Prelude.Text))
connection_connectionProperties = Lens.lens (\Connection' {connectionProperties} -> connectionProperties) (\s@Connection' {} a -> s {connectionProperties = a} :: Connection) Prelude.. Lens.mapping Lens._Coerce

-- | The time that this connection definition was created.
connection_creationTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_creationTime = Lens.lens (\Connection' {creationTime} -> creationTime) (\s@Connection' {} a -> s {creationTime = a} :: Connection) Prelude.. Lens.mapping Core._Time

-- | The type of the connection. Currently, SFTP is not supported.
connection_connectionType :: Lens.Lens' Connection (Prelude.Maybe ConnectionType)
connection_connectionType = Lens.lens (\Connection' {connectionType} -> connectionType) (\s@Connection' {} a -> s {connectionType = a} :: Connection)

-- | A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to make this connection
-- successfully.
connection_physicalConnectionRequirements :: Lens.Lens' Connection (Prelude.Maybe PhysicalConnectionRequirements)
connection_physicalConnectionRequirements = Lens.lens (\Connection' {physicalConnectionRequirements} -> physicalConnectionRequirements) (\s@Connection' {} a -> s {physicalConnectionRequirements = a} :: Connection)

-- | The name of the connection definition.
connection_name :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_name = Lens.lens (\Connection' {name} -> name) (\s@Connection' {} a -> s {name = a} :: Connection)

-- | The user, group, or role that last updated this connection definition.
connection_lastUpdatedBy :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_lastUpdatedBy = Lens.lens (\Connection' {lastUpdatedBy} -> lastUpdatedBy) (\s@Connection' {} a -> s {lastUpdatedBy = a} :: Connection)

-- | The description of the connection.
connection_description :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_description = Lens.lens (\Connection' {description} -> description) (\s@Connection' {} a -> s {description = a} :: Connection)

-- | A list of criteria that can be used in selecting this connection.
connection_matchCriteria :: Lens.Lens' Connection (Prelude.Maybe [Prelude.Text])
connection_matchCriteria = Lens.lens (\Connection' {matchCriteria} -> matchCriteria) (\s@Connection' {} a -> s {matchCriteria = a} :: Connection) Prelude.. Lens.mapping Lens._Coerce

-- | The last time that this connection definition was updated.
connection_lastUpdatedTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_lastUpdatedTime = Lens.lens (\Connection' {lastUpdatedTime} -> lastUpdatedTime) (\s@Connection' {} a -> s {lastUpdatedTime = a} :: Connection) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> ( x Core..:? "ConnectionProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ConnectionType")
            Prelude.<*> (x Core..:? "PhysicalConnectionRequirements")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LastUpdatedBy")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "MatchCriteria" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastUpdatedTime")
      )

instance Prelude.Hashable Connection

instance Prelude.NFData Connection
