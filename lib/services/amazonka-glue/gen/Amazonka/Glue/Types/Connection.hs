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
-- Module      : Amazonka.Glue.Types.Connection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Connection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ConnectionPropertyKey
import Amazonka.Glue.Types.ConnectionType
import Amazonka.Glue.Types.PhysicalConnectionRequirements
import qualified Amazonka.Prelude as Prelude

-- | Defines a connection to a data source.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The name of the connection definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the connection. Currently, SFTP is not supported.
    connectionType :: Prelude.Maybe ConnectionType,
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
    --     customer\'s root certificate. Glue uses this root certificate to
    --     validate the customer’s certificate when connecting to the customer
    --     database. Glue only handles X.509 certificates. The certificate
    --     provided must be DER-encoded and supplied in Base64 encoding PEM
    --     format.
    --
    -- -   @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@.
    --     Glue validates the Signature algorithm and Subject Public Key
    --     Algorithm for the customer certificate. The only permitted
    --     algorithms for the Signature algorithm are SHA256withRSA,
    --     SHA384withRSA or SHA512withRSA. For the Subject Public Key
    --     Algorithm, the key length must be at least 2048. You can set the
    --     value of this property to @true@ to skip Glue’s validation of the
    --     customer certificate.
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
    --     of the CA cert file or not. Glue validates for three algorithms:
    --     SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is
    --     \"false\".
    --
    -- -   @KAFKA_CLIENT_KEYSTORE@ - The Amazon S3 location of the client
    --     keystore file for Kafka client side authentication (Optional).
    --
    -- -   @KAFKA_CLIENT_KEYSTORE_PASSWORD@ - The password to access the
    --     provided keystore (Optional).
    --
    -- -   @KAFKA_CLIENT_KEY_PASSWORD@ - A keystore can consist of multiple
    --     keys, so this is the password to access the client key to be used
    --     with the Kafka server side key (Optional).
    --
    -- -   @ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD@ - The encrypted version
    --     of the Kafka client keystore password (if the user has the Glue
    --     encrypt passwords setting selected).
    --
    -- -   @ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD@ - The encrypted version of the
    --     Kafka client key password (if the user has the Glue encrypt
    --     passwords setting selected).
    --
    -- -   @KAFKA_SASL_MECHANISM@ - @\"SCRAM-SHA-512\"@ or @\"GSSAPI\"@. These
    --     are the two supported
    --     <https://www.iana.org/assignments/sasl-mechanisms/sasl-mechanisms.xhtml SASL Mechanisms>.
    --
    -- -   @KAFKA_SASL_SCRAM_USERNAME@ - A plaintext username used to
    --     authenticate with the \"SCRAM-SHA-512\" mechanism.
    --
    -- -   @KAFKA_SASL_SCRAM_PASSWORD@ - A plaintext password used to
    --     authenticate with the \"SCRAM-SHA-512\" mechanism.
    --
    -- -   @ENCRYPTED_KAFKA_SASL_SCRAM_PASSWORD@ - The encrypted version of the
    --     Kafka SASL SCRAM password (if the user has the Glue encrypt
    --     passwords setting selected).
    --
    -- -   @KAFKA_SASL_GSSAPI_KEYTAB@ - The S3 location of a Kerberos @keytab@
    --     file. A keytab stores long-term keys for one or more principals. For
    --     more information, see
    --     <https://web.mit.edu/kerberos/krb5-latest/doc/basic/keytab_def.html MIT Kerberos Documentation: Keytab>.
    --
    -- -   @KAFKA_SASL_GSSAPI_KRB5_CONF@ - The S3 location of a Kerberos
    --     @krb5.conf@ file. A krb5.conf stores Kerberos configuration
    --     information, such as the location of the KDC server. For more
    --     information, see
    --     <https://web.mit.edu/kerberos/krb5-1.12/doc/admin/conf_files/krb5_conf.html MIT Kerberos Documentation: krb5.conf>.
    --
    -- -   @KAFKA_SASL_GSSAPI_SERVICE@ - The Kerberos service name, as set with
    --     @sasl.kerberos.service.name@ in your
    --     <https://kafka.apache.org/documentation/#brokerconfigs_sasl.kerberos.service.name Kafka Configuration>.
    --
    -- -   @KAFKA_SASL_GSSAPI_PRINCIPAL@ - The name of the Kerberos princial
    --     used by Glue. For more information, see
    --     <https://kafka.apache.org/documentation/#security_sasl_kerberos_clientconfig Kafka Documentation: Configuring Kafka Brokers>.
    connectionProperties :: Prelude.Maybe (Prelude.HashMap ConnectionPropertyKey Prelude.Text),
    -- | A map of physical connection requirements, such as virtual private cloud
    -- (VPC) and @SecurityGroup@, that are needed to make this connection
    -- successfully.
    physicalConnectionRequirements :: Prelude.Maybe PhysicalConnectionRequirements,
    -- | The last time that this connection definition was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the connection.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of criteria that can be used in selecting this connection.
    matchCriteria :: Prelude.Maybe [Prelude.Text],
    -- | The time that this connection definition was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The user, group, or role that last updated this connection definition.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text
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
-- 'name', 'connection_name' - The name of the connection definition.
--
-- 'connectionType', 'connection_connectionType' - The type of the connection. Currently, SFTP is not supported.
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
--     customer\'s root certificate. Glue uses this root certificate to
--     validate the customer’s certificate when connecting to the customer
--     database. Glue only handles X.509 certificates. The certificate
--     provided must be DER-encoded and supplied in Base64 encoding PEM
--     format.
--
-- -   @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@.
--     Glue validates the Signature algorithm and Subject Public Key
--     Algorithm for the customer certificate. The only permitted
--     algorithms for the Signature algorithm are SHA256withRSA,
--     SHA384withRSA or SHA512withRSA. For the Subject Public Key
--     Algorithm, the key length must be at least 2048. You can set the
--     value of this property to @true@ to skip Glue’s validation of the
--     customer certificate.
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
--     of the CA cert file or not. Glue validates for three algorithms:
--     SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is
--     \"false\".
--
-- -   @KAFKA_CLIENT_KEYSTORE@ - The Amazon S3 location of the client
--     keystore file for Kafka client side authentication (Optional).
--
-- -   @KAFKA_CLIENT_KEYSTORE_PASSWORD@ - The password to access the
--     provided keystore (Optional).
--
-- -   @KAFKA_CLIENT_KEY_PASSWORD@ - A keystore can consist of multiple
--     keys, so this is the password to access the client key to be used
--     with the Kafka server side key (Optional).
--
-- -   @ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD@ - The encrypted version
--     of the Kafka client keystore password (if the user has the Glue
--     encrypt passwords setting selected).
--
-- -   @ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD@ - The encrypted version of the
--     Kafka client key password (if the user has the Glue encrypt
--     passwords setting selected).
--
-- -   @KAFKA_SASL_MECHANISM@ - @\"SCRAM-SHA-512\"@ or @\"GSSAPI\"@. These
--     are the two supported
--     <https://www.iana.org/assignments/sasl-mechanisms/sasl-mechanisms.xhtml SASL Mechanisms>.
--
-- -   @KAFKA_SASL_SCRAM_USERNAME@ - A plaintext username used to
--     authenticate with the \"SCRAM-SHA-512\" mechanism.
--
-- -   @KAFKA_SASL_SCRAM_PASSWORD@ - A plaintext password used to
--     authenticate with the \"SCRAM-SHA-512\" mechanism.
--
-- -   @ENCRYPTED_KAFKA_SASL_SCRAM_PASSWORD@ - The encrypted version of the
--     Kafka SASL SCRAM password (if the user has the Glue encrypt
--     passwords setting selected).
--
-- -   @KAFKA_SASL_GSSAPI_KEYTAB@ - The S3 location of a Kerberos @keytab@
--     file. A keytab stores long-term keys for one or more principals. For
--     more information, see
--     <https://web.mit.edu/kerberos/krb5-latest/doc/basic/keytab_def.html MIT Kerberos Documentation: Keytab>.
--
-- -   @KAFKA_SASL_GSSAPI_KRB5_CONF@ - The S3 location of a Kerberos
--     @krb5.conf@ file. A krb5.conf stores Kerberos configuration
--     information, such as the location of the KDC server. For more
--     information, see
--     <https://web.mit.edu/kerberos/krb5-1.12/doc/admin/conf_files/krb5_conf.html MIT Kerberos Documentation: krb5.conf>.
--
-- -   @KAFKA_SASL_GSSAPI_SERVICE@ - The Kerberos service name, as set with
--     @sasl.kerberos.service.name@ in your
--     <https://kafka.apache.org/documentation/#brokerconfigs_sasl.kerberos.service.name Kafka Configuration>.
--
-- -   @KAFKA_SASL_GSSAPI_PRINCIPAL@ - The name of the Kerberos princial
--     used by Glue. For more information, see
--     <https://kafka.apache.org/documentation/#security_sasl_kerberos_clientconfig Kafka Documentation: Configuring Kafka Brokers>.
--
-- 'physicalConnectionRequirements', 'connection_physicalConnectionRequirements' - A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to make this connection
-- successfully.
--
-- 'lastUpdatedTime', 'connection_lastUpdatedTime' - The last time that this connection definition was updated.
--
-- 'description', 'connection_description' - The description of the connection.
--
-- 'matchCriteria', 'connection_matchCriteria' - A list of criteria that can be used in selecting this connection.
--
-- 'creationTime', 'connection_creationTime' - The time that this connection definition was created.
--
-- 'lastUpdatedBy', 'connection_lastUpdatedBy' - The user, group, or role that last updated this connection definition.
newConnection ::
  Connection
newConnection =
  Connection'
    { name = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      connectionProperties = Prelude.Nothing,
      physicalConnectionRequirements = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      description = Prelude.Nothing,
      matchCriteria = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing
    }

-- | The name of the connection definition.
connection_name :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_name = Lens.lens (\Connection' {name} -> name) (\s@Connection' {} a -> s {name = a} :: Connection)

-- | The type of the connection. Currently, SFTP is not supported.
connection_connectionType :: Lens.Lens' Connection (Prelude.Maybe ConnectionType)
connection_connectionType = Lens.lens (\Connection' {connectionType} -> connectionType) (\s@Connection' {} a -> s {connectionType = a} :: Connection)

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
--     customer\'s root certificate. Glue uses this root certificate to
--     validate the customer’s certificate when connecting to the customer
--     database. Glue only handles X.509 certificates. The certificate
--     provided must be DER-encoded and supplied in Base64 encoding PEM
--     format.
--
-- -   @SKIP_CUSTOM_JDBC_CERT_VALIDATION@ - By default, this is @false@.
--     Glue validates the Signature algorithm and Subject Public Key
--     Algorithm for the customer certificate. The only permitted
--     algorithms for the Signature algorithm are SHA256withRSA,
--     SHA384withRSA or SHA512withRSA. For the Subject Public Key
--     Algorithm, the key length must be at least 2048. You can set the
--     value of this property to @true@ to skip Glue’s validation of the
--     customer certificate.
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
--     of the CA cert file or not. Glue validates for three algorithms:
--     SHA256withRSA, SHA384withRSA and SHA512withRSA. Default value is
--     \"false\".
--
-- -   @KAFKA_CLIENT_KEYSTORE@ - The Amazon S3 location of the client
--     keystore file for Kafka client side authentication (Optional).
--
-- -   @KAFKA_CLIENT_KEYSTORE_PASSWORD@ - The password to access the
--     provided keystore (Optional).
--
-- -   @KAFKA_CLIENT_KEY_PASSWORD@ - A keystore can consist of multiple
--     keys, so this is the password to access the client key to be used
--     with the Kafka server side key (Optional).
--
-- -   @ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD@ - The encrypted version
--     of the Kafka client keystore password (if the user has the Glue
--     encrypt passwords setting selected).
--
-- -   @ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD@ - The encrypted version of the
--     Kafka client key password (if the user has the Glue encrypt
--     passwords setting selected).
--
-- -   @KAFKA_SASL_MECHANISM@ - @\"SCRAM-SHA-512\"@ or @\"GSSAPI\"@. These
--     are the two supported
--     <https://www.iana.org/assignments/sasl-mechanisms/sasl-mechanisms.xhtml SASL Mechanisms>.
--
-- -   @KAFKA_SASL_SCRAM_USERNAME@ - A plaintext username used to
--     authenticate with the \"SCRAM-SHA-512\" mechanism.
--
-- -   @KAFKA_SASL_SCRAM_PASSWORD@ - A plaintext password used to
--     authenticate with the \"SCRAM-SHA-512\" mechanism.
--
-- -   @ENCRYPTED_KAFKA_SASL_SCRAM_PASSWORD@ - The encrypted version of the
--     Kafka SASL SCRAM password (if the user has the Glue encrypt
--     passwords setting selected).
--
-- -   @KAFKA_SASL_GSSAPI_KEYTAB@ - The S3 location of a Kerberos @keytab@
--     file. A keytab stores long-term keys for one or more principals. For
--     more information, see
--     <https://web.mit.edu/kerberos/krb5-latest/doc/basic/keytab_def.html MIT Kerberos Documentation: Keytab>.
--
-- -   @KAFKA_SASL_GSSAPI_KRB5_CONF@ - The S3 location of a Kerberos
--     @krb5.conf@ file. A krb5.conf stores Kerberos configuration
--     information, such as the location of the KDC server. For more
--     information, see
--     <https://web.mit.edu/kerberos/krb5-1.12/doc/admin/conf_files/krb5_conf.html MIT Kerberos Documentation: krb5.conf>.
--
-- -   @KAFKA_SASL_GSSAPI_SERVICE@ - The Kerberos service name, as set with
--     @sasl.kerberos.service.name@ in your
--     <https://kafka.apache.org/documentation/#brokerconfigs_sasl.kerberos.service.name Kafka Configuration>.
--
-- -   @KAFKA_SASL_GSSAPI_PRINCIPAL@ - The name of the Kerberos princial
--     used by Glue. For more information, see
--     <https://kafka.apache.org/documentation/#security_sasl_kerberos_clientconfig Kafka Documentation: Configuring Kafka Brokers>.
connection_connectionProperties :: Lens.Lens' Connection (Prelude.Maybe (Prelude.HashMap ConnectionPropertyKey Prelude.Text))
connection_connectionProperties = Lens.lens (\Connection' {connectionProperties} -> connectionProperties) (\s@Connection' {} a -> s {connectionProperties = a} :: Connection) Prelude.. Lens.mapping Lens.coerced

-- | A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to make this connection
-- successfully.
connection_physicalConnectionRequirements :: Lens.Lens' Connection (Prelude.Maybe PhysicalConnectionRequirements)
connection_physicalConnectionRequirements = Lens.lens (\Connection' {physicalConnectionRequirements} -> physicalConnectionRequirements) (\s@Connection' {} a -> s {physicalConnectionRequirements = a} :: Connection)

-- | The last time that this connection definition was updated.
connection_lastUpdatedTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_lastUpdatedTime = Lens.lens (\Connection' {lastUpdatedTime} -> lastUpdatedTime) (\s@Connection' {} a -> s {lastUpdatedTime = a} :: Connection) Prelude.. Lens.mapping Data._Time

-- | The description of the connection.
connection_description :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_description = Lens.lens (\Connection' {description} -> description) (\s@Connection' {} a -> s {description = a} :: Connection)

-- | A list of criteria that can be used in selecting this connection.
connection_matchCriteria :: Lens.Lens' Connection (Prelude.Maybe [Prelude.Text])
connection_matchCriteria = Lens.lens (\Connection' {matchCriteria} -> matchCriteria) (\s@Connection' {} a -> s {matchCriteria = a} :: Connection) Prelude.. Lens.mapping Lens.coerced

-- | The time that this connection definition was created.
connection_creationTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_creationTime = Lens.lens (\Connection' {creationTime} -> creationTime) (\s@Connection' {} a -> s {creationTime = a} :: Connection) Prelude.. Lens.mapping Data._Time

-- | The user, group, or role that last updated this connection definition.
connection_lastUpdatedBy :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_lastUpdatedBy = Lens.lens (\Connection' {lastUpdatedBy} -> lastUpdatedBy) (\s@Connection' {} a -> s {lastUpdatedBy = a} :: Connection)

instance Data.FromJSON Connection where
  parseJSON =
    Data.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ConnectionType")
            Prelude.<*> ( x Data..:? "ConnectionProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PhysicalConnectionRequirements")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "MatchCriteria" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastUpdatedBy")
      )

instance Prelude.Hashable Connection where
  hashWithSalt _salt Connection' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` connectionProperties
      `Prelude.hashWithSalt` physicalConnectionRequirements
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` matchCriteria
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdatedBy

instance Prelude.NFData Connection where
  rnf Connection' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf connectionProperties
      `Prelude.seq` Prelude.rnf physicalConnectionRequirements
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf matchCriteria
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdatedBy
