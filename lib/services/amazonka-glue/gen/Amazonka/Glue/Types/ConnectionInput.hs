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
-- Module      : Amazonka.Glue.Types.ConnectionInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ConnectionInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ConnectionPropertyKey
import Amazonka.Glue.Types.ConnectionType
import Amazonka.Glue.Types.PhysicalConnectionRequirements
import qualified Amazonka.Prelude as Prelude

-- | A structure that is used to specify a connection to create or update.
--
-- /See:/ 'newConnectionInput' smart constructor.
data ConnectionInput = ConnectionInput'
  { -- | The description of the connection.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of criteria that can be used in selecting this connection.
    matchCriteria :: Prelude.Maybe [Prelude.Text],
    -- | A map of physical connection requirements, such as virtual private cloud
    -- (VPC) and @SecurityGroup@, that are needed to successfully make this
    -- connection.
    physicalConnectionRequirements :: Prelude.Maybe PhysicalConnectionRequirements,
    -- | The name of the connection. Connection will not function as expected
    -- without a name.
    name :: Prelude.Text,
    -- | The type of the connection. Currently, these types are supported:
    --
    -- -   @JDBC@ - Designates a connection to a database through Java Database
    --     Connectivity (JDBC).
    --
    --     @JDBC@ Connections use the following ConnectionParameters.
    --
    --     -   Required: All of (@HOST@, @PORT@, @JDBC_ENGINE@) or
    --         @JDBC_CONNECTION_URL@.
    --
    --     -   Required: All of (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
    --
    --     -   Optional: @JDBC_ENFORCE_SSL@, @CUSTOM_JDBC_CERT@,
    --         @CUSTOM_JDBC_CERT_STRING@, @SKIP_CUSTOM_JDBC_CERT_VALIDATION@.
    --         These parameters are used to configure SSL with JDBC.
    --
    -- -   @KAFKA@ - Designates a connection to an Apache Kafka streaming
    --     platform.
    --
    --     @KAFKA@ Connections use the following ConnectionParameters.
    --
    --     -   Required: @KAFKA_BOOTSTRAP_SERVERS@.
    --
    --     -   Optional: @KAFKA_SSL_ENABLED@, @KAFKA_CUSTOM_CERT@,
    --         @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@. These parameters are used
    --         to configure SSL with @KAFKA@.
    --
    --     -   Optional: @KAFKA_CLIENT_KEYSTORE@,
    --         @KAFKA_CLIENT_KEYSTORE_PASSWORD@, @KAFKA_CLIENT_KEY_PASSWORD@,
    --         @ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD@,
    --         @ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD@. These parameters are used
    --         to configure TLS client configuration with SSL in @KAFKA@.
    --
    --     -   Optional: @KAFKA_SASL_MECHANISM@. Can be specified as
    --         @SCRAM-SHA-512@, @GSSAPI@, or @AWS_MSK_IAM@.
    --
    --     -   Optional: @KAFKA_SASL_SCRAM_USERNAME@,
    --         @KAFKA_SASL_SCRAM_PASSWORD@,
    --         @ENCRYPTED_KAFKA_SASL_SCRAM_PASSWORD@. These parameters are used
    --         to configure SASL\/SCRAM-SHA-512 authentication with @KAFKA@.
    --
    --     -   Optional: @KAFKA_SASL_GSSAPI_KEYTAB@,
    --         @KAFKA_SASL_GSSAPI_KRB5_CONF@, @KAFKA_SASL_GSSAPI_SERVICE@,
    --         @KAFKA_SASL_GSSAPI_PRINCIPAL@. These parameters are used to
    --         configure SASL\/GSSAPI authentication with @KAFKA@.
    --
    -- -   @MONGODB@ - Designates a connection to a MongoDB document database.
    --
    --     @MONGODB@ Connections use the following ConnectionParameters.
    --
    --     -   Required: @CONNECTION_URL@.
    --
    --     -   Required: All of (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
    --
    -- -   @NETWORK@ - Designates a network connection to a data source within
    --     an Amazon Virtual Private Cloud environment (Amazon VPC).
    --
    --     @NETWORK@ Connections do not require ConnectionParameters. Instead,
    --     provide a PhysicalConnectionRequirements.
    --
    -- -   @MARKETPLACE@ - Uses configuration settings contained in a connector
    --     purchased from Amazon Web Services Marketplace to read from and
    --     write to data stores that are not natively supported by Glue.
    --
    --     @MARKETPLACE@ Connections use the following ConnectionParameters.
    --
    --     -   Required: @CONNECTOR_TYPE@, @CONNECTOR_URL@,
    --         @CONNECTOR_CLASS_NAME@, @CONNECTION_URL@.
    --
    --     -   Required for @JDBC@ @CONNECTOR_TYPE@ connections: All of
    --         (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
    --
    -- -   @CUSTOM@ - Uses configuration settings contained in a custom
    --     connector to read from and write to data stores that are not
    --     natively supported by Glue.
    --
    -- @SFTP@ is not supported.
    --
    -- For more information about how optional ConnectionProperties are used to
    -- configure features in Glue, consult
    -- <https://docs.aws.amazon.com/glue/latest/dg/connection-defining.html Glue connection properties>.
    --
    -- For more information about how optional ConnectionProperties are used to
    -- configure features in Glue Studio, consult
    -- <https://docs.aws.amazon.com/glue/latest/ug/connectors-chapter.html Using connectors and connections>.
    connectionType :: ConnectionType,
    -- | These key-value pairs define parameters for the connection.
    connectionProperties :: Prelude.HashMap ConnectionPropertyKey Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'connectionInput_description' - The description of the connection.
--
-- 'matchCriteria', 'connectionInput_matchCriteria' - A list of criteria that can be used in selecting this connection.
--
-- 'physicalConnectionRequirements', 'connectionInput_physicalConnectionRequirements' - A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to successfully make this
-- connection.
--
-- 'name', 'connectionInput_name' - The name of the connection. Connection will not function as expected
-- without a name.
--
-- 'connectionType', 'connectionInput_connectionType' - The type of the connection. Currently, these types are supported:
--
-- -   @JDBC@ - Designates a connection to a database through Java Database
--     Connectivity (JDBC).
--
--     @JDBC@ Connections use the following ConnectionParameters.
--
--     -   Required: All of (@HOST@, @PORT@, @JDBC_ENGINE@) or
--         @JDBC_CONNECTION_URL@.
--
--     -   Required: All of (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
--
--     -   Optional: @JDBC_ENFORCE_SSL@, @CUSTOM_JDBC_CERT@,
--         @CUSTOM_JDBC_CERT_STRING@, @SKIP_CUSTOM_JDBC_CERT_VALIDATION@.
--         These parameters are used to configure SSL with JDBC.
--
-- -   @KAFKA@ - Designates a connection to an Apache Kafka streaming
--     platform.
--
--     @KAFKA@ Connections use the following ConnectionParameters.
--
--     -   Required: @KAFKA_BOOTSTRAP_SERVERS@.
--
--     -   Optional: @KAFKA_SSL_ENABLED@, @KAFKA_CUSTOM_CERT@,
--         @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@. These parameters are used
--         to configure SSL with @KAFKA@.
--
--     -   Optional: @KAFKA_CLIENT_KEYSTORE@,
--         @KAFKA_CLIENT_KEYSTORE_PASSWORD@, @KAFKA_CLIENT_KEY_PASSWORD@,
--         @ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD@,
--         @ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD@. These parameters are used
--         to configure TLS client configuration with SSL in @KAFKA@.
--
--     -   Optional: @KAFKA_SASL_MECHANISM@. Can be specified as
--         @SCRAM-SHA-512@, @GSSAPI@, or @AWS_MSK_IAM@.
--
--     -   Optional: @KAFKA_SASL_SCRAM_USERNAME@,
--         @KAFKA_SASL_SCRAM_PASSWORD@,
--         @ENCRYPTED_KAFKA_SASL_SCRAM_PASSWORD@. These parameters are used
--         to configure SASL\/SCRAM-SHA-512 authentication with @KAFKA@.
--
--     -   Optional: @KAFKA_SASL_GSSAPI_KEYTAB@,
--         @KAFKA_SASL_GSSAPI_KRB5_CONF@, @KAFKA_SASL_GSSAPI_SERVICE@,
--         @KAFKA_SASL_GSSAPI_PRINCIPAL@. These parameters are used to
--         configure SASL\/GSSAPI authentication with @KAFKA@.
--
-- -   @MONGODB@ - Designates a connection to a MongoDB document database.
--
--     @MONGODB@ Connections use the following ConnectionParameters.
--
--     -   Required: @CONNECTION_URL@.
--
--     -   Required: All of (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
--
-- -   @NETWORK@ - Designates a network connection to a data source within
--     an Amazon Virtual Private Cloud environment (Amazon VPC).
--
--     @NETWORK@ Connections do not require ConnectionParameters. Instead,
--     provide a PhysicalConnectionRequirements.
--
-- -   @MARKETPLACE@ - Uses configuration settings contained in a connector
--     purchased from Amazon Web Services Marketplace to read from and
--     write to data stores that are not natively supported by Glue.
--
--     @MARKETPLACE@ Connections use the following ConnectionParameters.
--
--     -   Required: @CONNECTOR_TYPE@, @CONNECTOR_URL@,
--         @CONNECTOR_CLASS_NAME@, @CONNECTION_URL@.
--
--     -   Required for @JDBC@ @CONNECTOR_TYPE@ connections: All of
--         (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
--
-- -   @CUSTOM@ - Uses configuration settings contained in a custom
--     connector to read from and write to data stores that are not
--     natively supported by Glue.
--
-- @SFTP@ is not supported.
--
-- For more information about how optional ConnectionProperties are used to
-- configure features in Glue, consult
-- <https://docs.aws.amazon.com/glue/latest/dg/connection-defining.html Glue connection properties>.
--
-- For more information about how optional ConnectionProperties are used to
-- configure features in Glue Studio, consult
-- <https://docs.aws.amazon.com/glue/latest/ug/connectors-chapter.html Using connectors and connections>.
--
-- 'connectionProperties', 'connectionInput_connectionProperties' - These key-value pairs define parameters for the connection.
newConnectionInput ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectionType'
  ConnectionType ->
  ConnectionInput
newConnectionInput pName_ pConnectionType_ =
  ConnectionInput'
    { description = Prelude.Nothing,
      matchCriteria = Prelude.Nothing,
      physicalConnectionRequirements = Prelude.Nothing,
      name = pName_,
      connectionType = pConnectionType_,
      connectionProperties = Prelude.mempty
    }

-- | The description of the connection.
connectionInput_description :: Lens.Lens' ConnectionInput (Prelude.Maybe Prelude.Text)
connectionInput_description = Lens.lens (\ConnectionInput' {description} -> description) (\s@ConnectionInput' {} a -> s {description = a} :: ConnectionInput)

-- | A list of criteria that can be used in selecting this connection.
connectionInput_matchCriteria :: Lens.Lens' ConnectionInput (Prelude.Maybe [Prelude.Text])
connectionInput_matchCriteria = Lens.lens (\ConnectionInput' {matchCriteria} -> matchCriteria) (\s@ConnectionInput' {} a -> s {matchCriteria = a} :: ConnectionInput) Prelude.. Lens.mapping Lens.coerced

-- | A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to successfully make this
-- connection.
connectionInput_physicalConnectionRequirements :: Lens.Lens' ConnectionInput (Prelude.Maybe PhysicalConnectionRequirements)
connectionInput_physicalConnectionRequirements = Lens.lens (\ConnectionInput' {physicalConnectionRequirements} -> physicalConnectionRequirements) (\s@ConnectionInput' {} a -> s {physicalConnectionRequirements = a} :: ConnectionInput)

-- | The name of the connection. Connection will not function as expected
-- without a name.
connectionInput_name :: Lens.Lens' ConnectionInput Prelude.Text
connectionInput_name = Lens.lens (\ConnectionInput' {name} -> name) (\s@ConnectionInput' {} a -> s {name = a} :: ConnectionInput)

-- | The type of the connection. Currently, these types are supported:
--
-- -   @JDBC@ - Designates a connection to a database through Java Database
--     Connectivity (JDBC).
--
--     @JDBC@ Connections use the following ConnectionParameters.
--
--     -   Required: All of (@HOST@, @PORT@, @JDBC_ENGINE@) or
--         @JDBC_CONNECTION_URL@.
--
--     -   Required: All of (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
--
--     -   Optional: @JDBC_ENFORCE_SSL@, @CUSTOM_JDBC_CERT@,
--         @CUSTOM_JDBC_CERT_STRING@, @SKIP_CUSTOM_JDBC_CERT_VALIDATION@.
--         These parameters are used to configure SSL with JDBC.
--
-- -   @KAFKA@ - Designates a connection to an Apache Kafka streaming
--     platform.
--
--     @KAFKA@ Connections use the following ConnectionParameters.
--
--     -   Required: @KAFKA_BOOTSTRAP_SERVERS@.
--
--     -   Optional: @KAFKA_SSL_ENABLED@, @KAFKA_CUSTOM_CERT@,
--         @KAFKA_SKIP_CUSTOM_CERT_VALIDATION@. These parameters are used
--         to configure SSL with @KAFKA@.
--
--     -   Optional: @KAFKA_CLIENT_KEYSTORE@,
--         @KAFKA_CLIENT_KEYSTORE_PASSWORD@, @KAFKA_CLIENT_KEY_PASSWORD@,
--         @ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD@,
--         @ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD@. These parameters are used
--         to configure TLS client configuration with SSL in @KAFKA@.
--
--     -   Optional: @KAFKA_SASL_MECHANISM@. Can be specified as
--         @SCRAM-SHA-512@, @GSSAPI@, or @AWS_MSK_IAM@.
--
--     -   Optional: @KAFKA_SASL_SCRAM_USERNAME@,
--         @KAFKA_SASL_SCRAM_PASSWORD@,
--         @ENCRYPTED_KAFKA_SASL_SCRAM_PASSWORD@. These parameters are used
--         to configure SASL\/SCRAM-SHA-512 authentication with @KAFKA@.
--
--     -   Optional: @KAFKA_SASL_GSSAPI_KEYTAB@,
--         @KAFKA_SASL_GSSAPI_KRB5_CONF@, @KAFKA_SASL_GSSAPI_SERVICE@,
--         @KAFKA_SASL_GSSAPI_PRINCIPAL@. These parameters are used to
--         configure SASL\/GSSAPI authentication with @KAFKA@.
--
-- -   @MONGODB@ - Designates a connection to a MongoDB document database.
--
--     @MONGODB@ Connections use the following ConnectionParameters.
--
--     -   Required: @CONNECTION_URL@.
--
--     -   Required: All of (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
--
-- -   @NETWORK@ - Designates a network connection to a data source within
--     an Amazon Virtual Private Cloud environment (Amazon VPC).
--
--     @NETWORK@ Connections do not require ConnectionParameters. Instead,
--     provide a PhysicalConnectionRequirements.
--
-- -   @MARKETPLACE@ - Uses configuration settings contained in a connector
--     purchased from Amazon Web Services Marketplace to read from and
--     write to data stores that are not natively supported by Glue.
--
--     @MARKETPLACE@ Connections use the following ConnectionParameters.
--
--     -   Required: @CONNECTOR_TYPE@, @CONNECTOR_URL@,
--         @CONNECTOR_CLASS_NAME@, @CONNECTION_URL@.
--
--     -   Required for @JDBC@ @CONNECTOR_TYPE@ connections: All of
--         (@USERNAME@, @PASSWORD@) or @SECRET_ID@.
--
-- -   @CUSTOM@ - Uses configuration settings contained in a custom
--     connector to read from and write to data stores that are not
--     natively supported by Glue.
--
-- @SFTP@ is not supported.
--
-- For more information about how optional ConnectionProperties are used to
-- configure features in Glue, consult
-- <https://docs.aws.amazon.com/glue/latest/dg/connection-defining.html Glue connection properties>.
--
-- For more information about how optional ConnectionProperties are used to
-- configure features in Glue Studio, consult
-- <https://docs.aws.amazon.com/glue/latest/ug/connectors-chapter.html Using connectors and connections>.
connectionInput_connectionType :: Lens.Lens' ConnectionInput ConnectionType
connectionInput_connectionType = Lens.lens (\ConnectionInput' {connectionType} -> connectionType) (\s@ConnectionInput' {} a -> s {connectionType = a} :: ConnectionInput)

-- | These key-value pairs define parameters for the connection.
connectionInput_connectionProperties :: Lens.Lens' ConnectionInput (Prelude.HashMap ConnectionPropertyKey Prelude.Text)
connectionInput_connectionProperties = Lens.lens (\ConnectionInput' {connectionProperties} -> connectionProperties) (\s@ConnectionInput' {} a -> s {connectionProperties = a} :: ConnectionInput) Prelude.. Lens.coerced

instance Prelude.Hashable ConnectionInput where
  hashWithSalt _salt ConnectionInput' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` matchCriteria
      `Prelude.hashWithSalt` physicalConnectionRequirements
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` connectionProperties

instance Prelude.NFData ConnectionInput where
  rnf ConnectionInput' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf matchCriteria
      `Prelude.seq` Prelude.rnf physicalConnectionRequirements
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf connectionProperties

instance Data.ToJSON ConnectionInput where
  toJSON ConnectionInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("MatchCriteria" Data..=) Prelude.<$> matchCriteria,
            ("PhysicalConnectionRequirements" Data..=)
              Prelude.<$> physicalConnectionRequirements,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ConnectionType" Data..= connectionType),
            Prelude.Just
              ( "ConnectionProperties"
                  Data..= connectionProperties
              )
          ]
      )
