{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionPropertyKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionPropertyKey where

import Network.AWS.Prelude

data ConnectionPropertyKey
  = ConfigFiles
  | ConnectionURL
  | CustomJdbcCert
  | CustomJdbcCertString
  | EncryptedPassword
  | Host
  | InstanceId
  | JdbcConnectionURL
  | JdbcDriverClassName
  | JdbcDriverJARURI
  | JdbcEnforceSSL
  | JdbcEngine
  | JdbcEngineVersion
  | KafkaBootstrapServers
  | KafkaCustomCert
  | KafkaSSLEnabled
  | KafkaSkipCustomCertValidation
  | Password
  | Port
  | SkipCustomJdbcCertValidation
  | Username
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ConnectionPropertyKey where
  parser =
    takeLowerText >>= \case
      "config_files" -> pure ConfigFiles
      "connection_url" -> pure ConnectionURL
      "custom_jdbc_cert" -> pure CustomJdbcCert
      "custom_jdbc_cert_string" -> pure CustomJdbcCertString
      "encrypted_password" -> pure EncryptedPassword
      "host" -> pure Host
      "instance_id" -> pure InstanceId
      "jdbc_connection_url" -> pure JdbcConnectionURL
      "jdbc_driver_class_name" -> pure JdbcDriverClassName
      "jdbc_driver_jar_uri" -> pure JdbcDriverJARURI
      "jdbc_enforce_ssl" -> pure JdbcEnforceSSL
      "jdbc_engine" -> pure JdbcEngine
      "jdbc_engine_version" -> pure JdbcEngineVersion
      "kafka_bootstrap_servers" -> pure KafkaBootstrapServers
      "kafka_custom_cert" -> pure KafkaCustomCert
      "kafka_ssl_enabled" -> pure KafkaSSLEnabled
      "kafka_skip_custom_cert_validation" -> pure KafkaSkipCustomCertValidation
      "password" -> pure Password
      "port" -> pure Port
      "skip_custom_jdbc_cert_validation" -> pure SkipCustomJdbcCertValidation
      "username" -> pure Username
      e ->
        fromTextError $
          "Failure parsing ConnectionPropertyKey from value: '" <> e
            <> "'. Accepted values: config_files, connection_url, custom_jdbc_cert, custom_jdbc_cert_string, encrypted_password, host, instance_id, jdbc_connection_url, jdbc_driver_class_name, jdbc_driver_jar_uri, jdbc_enforce_ssl, jdbc_engine, jdbc_engine_version, kafka_bootstrap_servers, kafka_custom_cert, kafka_ssl_enabled, kafka_skip_custom_cert_validation, password, port, skip_custom_jdbc_cert_validation, username"

instance ToText ConnectionPropertyKey where
  toText = \case
    ConfigFiles -> "CONFIG_FILES"
    ConnectionURL -> "CONNECTION_URL"
    CustomJdbcCert -> "CUSTOM_JDBC_CERT"
    CustomJdbcCertString -> "CUSTOM_JDBC_CERT_STRING"
    EncryptedPassword -> "ENCRYPTED_PASSWORD"
    Host -> "HOST"
    InstanceId -> "INSTANCE_ID"
    JdbcConnectionURL -> "JDBC_CONNECTION_URL"
    JdbcDriverClassName -> "JDBC_DRIVER_CLASS_NAME"
    JdbcDriverJARURI -> "JDBC_DRIVER_JAR_URI"
    JdbcEnforceSSL -> "JDBC_ENFORCE_SSL"
    JdbcEngine -> "JDBC_ENGINE"
    JdbcEngineVersion -> "JDBC_ENGINE_VERSION"
    KafkaBootstrapServers -> "KAFKA_BOOTSTRAP_SERVERS"
    KafkaCustomCert -> "KAFKA_CUSTOM_CERT"
    KafkaSSLEnabled -> "KAFKA_SSL_ENABLED"
    KafkaSkipCustomCertValidation -> "KAFKA_SKIP_CUSTOM_CERT_VALIDATION"
    Password -> "PASSWORD"
    Port -> "PORT"
    SkipCustomJdbcCertValidation -> "SKIP_CUSTOM_JDBC_CERT_VALIDATION"
    Username -> "USERNAME"

instance Hashable ConnectionPropertyKey

instance NFData ConnectionPropertyKey

instance ToByteString ConnectionPropertyKey

instance ToQuery ConnectionPropertyKey

instance ToHeader ConnectionPropertyKey

instance ToJSON ConnectionPropertyKey where
  toJSON = toJSONText

instance FromJSON ConnectionPropertyKey where
  parseJSON = parseJSONText "ConnectionPropertyKey"
