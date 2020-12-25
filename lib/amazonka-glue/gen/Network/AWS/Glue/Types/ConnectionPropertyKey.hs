{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionPropertyKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionPropertyKey
  ( ConnectionPropertyKey
      ( ConnectionPropertyKey',
        ConnectionPropertyKeyHost,
        ConnectionPropertyKeyPort,
        ConnectionPropertyKeyUsername,
        ConnectionPropertyKeyPassword,
        ConnectionPropertyKeyEncryptedPassword,
        ConnectionPropertyKeyJdbcDriverJarUri,
        ConnectionPropertyKeyJdbcDriverClassName,
        ConnectionPropertyKeyJdbcEngine,
        ConnectionPropertyKeyJdbcEngineVersion,
        ConnectionPropertyKeyConfigFiles,
        ConnectionPropertyKeyInstanceId,
        ConnectionPropertyKeyJdbcConnectionUrl,
        ConnectionPropertyKeyJdbcEnforceSsl,
        ConnectionPropertyKeyCustomJdbcCert,
        ConnectionPropertyKeySkipCustomJdbcCertValidation,
        ConnectionPropertyKeyCustomJdbcCertString,
        ConnectionPropertyKeyConnectionUrl,
        ConnectionPropertyKeyKafkaBootstrapServers,
        ConnectionPropertyKeyKafkaSslEnabled,
        ConnectionPropertyKeyKafkaCustomCert,
        ConnectionPropertyKeyKafkaSkipCustomCertValidation,
        fromConnectionPropertyKey
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConnectionPropertyKey = ConnectionPropertyKey'
  { fromConnectionPropertyKey ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ConnectionPropertyKeyHost :: ConnectionPropertyKey
pattern ConnectionPropertyKeyHost = ConnectionPropertyKey' "HOST"

pattern ConnectionPropertyKeyPort :: ConnectionPropertyKey
pattern ConnectionPropertyKeyPort = ConnectionPropertyKey' "PORT"

pattern ConnectionPropertyKeyUsername :: ConnectionPropertyKey
pattern ConnectionPropertyKeyUsername = ConnectionPropertyKey' "USERNAME"

pattern ConnectionPropertyKeyPassword :: ConnectionPropertyKey
pattern ConnectionPropertyKeyPassword = ConnectionPropertyKey' "PASSWORD"

pattern ConnectionPropertyKeyEncryptedPassword :: ConnectionPropertyKey
pattern ConnectionPropertyKeyEncryptedPassword = ConnectionPropertyKey' "ENCRYPTED_PASSWORD"

pattern ConnectionPropertyKeyJdbcDriverJarUri :: ConnectionPropertyKey
pattern ConnectionPropertyKeyJdbcDriverJarUri = ConnectionPropertyKey' "JDBC_DRIVER_JAR_URI"

pattern ConnectionPropertyKeyJdbcDriverClassName :: ConnectionPropertyKey
pattern ConnectionPropertyKeyJdbcDriverClassName = ConnectionPropertyKey' "JDBC_DRIVER_CLASS_NAME"

pattern ConnectionPropertyKeyJdbcEngine :: ConnectionPropertyKey
pattern ConnectionPropertyKeyJdbcEngine = ConnectionPropertyKey' "JDBC_ENGINE"

pattern ConnectionPropertyKeyJdbcEngineVersion :: ConnectionPropertyKey
pattern ConnectionPropertyKeyJdbcEngineVersion = ConnectionPropertyKey' "JDBC_ENGINE_VERSION"

pattern ConnectionPropertyKeyConfigFiles :: ConnectionPropertyKey
pattern ConnectionPropertyKeyConfigFiles = ConnectionPropertyKey' "CONFIG_FILES"

pattern ConnectionPropertyKeyInstanceId :: ConnectionPropertyKey
pattern ConnectionPropertyKeyInstanceId = ConnectionPropertyKey' "INSTANCE_ID"

pattern ConnectionPropertyKeyJdbcConnectionUrl :: ConnectionPropertyKey
pattern ConnectionPropertyKeyJdbcConnectionUrl = ConnectionPropertyKey' "JDBC_CONNECTION_URL"

pattern ConnectionPropertyKeyJdbcEnforceSsl :: ConnectionPropertyKey
pattern ConnectionPropertyKeyJdbcEnforceSsl = ConnectionPropertyKey' "JDBC_ENFORCE_SSL"

pattern ConnectionPropertyKeyCustomJdbcCert :: ConnectionPropertyKey
pattern ConnectionPropertyKeyCustomJdbcCert = ConnectionPropertyKey' "CUSTOM_JDBC_CERT"

pattern ConnectionPropertyKeySkipCustomJdbcCertValidation :: ConnectionPropertyKey
pattern ConnectionPropertyKeySkipCustomJdbcCertValidation = ConnectionPropertyKey' "SKIP_CUSTOM_JDBC_CERT_VALIDATION"

pattern ConnectionPropertyKeyCustomJdbcCertString :: ConnectionPropertyKey
pattern ConnectionPropertyKeyCustomJdbcCertString = ConnectionPropertyKey' "CUSTOM_JDBC_CERT_STRING"

pattern ConnectionPropertyKeyConnectionUrl :: ConnectionPropertyKey
pattern ConnectionPropertyKeyConnectionUrl = ConnectionPropertyKey' "CONNECTION_URL"

pattern ConnectionPropertyKeyKafkaBootstrapServers :: ConnectionPropertyKey
pattern ConnectionPropertyKeyKafkaBootstrapServers = ConnectionPropertyKey' "KAFKA_BOOTSTRAP_SERVERS"

pattern ConnectionPropertyKeyKafkaSslEnabled :: ConnectionPropertyKey
pattern ConnectionPropertyKeyKafkaSslEnabled = ConnectionPropertyKey' "KAFKA_SSL_ENABLED"

pattern ConnectionPropertyKeyKafkaCustomCert :: ConnectionPropertyKey
pattern ConnectionPropertyKeyKafkaCustomCert = ConnectionPropertyKey' "KAFKA_CUSTOM_CERT"

pattern ConnectionPropertyKeyKafkaSkipCustomCertValidation :: ConnectionPropertyKey
pattern ConnectionPropertyKeyKafkaSkipCustomCertValidation = ConnectionPropertyKey' "KAFKA_SKIP_CUSTOM_CERT_VALIDATION"

{-# COMPLETE
  ConnectionPropertyKeyHost,
  ConnectionPropertyKeyPort,
  ConnectionPropertyKeyUsername,
  ConnectionPropertyKeyPassword,
  ConnectionPropertyKeyEncryptedPassword,
  ConnectionPropertyKeyJdbcDriverJarUri,
  ConnectionPropertyKeyJdbcDriverClassName,
  ConnectionPropertyKeyJdbcEngine,
  ConnectionPropertyKeyJdbcEngineVersion,
  ConnectionPropertyKeyConfigFiles,
  ConnectionPropertyKeyInstanceId,
  ConnectionPropertyKeyJdbcConnectionUrl,
  ConnectionPropertyKeyJdbcEnforceSsl,
  ConnectionPropertyKeyCustomJdbcCert,
  ConnectionPropertyKeySkipCustomJdbcCertValidation,
  ConnectionPropertyKeyCustomJdbcCertString,
  ConnectionPropertyKeyConnectionUrl,
  ConnectionPropertyKeyKafkaBootstrapServers,
  ConnectionPropertyKeyKafkaSslEnabled,
  ConnectionPropertyKeyKafkaCustomCert,
  ConnectionPropertyKeyKafkaSkipCustomCertValidation,
  ConnectionPropertyKey'
  #-}
