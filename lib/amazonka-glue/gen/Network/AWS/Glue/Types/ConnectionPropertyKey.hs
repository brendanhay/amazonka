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
        Host,
        Port,
        Username,
        Password,
        EncryptedPassword,
        JdbcDriverJARURI,
        JdbcDriverClassName,
        JdbcEngine,
        JdbcEngineVersion,
        ConfigFiles,
        InstanceId,
        JdbcConnectionURL,
        JdbcEnforceSSL,
        CustomJdbcCert,
        SkipCustomJdbcCertValidation,
        CustomJdbcCertString,
        ConnectionURL,
        KafkaBootstrapServers,
        KafkaSSLEnabled,
        KafkaCustomCert,
        KafkaSkipCustomCertValidation
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConnectionPropertyKey = ConnectionPropertyKey' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Host :: ConnectionPropertyKey
pattern Host = ConnectionPropertyKey' "HOST"

pattern Port :: ConnectionPropertyKey
pattern Port = ConnectionPropertyKey' "PORT"

pattern Username :: ConnectionPropertyKey
pattern Username = ConnectionPropertyKey' "USERNAME"

pattern Password :: ConnectionPropertyKey
pattern Password = ConnectionPropertyKey' "PASSWORD"

pattern EncryptedPassword :: ConnectionPropertyKey
pattern EncryptedPassword = ConnectionPropertyKey' "ENCRYPTED_PASSWORD"

pattern JdbcDriverJARURI :: ConnectionPropertyKey
pattern JdbcDriverJARURI = ConnectionPropertyKey' "JDBC_DRIVER_JAR_URI"

pattern JdbcDriverClassName :: ConnectionPropertyKey
pattern JdbcDriverClassName = ConnectionPropertyKey' "JDBC_DRIVER_CLASS_NAME"

pattern JdbcEngine :: ConnectionPropertyKey
pattern JdbcEngine = ConnectionPropertyKey' "JDBC_ENGINE"

pattern JdbcEngineVersion :: ConnectionPropertyKey
pattern JdbcEngineVersion = ConnectionPropertyKey' "JDBC_ENGINE_VERSION"

pattern ConfigFiles :: ConnectionPropertyKey
pattern ConfigFiles = ConnectionPropertyKey' "CONFIG_FILES"

pattern InstanceId :: ConnectionPropertyKey
pattern InstanceId = ConnectionPropertyKey' "INSTANCE_ID"

pattern JdbcConnectionURL :: ConnectionPropertyKey
pattern JdbcConnectionURL = ConnectionPropertyKey' "JDBC_CONNECTION_URL"

pattern JdbcEnforceSSL :: ConnectionPropertyKey
pattern JdbcEnforceSSL = ConnectionPropertyKey' "JDBC_ENFORCE_SSL"

pattern CustomJdbcCert :: ConnectionPropertyKey
pattern CustomJdbcCert = ConnectionPropertyKey' "CUSTOM_JDBC_CERT"

pattern SkipCustomJdbcCertValidation :: ConnectionPropertyKey
pattern SkipCustomJdbcCertValidation = ConnectionPropertyKey' "SKIP_CUSTOM_JDBC_CERT_VALIDATION"

pattern CustomJdbcCertString :: ConnectionPropertyKey
pattern CustomJdbcCertString = ConnectionPropertyKey' "CUSTOM_JDBC_CERT_STRING"

pattern ConnectionURL :: ConnectionPropertyKey
pattern ConnectionURL = ConnectionPropertyKey' "CONNECTION_URL"

pattern KafkaBootstrapServers :: ConnectionPropertyKey
pattern KafkaBootstrapServers = ConnectionPropertyKey' "KAFKA_BOOTSTRAP_SERVERS"

pattern KafkaSSLEnabled :: ConnectionPropertyKey
pattern KafkaSSLEnabled = ConnectionPropertyKey' "KAFKA_SSL_ENABLED"

pattern KafkaCustomCert :: ConnectionPropertyKey
pattern KafkaCustomCert = ConnectionPropertyKey' "KAFKA_CUSTOM_CERT"

pattern KafkaSkipCustomCertValidation :: ConnectionPropertyKey
pattern KafkaSkipCustomCertValidation = ConnectionPropertyKey' "KAFKA_SKIP_CUSTOM_CERT_VALIDATION"

{-# COMPLETE
  Host,
  Port,
  Username,
  Password,
  EncryptedPassword,
  JdbcDriverJARURI,
  JdbcDriverClassName,
  JdbcEngine,
  JdbcEngineVersion,
  ConfigFiles,
  InstanceId,
  JdbcConnectionURL,
  JdbcEnforceSSL,
  CustomJdbcCert,
  SkipCustomJdbcCertValidation,
  CustomJdbcCertString,
  ConnectionURL,
  KafkaBootstrapServers,
  KafkaSSLEnabled,
  KafkaCustomCert,
  KafkaSkipCustomCertValidation,
  ConnectionPropertyKey'
  #-}
