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
        ConfigFiles,
        ConnectionURL,
        CustomJdbcCert,
        CustomJdbcCertString,
        EncryptedPassword,
        Host,
        InstanceId,
        JdbcConnectionURL,
        JdbcDriverClassName,
        JdbcDriverJARURI,
        JdbcEnforceSSL,
        JdbcEngine,
        JdbcEngineVersion,
        KafkaBootstrapServers,
        KafkaCustomCert,
        KafkaSSLEnabled,
        KafkaSkipCustomCertValidation,
        Password,
        Port,
        SkipCustomJdbcCertValidation,
        Username
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

pattern ConfigFiles :: ConnectionPropertyKey
pattern ConfigFiles = ConnectionPropertyKey' "CONFIG_FILES"

pattern ConnectionURL :: ConnectionPropertyKey
pattern ConnectionURL = ConnectionPropertyKey' "CONNECTION_URL"

pattern CustomJdbcCert :: ConnectionPropertyKey
pattern CustomJdbcCert = ConnectionPropertyKey' "CUSTOM_JDBC_CERT"

pattern CustomJdbcCertString :: ConnectionPropertyKey
pattern CustomJdbcCertString = ConnectionPropertyKey' "CUSTOM_JDBC_CERT_STRING"

pattern EncryptedPassword :: ConnectionPropertyKey
pattern EncryptedPassword = ConnectionPropertyKey' "ENCRYPTED_PASSWORD"

pattern Host :: ConnectionPropertyKey
pattern Host = ConnectionPropertyKey' "HOST"

pattern InstanceId :: ConnectionPropertyKey
pattern InstanceId = ConnectionPropertyKey' "INSTANCE_ID"

pattern JdbcConnectionURL :: ConnectionPropertyKey
pattern JdbcConnectionURL = ConnectionPropertyKey' "JDBC_CONNECTION_URL"

pattern JdbcDriverClassName :: ConnectionPropertyKey
pattern JdbcDriverClassName = ConnectionPropertyKey' "JDBC_DRIVER_CLASS_NAME"

pattern JdbcDriverJARURI :: ConnectionPropertyKey
pattern JdbcDriverJARURI = ConnectionPropertyKey' "JDBC_DRIVER_JAR_URI"

pattern JdbcEnforceSSL :: ConnectionPropertyKey
pattern JdbcEnforceSSL = ConnectionPropertyKey' "JDBC_ENFORCE_SSL"

pattern JdbcEngine :: ConnectionPropertyKey
pattern JdbcEngine = ConnectionPropertyKey' "JDBC_ENGINE"

pattern JdbcEngineVersion :: ConnectionPropertyKey
pattern JdbcEngineVersion = ConnectionPropertyKey' "JDBC_ENGINE_VERSION"

pattern KafkaBootstrapServers :: ConnectionPropertyKey
pattern KafkaBootstrapServers = ConnectionPropertyKey' "KAFKA_BOOTSTRAP_SERVERS"

pattern KafkaCustomCert :: ConnectionPropertyKey
pattern KafkaCustomCert = ConnectionPropertyKey' "KAFKA_CUSTOM_CERT"

pattern KafkaSSLEnabled :: ConnectionPropertyKey
pattern KafkaSSLEnabled = ConnectionPropertyKey' "KAFKA_SSL_ENABLED"

pattern KafkaSkipCustomCertValidation :: ConnectionPropertyKey
pattern KafkaSkipCustomCertValidation = ConnectionPropertyKey' "KAFKA_SKIP_CUSTOM_CERT_VALIDATION"

pattern Password :: ConnectionPropertyKey
pattern Password = ConnectionPropertyKey' "PASSWORD"

pattern Port :: ConnectionPropertyKey
pattern Port = ConnectionPropertyKey' "PORT"

pattern SkipCustomJdbcCertValidation :: ConnectionPropertyKey
pattern SkipCustomJdbcCertValidation = ConnectionPropertyKey' "SKIP_CUSTOM_JDBC_CERT_VALIDATION"

pattern Username :: ConnectionPropertyKey
pattern Username = ConnectionPropertyKey' "USERNAME"

{-# COMPLETE
  ConfigFiles,
  ConnectionURL,
  CustomJdbcCert,
  CustomJdbcCertString,
  EncryptedPassword,
  Host,
  InstanceId,
  JdbcConnectionURL,
  JdbcDriverClassName,
  JdbcDriverJARURI,
  JdbcEnforceSSL,
  JdbcEngine,
  JdbcEngineVersion,
  KafkaBootstrapServers,
  KafkaCustomCert,
  KafkaSSLEnabled,
  KafkaSkipCustomCertValidation,
  Password,
  Port,
  SkipCustomJdbcCertValidation,
  Username,
  ConnectionPropertyKey'
  #-}
