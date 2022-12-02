{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.ConnectionPropertyKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ConnectionPropertyKey
  ( ConnectionPropertyKey
      ( ..,
        ConnectionPropertyKey_CONFIG_FILES,
        ConnectionPropertyKey_CONNECTION_URL,
        ConnectionPropertyKey_CONNECTOR_CLASS_NAME,
        ConnectionPropertyKey_CONNECTOR_TYPE,
        ConnectionPropertyKey_CONNECTOR_URL,
        ConnectionPropertyKey_CUSTOM_JDBC_CERT,
        ConnectionPropertyKey_CUSTOM_JDBC_CERT_STRING,
        ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD,
        ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD,
        ConnectionPropertyKey_ENCRYPTED_PASSWORD,
        ConnectionPropertyKey_HOST,
        ConnectionPropertyKey_INSTANCE_ID,
        ConnectionPropertyKey_JDBC_CONNECTION_URL,
        ConnectionPropertyKey_JDBC_DRIVER_CLASS_NAME,
        ConnectionPropertyKey_JDBC_DRIVER_JAR_URI,
        ConnectionPropertyKey_JDBC_ENFORCE_SSL,
        ConnectionPropertyKey_JDBC_ENGINE,
        ConnectionPropertyKey_JDBC_ENGINE_VERSION,
        ConnectionPropertyKey_KAFKA_BOOTSTRAP_SERVERS,
        ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE,
        ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE_PASSWORD,
        ConnectionPropertyKey_KAFKA_CLIENT_KEY_PASSWORD,
        ConnectionPropertyKey_KAFKA_CUSTOM_CERT,
        ConnectionPropertyKey_KAFKA_SKIP_CUSTOM_CERT_VALIDATION,
        ConnectionPropertyKey_KAFKA_SSL_ENABLED,
        ConnectionPropertyKey_PASSWORD,
        ConnectionPropertyKey_PORT,
        ConnectionPropertyKey_SECRET_ID,
        ConnectionPropertyKey_SKIP_CUSTOM_JDBC_CERT_VALIDATION,
        ConnectionPropertyKey_USERNAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionPropertyKey = ConnectionPropertyKey'
  { fromConnectionPropertyKey ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ConnectionPropertyKey_CONFIG_FILES :: ConnectionPropertyKey
pattern ConnectionPropertyKey_CONFIG_FILES = ConnectionPropertyKey' "CONFIG_FILES"

pattern ConnectionPropertyKey_CONNECTION_URL :: ConnectionPropertyKey
pattern ConnectionPropertyKey_CONNECTION_URL = ConnectionPropertyKey' "CONNECTION_URL"

pattern ConnectionPropertyKey_CONNECTOR_CLASS_NAME :: ConnectionPropertyKey
pattern ConnectionPropertyKey_CONNECTOR_CLASS_NAME = ConnectionPropertyKey' "CONNECTOR_CLASS_NAME"

pattern ConnectionPropertyKey_CONNECTOR_TYPE :: ConnectionPropertyKey
pattern ConnectionPropertyKey_CONNECTOR_TYPE = ConnectionPropertyKey' "CONNECTOR_TYPE"

pattern ConnectionPropertyKey_CONNECTOR_URL :: ConnectionPropertyKey
pattern ConnectionPropertyKey_CONNECTOR_URL = ConnectionPropertyKey' "CONNECTOR_URL"

pattern ConnectionPropertyKey_CUSTOM_JDBC_CERT :: ConnectionPropertyKey
pattern ConnectionPropertyKey_CUSTOM_JDBC_CERT = ConnectionPropertyKey' "CUSTOM_JDBC_CERT"

pattern ConnectionPropertyKey_CUSTOM_JDBC_CERT_STRING :: ConnectionPropertyKey
pattern ConnectionPropertyKey_CUSTOM_JDBC_CERT_STRING = ConnectionPropertyKey' "CUSTOM_JDBC_CERT_STRING"

pattern ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD :: ConnectionPropertyKey
pattern ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD = ConnectionPropertyKey' "ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD"

pattern ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD :: ConnectionPropertyKey
pattern ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD = ConnectionPropertyKey' "ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD"

pattern ConnectionPropertyKey_ENCRYPTED_PASSWORD :: ConnectionPropertyKey
pattern ConnectionPropertyKey_ENCRYPTED_PASSWORD = ConnectionPropertyKey' "ENCRYPTED_PASSWORD"

pattern ConnectionPropertyKey_HOST :: ConnectionPropertyKey
pattern ConnectionPropertyKey_HOST = ConnectionPropertyKey' "HOST"

pattern ConnectionPropertyKey_INSTANCE_ID :: ConnectionPropertyKey
pattern ConnectionPropertyKey_INSTANCE_ID = ConnectionPropertyKey' "INSTANCE_ID"

pattern ConnectionPropertyKey_JDBC_CONNECTION_URL :: ConnectionPropertyKey
pattern ConnectionPropertyKey_JDBC_CONNECTION_URL = ConnectionPropertyKey' "JDBC_CONNECTION_URL"

pattern ConnectionPropertyKey_JDBC_DRIVER_CLASS_NAME :: ConnectionPropertyKey
pattern ConnectionPropertyKey_JDBC_DRIVER_CLASS_NAME = ConnectionPropertyKey' "JDBC_DRIVER_CLASS_NAME"

pattern ConnectionPropertyKey_JDBC_DRIVER_JAR_URI :: ConnectionPropertyKey
pattern ConnectionPropertyKey_JDBC_DRIVER_JAR_URI = ConnectionPropertyKey' "JDBC_DRIVER_JAR_URI"

pattern ConnectionPropertyKey_JDBC_ENFORCE_SSL :: ConnectionPropertyKey
pattern ConnectionPropertyKey_JDBC_ENFORCE_SSL = ConnectionPropertyKey' "JDBC_ENFORCE_SSL"

pattern ConnectionPropertyKey_JDBC_ENGINE :: ConnectionPropertyKey
pattern ConnectionPropertyKey_JDBC_ENGINE = ConnectionPropertyKey' "JDBC_ENGINE"

pattern ConnectionPropertyKey_JDBC_ENGINE_VERSION :: ConnectionPropertyKey
pattern ConnectionPropertyKey_JDBC_ENGINE_VERSION = ConnectionPropertyKey' "JDBC_ENGINE_VERSION"

pattern ConnectionPropertyKey_KAFKA_BOOTSTRAP_SERVERS :: ConnectionPropertyKey
pattern ConnectionPropertyKey_KAFKA_BOOTSTRAP_SERVERS = ConnectionPropertyKey' "KAFKA_BOOTSTRAP_SERVERS"

pattern ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE :: ConnectionPropertyKey
pattern ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE = ConnectionPropertyKey' "KAFKA_CLIENT_KEYSTORE"

pattern ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE_PASSWORD :: ConnectionPropertyKey
pattern ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE_PASSWORD = ConnectionPropertyKey' "KAFKA_CLIENT_KEYSTORE_PASSWORD"

pattern ConnectionPropertyKey_KAFKA_CLIENT_KEY_PASSWORD :: ConnectionPropertyKey
pattern ConnectionPropertyKey_KAFKA_CLIENT_KEY_PASSWORD = ConnectionPropertyKey' "KAFKA_CLIENT_KEY_PASSWORD"

pattern ConnectionPropertyKey_KAFKA_CUSTOM_CERT :: ConnectionPropertyKey
pattern ConnectionPropertyKey_KAFKA_CUSTOM_CERT = ConnectionPropertyKey' "KAFKA_CUSTOM_CERT"

pattern ConnectionPropertyKey_KAFKA_SKIP_CUSTOM_CERT_VALIDATION :: ConnectionPropertyKey
pattern ConnectionPropertyKey_KAFKA_SKIP_CUSTOM_CERT_VALIDATION = ConnectionPropertyKey' "KAFKA_SKIP_CUSTOM_CERT_VALIDATION"

pattern ConnectionPropertyKey_KAFKA_SSL_ENABLED :: ConnectionPropertyKey
pattern ConnectionPropertyKey_KAFKA_SSL_ENABLED = ConnectionPropertyKey' "KAFKA_SSL_ENABLED"

pattern ConnectionPropertyKey_PASSWORD :: ConnectionPropertyKey
pattern ConnectionPropertyKey_PASSWORD = ConnectionPropertyKey' "PASSWORD"

pattern ConnectionPropertyKey_PORT :: ConnectionPropertyKey
pattern ConnectionPropertyKey_PORT = ConnectionPropertyKey' "PORT"

pattern ConnectionPropertyKey_SECRET_ID :: ConnectionPropertyKey
pattern ConnectionPropertyKey_SECRET_ID = ConnectionPropertyKey' "SECRET_ID"

pattern ConnectionPropertyKey_SKIP_CUSTOM_JDBC_CERT_VALIDATION :: ConnectionPropertyKey
pattern ConnectionPropertyKey_SKIP_CUSTOM_JDBC_CERT_VALIDATION = ConnectionPropertyKey' "SKIP_CUSTOM_JDBC_CERT_VALIDATION"

pattern ConnectionPropertyKey_USERNAME :: ConnectionPropertyKey
pattern ConnectionPropertyKey_USERNAME = ConnectionPropertyKey' "USERNAME"

{-# COMPLETE
  ConnectionPropertyKey_CONFIG_FILES,
  ConnectionPropertyKey_CONNECTION_URL,
  ConnectionPropertyKey_CONNECTOR_CLASS_NAME,
  ConnectionPropertyKey_CONNECTOR_TYPE,
  ConnectionPropertyKey_CONNECTOR_URL,
  ConnectionPropertyKey_CUSTOM_JDBC_CERT,
  ConnectionPropertyKey_CUSTOM_JDBC_CERT_STRING,
  ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEYSTORE_PASSWORD,
  ConnectionPropertyKey_ENCRYPTED_KAFKA_CLIENT_KEY_PASSWORD,
  ConnectionPropertyKey_ENCRYPTED_PASSWORD,
  ConnectionPropertyKey_HOST,
  ConnectionPropertyKey_INSTANCE_ID,
  ConnectionPropertyKey_JDBC_CONNECTION_URL,
  ConnectionPropertyKey_JDBC_DRIVER_CLASS_NAME,
  ConnectionPropertyKey_JDBC_DRIVER_JAR_URI,
  ConnectionPropertyKey_JDBC_ENFORCE_SSL,
  ConnectionPropertyKey_JDBC_ENGINE,
  ConnectionPropertyKey_JDBC_ENGINE_VERSION,
  ConnectionPropertyKey_KAFKA_BOOTSTRAP_SERVERS,
  ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE,
  ConnectionPropertyKey_KAFKA_CLIENT_KEYSTORE_PASSWORD,
  ConnectionPropertyKey_KAFKA_CLIENT_KEY_PASSWORD,
  ConnectionPropertyKey_KAFKA_CUSTOM_CERT,
  ConnectionPropertyKey_KAFKA_SKIP_CUSTOM_CERT_VALIDATION,
  ConnectionPropertyKey_KAFKA_SSL_ENABLED,
  ConnectionPropertyKey_PASSWORD,
  ConnectionPropertyKey_PORT,
  ConnectionPropertyKey_SECRET_ID,
  ConnectionPropertyKey_SKIP_CUSTOM_JDBC_CERT_VALIDATION,
  ConnectionPropertyKey_USERNAME,
  ConnectionPropertyKey'
  #-}
