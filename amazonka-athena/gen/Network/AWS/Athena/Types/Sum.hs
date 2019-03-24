{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.Sum where

import Network.AWS.Prelude

data ColumnNullable
  = NotNull
  | Nullable
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ColumnNullable where
    parser = takeLowerText >>= \case
        "not_null" -> pure NotNull
        "nullable" -> pure Nullable
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing ColumnNullable from value: '" <> e
           <> "'. Accepted values: not_null, nullable, unknown"

instance ToText ColumnNullable where
    toText = \case
        NotNull -> "NOT_NULL"
        Nullable -> "NULLABLE"
        Unknown -> "UNKNOWN"

instance Hashable     ColumnNullable
instance NFData       ColumnNullable
instance ToByteString ColumnNullable
instance ToQuery      ColumnNullable
instance ToHeader     ColumnNullable

instance FromJSON ColumnNullable where
    parseJSON = parseJSONText "ColumnNullable"

data EncryptionOption
  = CseKMS
  | SseKMS
  | SseS3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncryptionOption where
    parser = takeLowerText >>= \case
        "cse_kms" -> pure CseKMS
        "sse_kms" -> pure SseKMS
        "sse_s3" -> pure SseS3
        e -> fromTextError $ "Failure parsing EncryptionOption from value: '" <> e
           <> "'. Accepted values: cse_kms, sse_kms, sse_s3"

instance ToText EncryptionOption where
    toText = \case
        CseKMS -> "CSE_KMS"
        SseKMS -> "SSE_KMS"
        SseS3 -> "SSE_S3"

instance Hashable     EncryptionOption
instance NFData       EncryptionOption
instance ToByteString EncryptionOption
instance ToQuery      EncryptionOption
instance ToHeader     EncryptionOption

instance ToJSON EncryptionOption where
    toJSON = toJSONText

instance FromJSON EncryptionOption where
    parseJSON = parseJSONText "EncryptionOption"

data QueryExecutionState
  = Cancelled
  | Failed
  | Queued
  | Running
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QueryExecutionState where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "failed" -> pure Failed
        "queued" -> pure Queued
        "running" -> pure Running
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing QueryExecutionState from value: '" <> e
           <> "'. Accepted values: cancelled, failed, queued, running, succeeded"

instance ToText QueryExecutionState where
    toText = \case
        Cancelled -> "CANCELLED"
        Failed -> "FAILED"
        Queued -> "QUEUED"
        Running -> "RUNNING"
        Succeeded -> "SUCCEEDED"

instance Hashable     QueryExecutionState
instance NFData       QueryExecutionState
instance ToByteString QueryExecutionState
instance ToQuery      QueryExecutionState
instance ToHeader     QueryExecutionState

instance FromJSON QueryExecutionState where
    parseJSON = parseJSONText "QueryExecutionState"

data StatementType
  = Ddl
  | Dml
  | Utility
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatementType where
    parser = takeLowerText >>= \case
        "ddl" -> pure Ddl
        "dml" -> pure Dml
        "utility" -> pure Utility
        e -> fromTextError $ "Failure parsing StatementType from value: '" <> e
           <> "'. Accepted values: ddl, dml, utility"

instance ToText StatementType where
    toText = \case
        Ddl -> "DDL"
        Dml -> "DML"
        Utility -> "UTILITY"

instance Hashable     StatementType
instance NFData       StatementType
instance ToByteString StatementType
instance ToQuery      StatementType
instance ToHeader     StatementType

instance FromJSON StatementType where
    parseJSON = parseJSONText "StatementType"

data WorkGroupState
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WorkGroupState where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing WorkGroupState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText WorkGroupState where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     WorkGroupState
instance NFData       WorkGroupState
instance ToByteString WorkGroupState
instance ToQuery      WorkGroupState
instance ToHeader     WorkGroupState

instance ToJSON WorkGroupState where
    toJSON = toJSONText

instance FromJSON WorkGroupState where
    parseJSON = parseJSONText "WorkGroupState"
