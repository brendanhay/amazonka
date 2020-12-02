{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.SchemaStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.SchemaStatus where

import Network.AWS.Prelude

data SchemaStatus
  = SSActive
  | SSDeleting
  | SSFailed
  | SSNotApplicable
  | SSProcessing
  | SSSuccess
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

instance FromText SchemaStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure SSActive
      "deleting" -> pure SSDeleting
      "failed" -> pure SSFailed
      "not_applicable" -> pure SSNotApplicable
      "processing" -> pure SSProcessing
      "success" -> pure SSSuccess
      e ->
        fromTextError $
          "Failure parsing SchemaStatus from value: '" <> e
            <> "'. Accepted values: active, deleting, failed, not_applicable, processing, success"

instance ToText SchemaStatus where
  toText = \case
    SSActive -> "ACTIVE"
    SSDeleting -> "DELETING"
    SSFailed -> "FAILED"
    SSNotApplicable -> "NOT_APPLICABLE"
    SSProcessing -> "PROCESSING"
    SSSuccess -> "SUCCESS"

instance Hashable SchemaStatus

instance NFData SchemaStatus

instance ToByteString SchemaStatus

instance ToQuery SchemaStatus

instance ToHeader SchemaStatus

instance FromJSON SchemaStatus where
  parseJSON = parseJSONText "SchemaStatus"
