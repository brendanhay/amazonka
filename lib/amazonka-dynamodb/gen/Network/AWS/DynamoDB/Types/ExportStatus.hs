{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportStatus where

import Network.AWS.Prelude

data ExportStatus
  = Completed
  | Failed
  | InProgress
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

instance FromText ExportStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      e ->
        fromTextError $
          "Failure parsing ExportStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, in_progress"

instance ToText ExportStatus where
  toText = \case
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"

instance Hashable ExportStatus

instance NFData ExportStatus

instance ToByteString ExportStatus

instance ToQuery ExportStatus

instance ToHeader ExportStatus

instance FromJSON ExportStatus where
  parseJSON = parseJSONText "ExportStatus"
