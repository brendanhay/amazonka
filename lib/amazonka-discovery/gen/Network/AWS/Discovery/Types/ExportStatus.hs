{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportStatus where

import Network.AWS.Prelude

data ExportStatus
  = Failed
  | InProgress
  | Succeeded
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
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing ExportStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText ExportStatus where
  toText = \case
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"
    Succeeded -> "SUCCEEDED"

instance Hashable ExportStatus

instance NFData ExportStatus

instance ToByteString ExportStatus

instance ToQuery ExportStatus

instance ToHeader ExportStatus

instance FromJSON ExportStatus where
  parseJSON = parseJSONText "ExportStatus"
