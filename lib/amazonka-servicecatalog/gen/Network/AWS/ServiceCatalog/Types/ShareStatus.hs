{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareStatus where

import Network.AWS.Prelude

data ShareStatus
  = Completed
  | CompletedWithErrors
  | Error'
  | InProgress
  | NotStarted
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

instance FromText ShareStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "completed_with_errors" -> pure CompletedWithErrors
      "error" -> pure Error'
      "in_progress" -> pure InProgress
      "not_started" -> pure NotStarted
      e ->
        fromTextError $
          "Failure parsing ShareStatus from value: '" <> e
            <> "'. Accepted values: completed, completed_with_errors, error, in_progress, not_started"

instance ToText ShareStatus where
  toText = \case
    Completed -> "COMPLETED"
    CompletedWithErrors -> "COMPLETED_WITH_ERRORS"
    Error' -> "ERROR"
    InProgress -> "IN_PROGRESS"
    NotStarted -> "NOT_STARTED"

instance Hashable ShareStatus

instance NFData ShareStatus

instance ToByteString ShareStatus

instance ToQuery ShareStatus

instance ToHeader ShareStatus

instance FromJSON ShareStatus where
  parseJSON = parseJSONText "ShareStatus"
