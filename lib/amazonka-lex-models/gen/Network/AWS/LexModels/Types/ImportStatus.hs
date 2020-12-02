{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ImportStatus where

import Network.AWS.Prelude

data ImportStatus
  = ISComplete
  | ISFailed
  | ISInProgress
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

instance FromText ImportStatus where
  parser =
    takeLowerText >>= \case
      "complete" -> pure ISComplete
      "failed" -> pure ISFailed
      "in_progress" -> pure ISInProgress
      e ->
        fromTextError $
          "Failure parsing ImportStatus from value: '" <> e
            <> "'. Accepted values: complete, failed, in_progress"

instance ToText ImportStatus where
  toText = \case
    ISComplete -> "COMPLETE"
    ISFailed -> "FAILED"
    ISInProgress -> "IN_PROGRESS"

instance Hashable ImportStatus

instance NFData ImportStatus

instance ToByteString ImportStatus

instance ToQuery ImportStatus

instance ToHeader ImportStatus

instance FromJSON ImportStatus where
  parseJSON = parseJSONText "ImportStatus"
