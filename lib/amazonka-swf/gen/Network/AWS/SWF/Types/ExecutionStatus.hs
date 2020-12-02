{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExecutionStatus where

import Network.AWS.Prelude

data ExecutionStatus
  = Closed
  | Open
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

instance FromText ExecutionStatus where
  parser =
    takeLowerText >>= \case
      "closed" -> pure Closed
      "open" -> pure Open
      e ->
        fromTextError $
          "Failure parsing ExecutionStatus from value: '" <> e
            <> "'. Accepted values: closed, open"

instance ToText ExecutionStatus where
  toText = \case
    Closed -> "CLOSED"
    Open -> "OPEN"

instance Hashable ExecutionStatus

instance NFData ExecutionStatus

instance ToByteString ExecutionStatus

instance ToQuery ExecutionStatus

instance ToHeader ExecutionStatus

instance FromJSON ExecutionStatus where
  parseJSON = parseJSONText "ExecutionStatus"
