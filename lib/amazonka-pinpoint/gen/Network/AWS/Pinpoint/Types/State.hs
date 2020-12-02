{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.State
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.State where

import Network.AWS.Prelude

data State
  = SActive
  | SCancelled
  | SClosed
  | SCompleted
  | SDraft
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

instance FromText State where
  parser =
    takeLowerText >>= \case
      "active" -> pure SActive
      "cancelled" -> pure SCancelled
      "closed" -> pure SClosed
      "completed" -> pure SCompleted
      "draft" -> pure SDraft
      e ->
        fromTextError $
          "Failure parsing State from value: '" <> e
            <> "'. Accepted values: active, cancelled, closed, completed, draft"

instance ToText State where
  toText = \case
    SActive -> "ACTIVE"
    SCancelled -> "CANCELLED"
    SClosed -> "CLOSED"
    SCompleted -> "COMPLETED"
    SDraft -> "DRAFT"

instance Hashable State

instance NFData State

instance ToByteString State

instance ToQuery State

instance ToHeader State

instance ToJSON State where
  toJSON = toJSONText

instance FromJSON State where
  parseJSON = parseJSONText "State"
