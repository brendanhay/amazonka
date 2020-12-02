{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProactiveEngagementStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProactiveEngagementStatus where

import Network.AWS.Prelude

data ProactiveEngagementStatus
  = PESDisabled
  | PESEnabled
  | PESPending
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

instance FromText ProactiveEngagementStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure PESDisabled
      "enabled" -> pure PESEnabled
      "pending" -> pure PESPending
      e ->
        fromTextError $
          "Failure parsing ProactiveEngagementStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled, pending"

instance ToText ProactiveEngagementStatus where
  toText = \case
    PESDisabled -> "DISABLED"
    PESEnabled -> "ENABLED"
    PESPending -> "PENDING"

instance Hashable ProactiveEngagementStatus

instance NFData ProactiveEngagementStatus

instance ToByteString ProactiveEngagementStatus

instance ToQuery ProactiveEngagementStatus

instance ToHeader ProactiveEngagementStatus

instance FromJSON ProactiveEngagementStatus where
  parseJSON = parseJSONText "ProactiveEngagementStatus"
