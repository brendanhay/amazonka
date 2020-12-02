{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewActionStatus where

import Network.AWS.Prelude

data ReviewActionStatus
  = Cancelled
  | Failed
  | Intended
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

instance FromText ReviewActionStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure Cancelled
      "failed" -> pure Failed
      "intended" -> pure Intended
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing ReviewActionStatus from value: '" <> e
            <> "'. Accepted values: cancelled, failed, intended, succeeded"

instance ToText ReviewActionStatus where
  toText = \case
    Cancelled -> "Cancelled"
    Failed -> "Failed"
    Intended -> "Intended"
    Succeeded -> "Succeeded"

instance Hashable ReviewActionStatus

instance NFData ReviewActionStatus

instance ToByteString ReviewActionStatus

instance ToQuery ReviewActionStatus

instance ToHeader ReviewActionStatus

instance FromJSON ReviewActionStatus where
  parseJSON = parseJSONText "ReviewActionStatus"
