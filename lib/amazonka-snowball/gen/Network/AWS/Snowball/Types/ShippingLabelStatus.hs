{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShippingLabelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShippingLabelStatus where

import Network.AWS.Prelude

data ShippingLabelStatus
  = Failed
  | InProgress
  | Succeeded
  | TimedOut
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

instance FromText ShippingLabelStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "inprogress" -> pure InProgress
      "succeeded" -> pure Succeeded
      "timedout" -> pure TimedOut
      e ->
        fromTextError $
          "Failure parsing ShippingLabelStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, succeeded, timedout"

instance ToText ShippingLabelStatus where
  toText = \case
    Failed -> "Failed"
    InProgress -> "InProgress"
    Succeeded -> "Succeeded"
    TimedOut -> "TimedOut"

instance Hashable ShippingLabelStatus

instance NFData ShippingLabelStatus

instance ToByteString ShippingLabelStatus

instance ToQuery ShippingLabelStatus

instance ToHeader ShippingLabelStatus

instance FromJSON ShippingLabelStatus where
  parseJSON = parseJSONText "ShippingLabelStatus"
