{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Mode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Mode where

import Network.AWS.Prelude

data Mode
  = Delivery
  | Filter
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

instance FromText Mode where
  parser =
    takeLowerText >>= \case
      "delivery" -> pure Delivery
      "filter" -> pure Filter
      e ->
        fromTextError $
          "Failure parsing Mode from value: '" <> e
            <> "'. Accepted values: delivery, filter"

instance ToText Mode where
  toText = \case
    Delivery -> "DELIVERY"
    Filter -> "FILTER"

instance Hashable Mode

instance NFData Mode

instance ToByteString Mode

instance ToQuery Mode

instance ToHeader Mode

instance ToJSON Mode where
  toJSON = toJSONText

instance FromJSON Mode where
  parseJSON = parseJSONText "Mode"
