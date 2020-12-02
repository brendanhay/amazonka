{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.LookbackPeriodInDays
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.LookbackPeriodInDays where

import Network.AWS.Prelude

data LookbackPeriodInDays
  = SevenDays
  | SixtyDays
  | ThirtyDays
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

instance FromText LookbackPeriodInDays where
  parser =
    takeLowerText >>= \case
      "seven_days" -> pure SevenDays
      "sixty_days" -> pure SixtyDays
      "thirty_days" -> pure ThirtyDays
      e ->
        fromTextError $
          "Failure parsing LookbackPeriodInDays from value: '" <> e
            <> "'. Accepted values: seven_days, sixty_days, thirty_days"

instance ToText LookbackPeriodInDays where
  toText = \case
    SevenDays -> "SEVEN_DAYS"
    SixtyDays -> "SIXTY_DAYS"
    ThirtyDays -> "THIRTY_DAYS"

instance Hashable LookbackPeriodInDays

instance NFData LookbackPeriodInDays

instance ToByteString LookbackPeriodInDays

instance ToQuery LookbackPeriodInDays

instance ToHeader LookbackPeriodInDays

instance ToJSON LookbackPeriodInDays where
  toJSON = toJSONText

instance FromJSON LookbackPeriodInDays where
  parseJSON = parseJSONText "LookbackPeriodInDays"
