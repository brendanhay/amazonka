{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.NotificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.NotificationType where

import Network.AWS.Prelude

-- | The type of a notification. It must be ACTUAL or FORECASTED.
data NotificationType
  = Actual
  | Forecasted
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

instance FromText NotificationType where
  parser =
    takeLowerText >>= \case
      "actual" -> pure Actual
      "forecasted" -> pure Forecasted
      e ->
        fromTextError $
          "Failure parsing NotificationType from value: '" <> e
            <> "'. Accepted values: actual, forecasted"

instance ToText NotificationType where
  toText = \case
    Actual -> "ACTUAL"
    Forecasted -> "FORECASTED"

instance Hashable NotificationType

instance NFData NotificationType

instance ToByteString NotificationType

instance ToQuery NotificationType

instance ToHeader NotificationType

instance ToJSON NotificationType where
  toJSON = toJSONText

instance FromJSON NotificationType where
  parseJSON = parseJSONText "NotificationType"
