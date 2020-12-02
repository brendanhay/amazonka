{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType where

import Network.AWS.Prelude

data TimeUnitsType
  = Days
  | Hours
  | Minutes
  | Seconds
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

instance FromText TimeUnitsType where
  parser =
    takeLowerText >>= \case
      "days" -> pure Days
      "hours" -> pure Hours
      "minutes" -> pure Minutes
      "seconds" -> pure Seconds
      e ->
        fromTextError $
          "Failure parsing TimeUnitsType from value: '" <> e
            <> "'. Accepted values: days, hours, minutes, seconds"

instance ToText TimeUnitsType where
  toText = \case
    Days -> "days"
    Hours -> "hours"
    Minutes -> "minutes"
    Seconds -> "seconds"

instance Hashable TimeUnitsType

instance NFData TimeUnitsType

instance ToByteString TimeUnitsType

instance ToQuery TimeUnitsType

instance ToHeader TimeUnitsType

instance ToJSON TimeUnitsType where
  toJSON = toJSONText

instance FromJSON TimeUnitsType where
  parseJSON = parseJSONText "TimeUnitsType"
