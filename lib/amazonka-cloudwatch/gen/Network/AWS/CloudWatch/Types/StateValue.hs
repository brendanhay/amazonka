{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StateValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StateValue where

import Network.AWS.Prelude

data StateValue
  = Alarm
  | InsufficientData
  | OK
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

instance FromText StateValue where
  parser =
    takeLowerText >>= \case
      "alarm" -> pure Alarm
      "insufficient_data" -> pure InsufficientData
      "ok" -> pure OK
      e ->
        fromTextError $
          "Failure parsing StateValue from value: '" <> e
            <> "'. Accepted values: alarm, insufficient_data, ok"

instance ToText StateValue where
  toText = \case
    Alarm -> "ALARM"
    InsufficientData -> "INSUFFICIENT_DATA"
    OK -> "OK"

instance Hashable StateValue

instance NFData StateValue

instance ToByteString StateValue

instance ToQuery StateValue

instance ToHeader StateValue

instance FromXML StateValue where
  parseXML = parseXMLText "StateValue"
