{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionStatus where

import Network.AWS.Prelude

data TestGridSessionStatus
  = Active
  | Closed
  | Errored
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

instance FromText TestGridSessionStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "closed" -> pure Closed
      "errored" -> pure Errored
      e ->
        fromTextError $
          "Failure parsing TestGridSessionStatus from value: '" <> e
            <> "'. Accepted values: active, closed, errored"

instance ToText TestGridSessionStatus where
  toText = \case
    Active -> "ACTIVE"
    Closed -> "CLOSED"
    Errored -> "ERRORED"

instance Hashable TestGridSessionStatus

instance NFData TestGridSessionStatus

instance ToByteString TestGridSessionStatus

instance ToQuery TestGridSessionStatus

instance ToHeader TestGridSessionStatus

instance ToJSON TestGridSessionStatus where
  toJSON = toJSONText

instance FromJSON TestGridSessionStatus where
  parseJSON = parseJSONText "TestGridSessionStatus"
