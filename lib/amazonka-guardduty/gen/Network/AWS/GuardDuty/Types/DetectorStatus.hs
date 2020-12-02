{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DetectorStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DetectorStatus where

import Network.AWS.Prelude

data DetectorStatus
  = DSDisabled
  | DSEnabled
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

instance FromText DetectorStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DSDisabled
      "enabled" -> pure DSEnabled
      e ->
        fromTextError $
          "Failure parsing DetectorStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText DetectorStatus where
  toText = \case
    DSDisabled -> "DISABLED"
    DSEnabled -> "ENABLED"

instance Hashable DetectorStatus

instance NFData DetectorStatus

instance ToByteString DetectorStatus

instance ToQuery DetectorStatus

instance ToHeader DetectorStatus

instance FromJSON DetectorStatus where
  parseJSON = parseJSONText "DetectorStatus"
