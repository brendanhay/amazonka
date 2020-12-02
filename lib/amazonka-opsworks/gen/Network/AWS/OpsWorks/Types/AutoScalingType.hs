{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AutoScalingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AutoScalingType where

import Network.AWS.Prelude

data AutoScalingType
  = Load
  | Timer
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

instance FromText AutoScalingType where
  parser =
    takeLowerText >>= \case
      "load" -> pure Load
      "timer" -> pure Timer
      e ->
        fromTextError $
          "Failure parsing AutoScalingType from value: '" <> e
            <> "'. Accepted values: load, timer"

instance ToText AutoScalingType where
  toText = \case
    Load -> "load"
    Timer -> "timer"

instance Hashable AutoScalingType

instance NFData AutoScalingType

instance ToByteString AutoScalingType

instance ToQuery AutoScalingType

instance ToHeader AutoScalingType

instance ToJSON AutoScalingType where
  toJSON = toJSONText

instance FromJSON AutoScalingType where
  parseJSON = parseJSONText "AutoScalingType"
