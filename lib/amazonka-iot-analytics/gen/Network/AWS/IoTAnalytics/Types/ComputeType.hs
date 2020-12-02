{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ComputeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ComputeType where

import Network.AWS.Prelude

data ComputeType
  = Acu1
  | Acu2
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

instance FromText ComputeType where
  parser =
    takeLowerText >>= \case
      "acu_1" -> pure Acu1
      "acu_2" -> pure Acu2
      e ->
        fromTextError $
          "Failure parsing ComputeType from value: '" <> e
            <> "'. Accepted values: acu_1, acu_2"

instance ToText ComputeType where
  toText = \case
    Acu1 -> "ACU_1"
    Acu2 -> "ACU_2"

instance Hashable ComputeType

instance NFData ComputeType

instance ToByteString ComputeType

instance ToQuery ComputeType

instance ToHeader ComputeType

instance ToJSON ComputeType where
  toJSON = toJSONText

instance FromJSON ComputeType where
  parseJSON = parseJSONText "ComputeType"
