{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MonitorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MonitorType where

import Network.AWS.Prelude

data MonitorType
  = Custom
  | Dimensional
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

instance FromText MonitorType where
  parser =
    takeLowerText >>= \case
      "custom" -> pure Custom
      "dimensional" -> pure Dimensional
      e ->
        fromTextError $
          "Failure parsing MonitorType from value: '" <> e
            <> "'. Accepted values: custom, dimensional"

instance ToText MonitorType where
  toText = \case
    Custom -> "CUSTOM"
    Dimensional -> "DIMENSIONAL"

instance Hashable MonitorType

instance NFData MonitorType

instance ToByteString MonitorType

instance ToQuery MonitorType

instance ToHeader MonitorType

instance ToJSON MonitorType where
  toJSON = toJSONText

instance FromJSON MonitorType where
  parseJSON = parseJSONText "MonitorType"
