{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MonitorDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MonitorDimension where

import Network.AWS.Prelude

data MonitorDimension = Service
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

instance FromText MonitorDimension where
  parser =
    takeLowerText >>= \case
      "service" -> pure Service
      e ->
        fromTextError $
          "Failure parsing MonitorDimension from value: '" <> e
            <> "'. Accepted values: service"

instance ToText MonitorDimension where
  toText = \case
    Service -> "SERVICE"

instance Hashable MonitorDimension

instance NFData MonitorDimension

instance ToByteString MonitorDimension

instance ToQuery MonitorDimension

instance ToHeader MonitorDimension

instance ToJSON MonitorDimension where
  toJSON = toJSONText

instance FromJSON MonitorDimension where
  parseJSON = parseJSONText "MonitorDimension"
