{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimitBreachAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimitBreachAction where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data UsageLimitBreachAction
  = Disable
  | EmitMetric
  | Log
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

instance FromText UsageLimitBreachAction where
  parser =
    takeLowerText >>= \case
      "disable" -> pure Disable
      "emit-metric" -> pure EmitMetric
      "log" -> pure Log
      e ->
        fromTextError $
          "Failure parsing UsageLimitBreachAction from value: '" <> e
            <> "'. Accepted values: disable, emit-metric, log"

instance ToText UsageLimitBreachAction where
  toText = \case
    Disable -> "disable"
    EmitMetric -> "emit-metric"
    Log -> "log"

instance Hashable UsageLimitBreachAction

instance NFData UsageLimitBreachAction

instance ToByteString UsageLimitBreachAction

instance ToQuery UsageLimitBreachAction

instance ToHeader UsageLimitBreachAction

instance FromXML UsageLimitBreachAction where
  parseXML = parseXMLText "UsageLimitBreachAction"
