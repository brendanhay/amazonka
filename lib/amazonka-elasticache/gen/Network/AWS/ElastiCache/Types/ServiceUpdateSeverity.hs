{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdateSeverity where

import Network.AWS.Prelude

data ServiceUpdateSeverity
  = Critical
  | Important
  | Low
  | Medium
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

instance FromText ServiceUpdateSeverity where
  parser =
    takeLowerText >>= \case
      "critical" -> pure Critical
      "important" -> pure Important
      "low" -> pure Low
      "medium" -> pure Medium
      e ->
        fromTextError $
          "Failure parsing ServiceUpdateSeverity from value: '" <> e
            <> "'. Accepted values: critical, important, low, medium"

instance ToText ServiceUpdateSeverity where
  toText = \case
    Critical -> "critical"
    Important -> "important"
    Low -> "low"
    Medium -> "medium"

instance Hashable ServiceUpdateSeverity

instance NFData ServiceUpdateSeverity

instance ToByteString ServiceUpdateSeverity

instance ToQuery ServiceUpdateSeverity

instance ToHeader ServiceUpdateSeverity

instance FromXML ServiceUpdateSeverity where
  parseXML = parseXMLText "ServiceUpdateSeverity"
