{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MonitoringState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MonitoringState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MonitoringState
  = MSDisabled
  | MSDisabling
  | MSEnabled
  | MSPending
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

instance FromText MonitoringState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MSDisabled
      "disabling" -> pure MSDisabling
      "enabled" -> pure MSEnabled
      "pending" -> pure MSPending
      e ->
        fromTextError $
          "Failure parsing MonitoringState from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, pending"

instance ToText MonitoringState where
  toText = \case
    MSDisabled -> "disabled"
    MSDisabling -> "disabling"
    MSEnabled -> "enabled"
    MSPending -> "pending"

instance Hashable MonitoringState

instance NFData MonitoringState

instance ToByteString MonitoringState

instance ToQuery MonitoringState

instance ToHeader MonitoringState

instance FromXML MonitoringState where
  parseXML = parseXMLText "MonitoringState"
