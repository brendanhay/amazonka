{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.WriteForwardingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.WriteForwardingStatus where

import Network.AWS.Prelude

data WriteForwardingStatus
  = Disabled
  | Disabling
  | Enabled
  | Enabling
  | Unknown
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

instance FromText WriteForwardingStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "disabling" -> pure Disabling
      "enabled" -> pure Enabled
      "enabling" -> pure Enabling
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing WriteForwardingStatus from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling, unknown"

instance ToText WriteForwardingStatus where
  toText = \case
    Disabled -> "disabled"
    Disabling -> "disabling"
    Enabled -> "enabled"
    Enabling -> "enabling"
    Unknown -> "unknown"

instance Hashable WriteForwardingStatus

instance NFData WriteForwardingStatus

instance ToByteString WriteForwardingStatus

instance ToQuery WriteForwardingStatus

instance ToHeader WriteForwardingStatus

instance FromXML WriteForwardingStatus where
  parseXML = parseXMLText "WriteForwardingStatus"
