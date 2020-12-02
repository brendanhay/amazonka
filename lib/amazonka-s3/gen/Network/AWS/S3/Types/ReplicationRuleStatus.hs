{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRuleStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ReplicationRuleStatus
  = Disabled
  | Enabled
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

instance FromText ReplicationRuleStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing ReplicationRuleStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ReplicationRuleStatus where
  toText = \case
    Disabled -> "Disabled"
    Enabled -> "Enabled"

instance Hashable ReplicationRuleStatus

instance NFData ReplicationRuleStatus

instance ToByteString ReplicationRuleStatus

instance ToQuery ReplicationRuleStatus

instance ToHeader ReplicationRuleStatus

instance FromXML ReplicationRuleStatus where
  parseXML = parseXMLText "ReplicationRuleStatus"

instance ToXML ReplicationRuleStatus where
  toXML = toXMLText
