{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationTimeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationTimeStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ReplicationTimeStatus
  = RTSDisabled
  | RTSEnabled
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

instance FromText ReplicationTimeStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure RTSDisabled
      "enabled" -> pure RTSEnabled
      e ->
        fromTextError $
          "Failure parsing ReplicationTimeStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ReplicationTimeStatus where
  toText = \case
    RTSDisabled -> "Disabled"
    RTSEnabled -> "Enabled"

instance Hashable ReplicationTimeStatus

instance NFData ReplicationTimeStatus

instance ToByteString ReplicationTimeStatus

instance ToQuery ReplicationTimeStatus

instance ToHeader ReplicationTimeStatus

instance FromXML ReplicationTimeStatus where
  parseXML = parseXMLText "ReplicationTimeStatus"

instance ToXML ReplicationTimeStatus where
  toXML = toXMLText
