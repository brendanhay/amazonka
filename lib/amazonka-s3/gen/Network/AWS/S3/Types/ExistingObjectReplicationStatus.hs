{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ExistingObjectReplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ExistingObjectReplicationStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ExistingObjectReplicationStatus
  = EORSDisabled
  | EORSEnabled
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

instance FromText ExistingObjectReplicationStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure EORSDisabled
      "enabled" -> pure EORSEnabled
      e ->
        fromTextError $
          "Failure parsing ExistingObjectReplicationStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ExistingObjectReplicationStatus where
  toText = \case
    EORSDisabled -> "Disabled"
    EORSEnabled -> "Enabled"

instance Hashable ExistingObjectReplicationStatus

instance NFData ExistingObjectReplicationStatus

instance ToByteString ExistingObjectReplicationStatus

instance ToQuery ExistingObjectReplicationStatus

instance ToHeader ExistingObjectReplicationStatus

instance FromXML ExistingObjectReplicationStatus where
  parseXML = parseXMLText "ExistingObjectReplicationStatus"

instance ToXML ExistingObjectReplicationStatus where
  toXML = toXMLText
