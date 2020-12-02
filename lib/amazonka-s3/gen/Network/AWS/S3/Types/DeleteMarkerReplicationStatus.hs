{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeleteMarkerReplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeleteMarkerReplicationStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data DeleteMarkerReplicationStatus
  = DMRSDisabled
  | DMRSEnabled
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

instance FromText DeleteMarkerReplicationStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DMRSDisabled
      "enabled" -> pure DMRSEnabled
      e ->
        fromTextError $
          "Failure parsing DeleteMarkerReplicationStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText DeleteMarkerReplicationStatus where
  toText = \case
    DMRSDisabled -> "Disabled"
    DMRSEnabled -> "Enabled"

instance Hashable DeleteMarkerReplicationStatus

instance NFData DeleteMarkerReplicationStatus

instance ToByteString DeleteMarkerReplicationStatus

instance ToQuery DeleteMarkerReplicationStatus

instance ToHeader DeleteMarkerReplicationStatus

instance FromXML DeleteMarkerReplicationStatus where
  parseXML = parseXMLText "DeleteMarkerReplicationStatus"

instance ToXML DeleteMarkerReplicationStatus where
  toXML = toXMLText
