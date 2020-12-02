{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FastSnapshotRestoreStateCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FastSnapshotRestoreStateCode
  = FSRSCDisabled
  | FSRSCDisabling
  | FSRSCEnabled
  | FSRSCEnabling
  | FSRSCOptimizing
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

instance FromText FastSnapshotRestoreStateCode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure FSRSCDisabled
      "disabling" -> pure FSRSCDisabling
      "enabled" -> pure FSRSCEnabled
      "enabling" -> pure FSRSCEnabling
      "optimizing" -> pure FSRSCOptimizing
      e ->
        fromTextError $
          "Failure parsing FastSnapshotRestoreStateCode from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling, optimizing"

instance ToText FastSnapshotRestoreStateCode where
  toText = \case
    FSRSCDisabled -> "disabled"
    FSRSCDisabling -> "disabling"
    FSRSCEnabled -> "enabled"
    FSRSCEnabling -> "enabling"
    FSRSCOptimizing -> "optimizing"

instance Hashable FastSnapshotRestoreStateCode

instance NFData FastSnapshotRestoreStateCode

instance ToByteString FastSnapshotRestoreStateCode

instance ToQuery FastSnapshotRestoreStateCode

instance ToHeader FastSnapshotRestoreStateCode

instance FromXML FastSnapshotRestoreStateCode where
  parseXML = parseXMLText "FastSnapshotRestoreStateCode"
