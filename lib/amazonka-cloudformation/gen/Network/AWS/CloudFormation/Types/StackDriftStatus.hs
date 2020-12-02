{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftStatus where

import Network.AWS.Prelude

data StackDriftStatus
  = SDSDrifted
  | SDSInSync
  | SDSNotChecked
  | SDSUnknown
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

instance FromText StackDriftStatus where
  parser =
    takeLowerText >>= \case
      "drifted" -> pure SDSDrifted
      "in_sync" -> pure SDSInSync
      "not_checked" -> pure SDSNotChecked
      "unknown" -> pure SDSUnknown
      e ->
        fromTextError $
          "Failure parsing StackDriftStatus from value: '" <> e
            <> "'. Accepted values: drifted, in_sync, not_checked, unknown"

instance ToText StackDriftStatus where
  toText = \case
    SDSDrifted -> "DRIFTED"
    SDSInSync -> "IN_SYNC"
    SDSNotChecked -> "NOT_CHECKED"
    SDSUnknown -> "UNKNOWN"

instance Hashable StackDriftStatus

instance NFData StackDriftStatus

instance ToByteString StackDriftStatus

instance ToQuery StackDriftStatus

instance ToHeader StackDriftStatus

instance FromXML StackDriftStatus where
  parseXML = parseXMLText "StackDriftStatus"
