{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftStatus where

import Network.AWS.Prelude

data StackSetDriftStatus
  = Drifted
  | InSync
  | NotChecked
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

instance FromText StackSetDriftStatus where
  parser =
    takeLowerText >>= \case
      "drifted" -> pure Drifted
      "in_sync" -> pure InSync
      "not_checked" -> pure NotChecked
      e ->
        fromTextError $
          "Failure parsing StackSetDriftStatus from value: '" <> e
            <> "'. Accepted values: drifted, in_sync, not_checked"

instance ToText StackSetDriftStatus where
  toText = \case
    Drifted -> "DRIFTED"
    InSync -> "IN_SYNC"
    NotChecked -> "NOT_CHECKED"

instance Hashable StackSetDriftStatus

instance NFData StackSetDriftStatus

instance ToByteString StackSetDriftStatus

instance ToQuery StackSetDriftStatus

instance ToHeader StackSetDriftStatus

instance FromXML StackSetDriftStatus where
  parseXML = parseXMLText "StackSetDriftStatus"
