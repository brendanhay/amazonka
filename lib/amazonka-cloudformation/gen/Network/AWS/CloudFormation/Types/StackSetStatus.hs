{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetStatus where

import Network.AWS.Prelude

data StackSetStatus
  = Active
  | Deleted
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

instance FromText StackSetStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "deleted" -> pure Deleted
      e ->
        fromTextError $
          "Failure parsing StackSetStatus from value: '" <> e
            <> "'. Accepted values: active, deleted"

instance ToText StackSetStatus where
  toText = \case
    Active -> "ACTIVE"
    Deleted -> "DELETED"

instance Hashable StackSetStatus

instance NFData StackSetStatus

instance ToByteString StackSetStatus

instance ToQuery StackSetStatus

instance ToHeader StackSetStatus

instance FromXML StackSetStatus where
  parseXML = parseXMLText "StackSetStatus"
