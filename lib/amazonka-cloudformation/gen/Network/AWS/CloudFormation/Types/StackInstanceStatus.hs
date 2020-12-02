{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceStatus where

import Network.AWS.Prelude

data StackInstanceStatus
  = Current
  | Inoperable
  | Outdated
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

instance FromText StackInstanceStatus where
  parser =
    takeLowerText >>= \case
      "current" -> pure Current
      "inoperable" -> pure Inoperable
      "outdated" -> pure Outdated
      e ->
        fromTextError $
          "Failure parsing StackInstanceStatus from value: '" <> e
            <> "'. Accepted values: current, inoperable, outdated"

instance ToText StackInstanceStatus where
  toText = \case
    Current -> "CURRENT"
    Inoperable -> "INOPERABLE"
    Outdated -> "OUTDATED"

instance Hashable StackInstanceStatus

instance NFData StackInstanceStatus

instance ToByteString StackInstanceStatus

instance ToQuery StackInstanceStatus

instance ToHeader StackInstanceStatus

instance FromXML StackInstanceStatus where
  parseXML = parseXMLText "StackInstanceStatus"
