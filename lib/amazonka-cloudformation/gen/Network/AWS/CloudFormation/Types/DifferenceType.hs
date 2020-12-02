{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.DifferenceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.DifferenceType where

import Network.AWS.Prelude

data DifferenceType
  = DTAdd
  | DTNotEqual
  | DTRemove
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

instance FromText DifferenceType where
  parser =
    takeLowerText >>= \case
      "add" -> pure DTAdd
      "not_equal" -> pure DTNotEqual
      "remove" -> pure DTRemove
      e ->
        fromTextError $
          "Failure parsing DifferenceType from value: '" <> e
            <> "'. Accepted values: add, not_equal, remove"

instance ToText DifferenceType where
  toText = \case
    DTAdd -> "ADD"
    DTNotEqual -> "NOT_EQUAL"
    DTRemove -> "REMOVE"

instance Hashable DifferenceType

instance NFData DifferenceType

instance ToByteString DifferenceType

instance ToQuery DifferenceType

instance ToHeader DifferenceType

instance FromXML DifferenceType where
  parseXML = parseXMLText "DifferenceType"
