{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.ThroughputMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.ThroughputMode where

import Network.AWS.Prelude

data ThroughputMode
  = Bursting
  | Provisioned
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

instance FromText ThroughputMode where
  parser =
    takeLowerText >>= \case
      "bursting" -> pure Bursting
      "provisioned" -> pure Provisioned
      e ->
        fromTextError $
          "Failure parsing ThroughputMode from value: '" <> e
            <> "'. Accepted values: bursting, provisioned"

instance ToText ThroughputMode where
  toText = \case
    Bursting -> "bursting"
    Provisioned -> "provisioned"

instance Hashable ThroughputMode

instance NFData ThroughputMode

instance ToByteString ThroughputMode

instance ToQuery ThroughputMode

instance ToHeader ThroughputMode

instance ToJSON ThroughputMode where
  toJSON = toJSONText

instance FromJSON ThroughputMode where
  parseJSON = parseJSONText "ThroughputMode"
