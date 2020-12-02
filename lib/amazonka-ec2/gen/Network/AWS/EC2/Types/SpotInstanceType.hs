{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotInstanceType
  = OneTime
  | Persistent
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

instance FromText SpotInstanceType where
  parser =
    takeLowerText >>= \case
      "one-time" -> pure OneTime
      "persistent" -> pure Persistent
      e ->
        fromTextError $
          "Failure parsing SpotInstanceType from value: '" <> e
            <> "'. Accepted values: one-time, persistent"

instance ToText SpotInstanceType where
  toText = \case
    OneTime -> "one-time"
    Persistent -> "persistent"

instance Hashable SpotInstanceType

instance NFData SpotInstanceType

instance ToByteString SpotInstanceType

instance ToQuery SpotInstanceType

instance ToHeader SpotInstanceType

instance FromXML SpotInstanceType where
  parseXML = parseXMLText "SpotInstanceType"
