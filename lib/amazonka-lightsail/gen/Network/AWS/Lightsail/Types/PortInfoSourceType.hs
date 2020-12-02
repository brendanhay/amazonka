{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortInfoSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortInfoSourceType where

import Network.AWS.Prelude

data PortInfoSourceType
  = PISTClosed
  | PISTDefault
  | PISTInstance
  | PISTNone
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

instance FromText PortInfoSourceType where
  parser =
    takeLowerText >>= \case
      "closed" -> pure PISTClosed
      "default" -> pure PISTDefault
      "instance" -> pure PISTInstance
      "none" -> pure PISTNone
      e ->
        fromTextError $
          "Failure parsing PortInfoSourceType from value: '" <> e
            <> "'. Accepted values: closed, default, instance, none"

instance ToText PortInfoSourceType where
  toText = \case
    PISTClosed -> "CLOSED"
    PISTDefault -> "DEFAULT"
    PISTInstance -> "INSTANCE"
    PISTNone -> "NONE"

instance Hashable PortInfoSourceType

instance NFData PortInfoSourceType

instance ToByteString PortInfoSourceType

instance ToQuery PortInfoSourceType

instance ToHeader PortInfoSourceType

instance ToJSON PortInfoSourceType where
  toJSON = toJSONText
