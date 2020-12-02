{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6SupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6SupportValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data IPv6SupportValue
  = ISVDisable
  | ISVEnable
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

instance FromText IPv6SupportValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure ISVDisable
      "enable" -> pure ISVEnable
      e ->
        fromTextError $
          "Failure parsing IPv6SupportValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText IPv6SupportValue where
  toText = \case
    ISVDisable -> "disable"
    ISVEnable -> "enable"

instance Hashable IPv6SupportValue

instance NFData IPv6SupportValue

instance ToByteString IPv6SupportValue

instance ToQuery IPv6SupportValue

instance ToHeader IPv6SupportValue

instance FromXML IPv6SupportValue where
  parseXML = parseXMLText "IPv6SupportValue"
