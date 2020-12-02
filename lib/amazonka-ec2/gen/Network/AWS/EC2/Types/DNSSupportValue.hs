{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DNSSupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DNSSupportValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DNSSupportValue
  = DSVDisable
  | DSVEnable
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

instance FromText DNSSupportValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure DSVDisable
      "enable" -> pure DSVEnable
      e ->
        fromTextError $
          "Failure parsing DNSSupportValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText DNSSupportValue where
  toText = \case
    DSVDisable -> "disable"
    DSVEnable -> "enable"

instance Hashable DNSSupportValue

instance NFData DNSSupportValue

instance ToByteString DNSSupportValue

instance ToQuery DNSSupportValue

instance ToHeader DNSSupportValue

instance FromXML DNSSupportValue where
  parseXML = parseXMLText "DNSSupportValue"
