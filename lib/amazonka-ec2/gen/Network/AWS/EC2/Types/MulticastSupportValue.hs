{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MulticastSupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MulticastSupportValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MulticastSupportValue
  = MSVDisable
  | MSVEnable
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

instance FromText MulticastSupportValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure MSVDisable
      "enable" -> pure MSVEnable
      e ->
        fromTextError $
          "Failure parsing MulticastSupportValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText MulticastSupportValue where
  toText = \case
    MSVDisable -> "disable"
    MSVEnable -> "enable"

instance Hashable MulticastSupportValue

instance NFData MulticastSupportValue

instance ToByteString MulticastSupportValue

instance ToQuery MulticastSupportValue

instance ToHeader MulticastSupportValue

instance FromXML MulticastSupportValue where
  parseXML = parseXMLText "MulticastSupportValue"
