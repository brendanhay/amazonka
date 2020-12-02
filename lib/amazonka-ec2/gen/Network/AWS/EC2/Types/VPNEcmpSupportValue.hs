{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNEcmpSupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNEcmpSupportValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPNEcmpSupportValue
  = VESVDisable
  | VESVEnable
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

instance FromText VPNEcmpSupportValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure VESVDisable
      "enable" -> pure VESVEnable
      e ->
        fromTextError $
          "Failure parsing VPNEcmpSupportValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText VPNEcmpSupportValue where
  toText = \case
    VESVDisable -> "disable"
    VESVEnable -> "enable"

instance Hashable VPNEcmpSupportValue

instance NFData VPNEcmpSupportValue

instance ToByteString VPNEcmpSupportValue

instance ToQuery VPNEcmpSupportValue

instance ToHeader VPNEcmpSupportValue

instance FromXML VPNEcmpSupportValue where
  parseXML = parseXMLText "VPNEcmpSupportValue"
