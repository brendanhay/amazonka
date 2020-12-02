{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DefaultRouteTableAssociationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DefaultRouteTableAssociationValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DefaultRouteTableAssociationValue
  = DRTAVDisable
  | DRTAVEnable
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

instance FromText DefaultRouteTableAssociationValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure DRTAVDisable
      "enable" -> pure DRTAVEnable
      e ->
        fromTextError $
          "Failure parsing DefaultRouteTableAssociationValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText DefaultRouteTableAssociationValue where
  toText = \case
    DRTAVDisable -> "disable"
    DRTAVEnable -> "enable"

instance Hashable DefaultRouteTableAssociationValue

instance NFData DefaultRouteTableAssociationValue

instance ToByteString DefaultRouteTableAssociationValue

instance ToQuery DefaultRouteTableAssociationValue

instance ToHeader DefaultRouteTableAssociationValue

instance FromXML DefaultRouteTableAssociationValue where
  parseXML = parseXMLText "DefaultRouteTableAssociationValue"
