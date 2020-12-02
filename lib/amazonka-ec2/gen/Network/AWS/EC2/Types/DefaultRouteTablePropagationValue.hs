{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DefaultRouteTablePropagationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DefaultRouteTablePropagationValue where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DefaultRouteTablePropagationValue
  = DRTPVDisable
  | DRTPVEnable
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

instance FromText DefaultRouteTablePropagationValue where
  parser =
    takeLowerText >>= \case
      "disable" -> pure DRTPVDisable
      "enable" -> pure DRTPVEnable
      e ->
        fromTextError $
          "Failure parsing DefaultRouteTablePropagationValue from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText DefaultRouteTablePropagationValue where
  toText = \case
    DRTPVDisable -> "disable"
    DRTPVEnable -> "enable"

instance Hashable DefaultRouteTablePropagationValue

instance NFData DefaultRouteTablePropagationValue

instance ToByteString DefaultRouteTablePropagationValue

instance ToQuery DefaultRouteTablePropagationValue

instance ToHeader DefaultRouteTablePropagationValue

instance FromXML DefaultRouteTablePropagationValue where
  parseXML = parseXMLText "DefaultRouteTablePropagationValue"
