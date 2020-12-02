{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociationStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociationStateCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RouteTableAssociationStateCode
  = RTASCAssociated
  | RTASCAssociating
  | RTASCDisassociated
  | RTASCDisassociating
  | RTASCFailed
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

instance FromText RouteTableAssociationStateCode where
  parser =
    takeLowerText >>= \case
      "associated" -> pure RTASCAssociated
      "associating" -> pure RTASCAssociating
      "disassociated" -> pure RTASCDisassociated
      "disassociating" -> pure RTASCDisassociating
      "failed" -> pure RTASCFailed
      e ->
        fromTextError $
          "Failure parsing RouteTableAssociationStateCode from value: '" <> e
            <> "'. Accepted values: associated, associating, disassociated, disassociating, failed"

instance ToText RouteTableAssociationStateCode where
  toText = \case
    RTASCAssociated -> "associated"
    RTASCAssociating -> "associating"
    RTASCDisassociated -> "disassociated"
    RTASCDisassociating -> "disassociating"
    RTASCFailed -> "failed"

instance Hashable RouteTableAssociationStateCode

instance NFData RouteTableAssociationStateCode

instance ToByteString RouteTableAssociationStateCode

instance ToQuery RouteTableAssociationStateCode

instance ToHeader RouteTableAssociationStateCode

instance FromXML RouteTableAssociationStateCode where
  parseXML = parseXMLText "RouteTableAssociationStateCode"
