{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ReservedNodeOfferingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ReservedNodeOfferingType where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ReservedNodeOfferingType
  = Regular
  | Upgradable
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

instance FromText ReservedNodeOfferingType where
  parser =
    takeLowerText >>= \case
      "regular" -> pure Regular
      "upgradable" -> pure Upgradable
      e ->
        fromTextError $
          "Failure parsing ReservedNodeOfferingType from value: '" <> e
            <> "'. Accepted values: regular, upgradable"

instance ToText ReservedNodeOfferingType where
  toText = \case
    Regular -> "Regular"
    Upgradable -> "Upgradable"

instance Hashable ReservedNodeOfferingType

instance NFData ReservedNodeOfferingType

instance ToByteString ReservedNodeOfferingType

instance ToQuery ReservedNodeOfferingType

instance ToHeader ReservedNodeOfferingType

instance FromXML ReservedNodeOfferingType where
  parseXML = parseXMLText "ReservedNodeOfferingType"
