{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OfferingClassType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OfferingClassType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OfferingClassType
  = OCTConvertible
  | OCTStandard
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

instance FromText OfferingClassType where
  parser =
    takeLowerText >>= \case
      "convertible" -> pure OCTConvertible
      "standard" -> pure OCTStandard
      e ->
        fromTextError $
          "Failure parsing OfferingClassType from value: '" <> e
            <> "'. Accepted values: convertible, standard"

instance ToText OfferingClassType where
  toText = \case
    OCTConvertible -> "convertible"
    OCTStandard -> "standard"

instance Hashable OfferingClassType

instance NFData OfferingClassType

instance ToByteString OfferingClassType

instance ToQuery OfferingClassType

instance ToHeader OfferingClassType

instance FromXML OfferingClassType where
  parseXML = parseXMLText "OfferingClassType"
