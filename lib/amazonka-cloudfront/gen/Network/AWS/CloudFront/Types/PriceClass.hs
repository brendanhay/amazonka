{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PriceClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PriceClass where

import Network.AWS.Prelude

data PriceClass
  = PriceClass100
  | PriceClass200
  | PriceClassAll
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

instance FromText PriceClass where
  parser =
    takeLowerText >>= \case
      "priceclass_100" -> pure PriceClass100
      "priceclass_200" -> pure PriceClass200
      "priceclass_all" -> pure PriceClassAll
      e ->
        fromTextError $
          "Failure parsing PriceClass from value: '" <> e
            <> "'. Accepted values: priceclass_100, priceclass_200, priceclass_all"

instance ToText PriceClass where
  toText = \case
    PriceClass100 -> "PriceClass_100"
    PriceClass200 -> "PriceClass_200"
    PriceClassAll -> "PriceClass_All"

instance Hashable PriceClass

instance NFData PriceClass

instance ToByteString PriceClass

instance ToQuery PriceClass

instance ToHeader PriceClass

instance FromXML PriceClass where
  parseXML = parseXMLText "PriceClass"

instance ToXML PriceClass where
  toXML = toXMLText
