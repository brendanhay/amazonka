{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.OfferingClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.OfferingClass where

import Network.AWS.Prelude

data OfferingClass
  = Convertible
  | Standard
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

instance FromText OfferingClass where
  parser =
    takeLowerText >>= \case
      "convertible" -> pure Convertible
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing OfferingClass from value: '" <> e
            <> "'. Accepted values: convertible, standard"

instance ToText OfferingClass where
  toText = \case
    Convertible -> "CONVERTIBLE"
    Standard -> "STANDARD"

instance Hashable OfferingClass

instance NFData OfferingClass

instance ToByteString OfferingClass

instance ToQuery OfferingClass

instance ToHeader OfferingClass

instance ToJSON OfferingClass where
  toJSON = toJSONText

instance FromJSON OfferingClass where
  parseJSON = parseJSONText "OfferingClass"
