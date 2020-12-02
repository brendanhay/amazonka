{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProductCodeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProductCodeValues where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ProductCodeValues
  = Devpay
  | Marketplace
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

instance FromText ProductCodeValues where
  parser =
    takeLowerText >>= \case
      "devpay" -> pure Devpay
      "marketplace" -> pure Marketplace
      e ->
        fromTextError $
          "Failure parsing ProductCodeValues from value: '" <> e
            <> "'. Accepted values: devpay, marketplace"

instance ToText ProductCodeValues where
  toText = \case
    Devpay -> "devpay"
    Marketplace -> "marketplace"

instance Hashable ProductCodeValues

instance NFData ProductCodeValues

instance ToByteString ProductCodeValues

instance ToQuery ProductCodeValues

instance ToHeader ProductCodeValues

instance FromXML ProductCodeValues where
  parseXML = parseXMLText "ProductCodeValues"
