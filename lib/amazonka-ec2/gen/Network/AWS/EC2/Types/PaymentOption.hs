{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PaymentOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PaymentOption where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PaymentOption
  = POAllUpfront
  | PONoUpfront
  | POPartialUpfront
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

instance FromText PaymentOption where
  parser =
    takeLowerText >>= \case
      "allupfront" -> pure POAllUpfront
      "noupfront" -> pure PONoUpfront
      "partialupfront" -> pure POPartialUpfront
      e ->
        fromTextError $
          "Failure parsing PaymentOption from value: '" <> e
            <> "'. Accepted values: allupfront, noupfront, partialupfront"

instance ToText PaymentOption where
  toText = \case
    POAllUpfront -> "AllUpfront"
    PONoUpfront -> "NoUpfront"
    POPartialUpfront -> "PartialUpfront"

instance Hashable PaymentOption

instance NFData PaymentOption

instance ToByteString PaymentOption

instance ToQuery PaymentOption

instance ToHeader PaymentOption

instance FromXML PaymentOption where
  parseXML = parseXMLText "PaymentOption"
