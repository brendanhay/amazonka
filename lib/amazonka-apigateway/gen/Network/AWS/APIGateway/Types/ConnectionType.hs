{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.ConnectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ConnectionType where

import Network.AWS.Prelude

data ConnectionType
  = Internet
  | VPCLink
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

instance FromText ConnectionType where
  parser =
    takeLowerText >>= \case
      "internet" -> pure Internet
      "vpc_link" -> pure VPCLink
      e ->
        fromTextError $
          "Failure parsing ConnectionType from value: '" <> e
            <> "'. Accepted values: internet, vpc_link"

instance ToText ConnectionType where
  toText = \case
    Internet -> "INTERNET"
    VPCLink -> "VPC_LINK"

instance Hashable ConnectionType

instance NFData ConnectionType

instance ToByteString ConnectionType

instance ToQuery ConnectionType

instance ToHeader ConnectionType

instance ToJSON ConnectionType where
  toJSON = toJSONText

instance FromJSON ConnectionType where
  parseJSON = parseJSONText "ConnectionType"
