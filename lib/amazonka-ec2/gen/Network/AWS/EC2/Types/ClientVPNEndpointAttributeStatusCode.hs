{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNEndpointAttributeStatusCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ClientVPNEndpointAttributeStatusCode
  = Applied
  | Applying
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

instance FromText ClientVPNEndpointAttributeStatusCode where
  parser =
    takeLowerText >>= \case
      "applied" -> pure Applied
      "applying" -> pure Applying
      e ->
        fromTextError $
          "Failure parsing ClientVPNEndpointAttributeStatusCode from value: '" <> e
            <> "'. Accepted values: applied, applying"

instance ToText ClientVPNEndpointAttributeStatusCode where
  toText = \case
    Applied -> "applied"
    Applying -> "applying"

instance Hashable ClientVPNEndpointAttributeStatusCode

instance NFData ClientVPNEndpointAttributeStatusCode

instance ToByteString ClientVPNEndpointAttributeStatusCode

instance ToQuery ClientVPNEndpointAttributeStatusCode

instance ToHeader ClientVPNEndpointAttributeStatusCode

instance FromXML ClientVPNEndpointAttributeStatusCode where
  parseXML = parseXMLText "ClientVPNEndpointAttributeStatusCode"
