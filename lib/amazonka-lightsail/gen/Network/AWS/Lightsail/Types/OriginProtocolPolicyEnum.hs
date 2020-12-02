{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum where

import Network.AWS.Prelude

data OriginProtocolPolicyEnum
  = HTTPOnly
  | HTTPSOnly
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

instance FromText OriginProtocolPolicyEnum where
  parser =
    takeLowerText >>= \case
      "http-only" -> pure HTTPOnly
      "https-only" -> pure HTTPSOnly
      e ->
        fromTextError $
          "Failure parsing OriginProtocolPolicyEnum from value: '" <> e
            <> "'. Accepted values: http-only, https-only"

instance ToText OriginProtocolPolicyEnum where
  toText = \case
    HTTPOnly -> "http-only"
    HTTPSOnly -> "https-only"

instance Hashable OriginProtocolPolicyEnum

instance NFData OriginProtocolPolicyEnum

instance ToByteString OriginProtocolPolicyEnum

instance ToQuery OriginProtocolPolicyEnum

instance ToHeader OriginProtocolPolicyEnum

instance ToJSON OriginProtocolPolicyEnum where
  toJSON = toJSONText

instance FromJSON OriginProtocolPolicyEnum where
  parseJSON = parseJSONText "OriginProtocolPolicyEnum"
