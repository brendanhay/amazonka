{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ResourceType where

import Network.AWS.Prelude

data ResourceType
  = APIGateway
  | ApplicationLoadBalancer
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

instance FromText ResourceType where
  parser =
    takeLowerText >>= \case
      "api_gateway" -> pure APIGateway
      "application_load_balancer" -> pure ApplicationLoadBalancer
      e ->
        fromTextError $
          "Failure parsing ResourceType from value: '" <> e
            <> "'. Accepted values: api_gateway, application_load_balancer"

instance ToText ResourceType where
  toText = \case
    APIGateway -> "API_GATEWAY"
    ApplicationLoadBalancer -> "APPLICATION_LOAD_BALANCER"

instance Hashable ResourceType

instance NFData ResourceType

instance ToByteString ResourceType

instance ToQuery ResourceType

instance ToHeader ResourceType

instance ToJSON ResourceType where
  toJSON = toJSONText
