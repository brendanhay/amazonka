{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerProtocol where

import Network.AWS.Prelude

data LoadBalancerProtocol
  = HTTP
  | HTTPHTTPS
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

instance FromText LoadBalancerProtocol where
  parser =
    takeLowerText >>= \case
      "http" -> pure HTTP
      "http_https" -> pure HTTPHTTPS
      e ->
        fromTextError $
          "Failure parsing LoadBalancerProtocol from value: '" <> e
            <> "'. Accepted values: http, http_https"

instance ToText LoadBalancerProtocol where
  toText = \case
    HTTP -> "HTTP"
    HTTPHTTPS -> "HTTP_HTTPS"

instance Hashable LoadBalancerProtocol

instance NFData LoadBalancerProtocol

instance ToByteString LoadBalancerProtocol

instance ToQuery LoadBalancerProtocol

instance ToHeader LoadBalancerProtocol

instance FromJSON LoadBalancerProtocol where
  parseJSON = parseJSONText "LoadBalancerProtocol"
