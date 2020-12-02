{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior where

import Network.AWS.Prelude

data CachePolicyHeaderBehavior
  = CPHBNone
  | CPHBWhitelist
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

instance FromText CachePolicyHeaderBehavior where
  parser =
    takeLowerText >>= \case
      "none" -> pure CPHBNone
      "whitelist" -> pure CPHBWhitelist
      e ->
        fromTextError $
          "Failure parsing CachePolicyHeaderBehavior from value: '" <> e
            <> "'. Accepted values: none, whitelist"

instance ToText CachePolicyHeaderBehavior where
  toText = \case
    CPHBNone -> "none"
    CPHBWhitelist -> "whitelist"

instance Hashable CachePolicyHeaderBehavior

instance NFData CachePolicyHeaderBehavior

instance ToByteString CachePolicyHeaderBehavior

instance ToQuery CachePolicyHeaderBehavior

instance ToHeader CachePolicyHeaderBehavior

instance FromXML CachePolicyHeaderBehavior where
  parseXML = parseXMLText "CachePolicyHeaderBehavior"

instance ToXML CachePolicyHeaderBehavior where
  toXML = toXMLText
