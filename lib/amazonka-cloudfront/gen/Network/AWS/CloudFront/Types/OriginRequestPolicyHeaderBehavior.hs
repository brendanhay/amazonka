{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior where

import Network.AWS.Prelude

data OriginRequestPolicyHeaderBehavior
  = ORPHBAllViewer
  | ORPHBAllViewerAndWhitelistCloudFront
  | ORPHBNone
  | ORPHBWhitelist
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

instance FromText OriginRequestPolicyHeaderBehavior where
  parser =
    takeLowerText >>= \case
      "allviewer" -> pure ORPHBAllViewer
      "allviewerandwhitelistcloudfront" -> pure ORPHBAllViewerAndWhitelistCloudFront
      "none" -> pure ORPHBNone
      "whitelist" -> pure ORPHBWhitelist
      e ->
        fromTextError $
          "Failure parsing OriginRequestPolicyHeaderBehavior from value: '" <> e
            <> "'. Accepted values: allviewer, allviewerandwhitelistcloudfront, none, whitelist"

instance ToText OriginRequestPolicyHeaderBehavior where
  toText = \case
    ORPHBAllViewer -> "allViewer"
    ORPHBAllViewerAndWhitelistCloudFront -> "allViewerAndWhitelistCloudFront"
    ORPHBNone -> "none"
    ORPHBWhitelist -> "whitelist"

instance Hashable OriginRequestPolicyHeaderBehavior

instance NFData OriginRequestPolicyHeaderBehavior

instance ToByteString OriginRequestPolicyHeaderBehavior

instance ToQuery OriginRequestPolicyHeaderBehavior

instance ToHeader OriginRequestPolicyHeaderBehavior

instance FromXML OriginRequestPolicyHeaderBehavior where
  parseXML = parseXMLText "OriginRequestPolicyHeaderBehavior"

instance ToXML OriginRequestPolicyHeaderBehavior where
  toXML = toXMLText
