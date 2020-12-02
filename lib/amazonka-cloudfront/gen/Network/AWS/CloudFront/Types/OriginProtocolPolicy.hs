{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginProtocolPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginProtocolPolicy where

import Network.AWS.Prelude

data OriginProtocolPolicy
  = HTTPOnly
  | HTTPSOnly
  | MatchViewer
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

instance FromText OriginProtocolPolicy where
  parser =
    takeLowerText >>= \case
      "http-only" -> pure HTTPOnly
      "https-only" -> pure HTTPSOnly
      "match-viewer" -> pure MatchViewer
      e ->
        fromTextError $
          "Failure parsing OriginProtocolPolicy from value: '" <> e
            <> "'. Accepted values: http-only, https-only, match-viewer"

instance ToText OriginProtocolPolicy where
  toText = \case
    HTTPOnly -> "http-only"
    HTTPSOnly -> "https-only"
    MatchViewer -> "match-viewer"

instance Hashable OriginProtocolPolicy

instance NFData OriginProtocolPolicy

instance ToByteString OriginProtocolPolicy

instance ToQuery OriginProtocolPolicy

instance ToHeader OriginProtocolPolicy

instance FromXML OriginProtocolPolicy where
  parseXML = parseXMLText "OriginProtocolPolicy"

instance ToXML OriginProtocolPolicy where
  toXML = toXMLText
