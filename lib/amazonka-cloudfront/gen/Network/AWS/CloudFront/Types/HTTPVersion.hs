{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.HTTPVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.HTTPVersion where

import Network.AWS.Prelude

data HTTPVersion
  = HTTP1_1
  | HTTP2
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

instance FromText HTTPVersion where
  parser =
    takeLowerText >>= \case
      "http1.1" -> pure HTTP1_1
      "http2" -> pure HTTP2
      e ->
        fromTextError $
          "Failure parsing HTTPVersion from value: '" <> e
            <> "'. Accepted values: http1.1, http2"

instance ToText HTTPVersion where
  toText = \case
    HTTP1_1 -> "http1.1"
    HTTP2 -> "http2"

instance Hashable HTTPVersion

instance NFData HTTPVersion

instance ToByteString HTTPVersion

instance ToQuery HTTPVersion

instance ToHeader HTTPVersion

instance FromXML HTTPVersion where
  parseXML = parseXMLText "HTTPVersion"

instance ToXML HTTPVersion where
  toXML = toXMLText
