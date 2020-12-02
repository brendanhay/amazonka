{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EventType where

import Network.AWS.Prelude

data EventType
  = OriginRequest
  | OriginResponse
  | ViewerRequest
  | ViewerResponse
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

instance FromText EventType where
  parser =
    takeLowerText >>= \case
      "origin-request" -> pure OriginRequest
      "origin-response" -> pure OriginResponse
      "viewer-request" -> pure ViewerRequest
      "viewer-response" -> pure ViewerResponse
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: origin-request, origin-response, viewer-request, viewer-response"

instance ToText EventType where
  toText = \case
    OriginRequest -> "origin-request"
    OriginResponse -> "origin-response"
    ViewerRequest -> "viewer-request"
    ViewerResponse -> "viewer-response"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance FromXML EventType where
  parseXML = parseXMLText "EventType"

instance ToXML EventType where
  toXML = toXMLText
