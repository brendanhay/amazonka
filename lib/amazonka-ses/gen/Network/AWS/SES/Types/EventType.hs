{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.EventType where

import Network.AWS.Prelude

data EventType
  = ETBounce
  | ETClick
  | ETComplaint
  | ETDelivery
  | ETOpen
  | ETReject
  | ETRenderingFailure
  | ETSend
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
      "bounce" -> pure ETBounce
      "click" -> pure ETClick
      "complaint" -> pure ETComplaint
      "delivery" -> pure ETDelivery
      "open" -> pure ETOpen
      "reject" -> pure ETReject
      "renderingfailure" -> pure ETRenderingFailure
      "send" -> pure ETSend
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: bounce, click, complaint, delivery, open, reject, renderingfailure, send"

instance ToText EventType where
  toText = \case
    ETBounce -> "bounce"
    ETClick -> "click"
    ETComplaint -> "complaint"
    ETDelivery -> "delivery"
    ETOpen -> "open"
    ETReject -> "reject"
    ETRenderingFailure -> "renderingFailure"
    ETSend -> "send"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance FromXML EventType where
  parseXML = parseXMLText "EventType"
