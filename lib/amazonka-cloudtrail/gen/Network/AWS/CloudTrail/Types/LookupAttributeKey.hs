{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.LookupAttributeKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.LookupAttributeKey where

import Network.AWS.Prelude

data LookupAttributeKey
  = AccessKeyId
  | EventId
  | EventName
  | EventSource
  | ReadOnly
  | ResourceName
  | ResourceType
  | Username
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

instance FromText LookupAttributeKey where
  parser =
    takeLowerText >>= \case
      "accesskeyid" -> pure AccessKeyId
      "eventid" -> pure EventId
      "eventname" -> pure EventName
      "eventsource" -> pure EventSource
      "readonly" -> pure ReadOnly
      "resourcename" -> pure ResourceName
      "resourcetype" -> pure ResourceType
      "username" -> pure Username
      e ->
        fromTextError $
          "Failure parsing LookupAttributeKey from value: '" <> e
            <> "'. Accepted values: accesskeyid, eventid, eventname, eventsource, readonly, resourcename, resourcetype, username"

instance ToText LookupAttributeKey where
  toText = \case
    AccessKeyId -> "AccessKeyId"
    EventId -> "EventId"
    EventName -> "EventName"
    EventSource -> "EventSource"
    ReadOnly -> "ReadOnly"
    ResourceName -> "ResourceName"
    ResourceType -> "ResourceType"
    Username -> "Username"

instance Hashable LookupAttributeKey

instance NFData LookupAttributeKey

instance ToByteString LookupAttributeKey

instance ToQuery LookupAttributeKey

instance ToHeader LookupAttributeKey

instance ToJSON LookupAttributeKey where
  toJSON = toJSONText
