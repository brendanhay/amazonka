{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Sum where

import           Network.AWS.Prelude

data LookupAttributeKey
    = EventId
    | EventName
    | ResourceName
    | ResourceType
    | Username
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText LookupAttributeKey where
    parser = takeLowerText >>= \case
        "eventid" -> pure EventId
        "eventname" -> pure EventName
        "resourcename" -> pure ResourceName
        "resourcetype" -> pure ResourceType
        "username" -> pure Username
        e -> fromTextError $ "Failure parsing LookupAttributeKey from value: '" <> e
           <> "'. Accepted values: EventId, EventName, ResourceName, ResourceType, Username"

instance ToText LookupAttributeKey where
    toText = \case
        EventId -> "EventId"
        EventName -> "EventName"
        ResourceName -> "ResourceName"
        ResourceType -> "ResourceType"
        Username -> "Username"

instance Hashable     LookupAttributeKey
instance ToByteString LookupAttributeKey
instance ToQuery      LookupAttributeKey
instance ToHeader     LookupAttributeKey

instance ToJSON LookupAttributeKey where
    toJSON = toJSONText
