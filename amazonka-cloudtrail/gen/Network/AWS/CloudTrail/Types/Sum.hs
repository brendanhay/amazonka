{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Sum where

import           Network.AWS.Prelude

data LookupAttributeKey
    = ResourceType
    | ResourceName
    | Username
    | EventName
    | EventId
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText LookupAttributeKey where
    parser = takeLowerText >>= \case
        "eventid" -> pure EventId
        "eventname" -> pure EventName
        "resourcename" -> pure ResourceName
        "resourcetype" -> pure ResourceType
        "username" -> pure Username
        e -> fromTextError $ "Failure parsing LookupAttributeKey from value: '" <> e
           <> "'. Accepted values: eventid, eventname, resourcename, resourcetype, username"

instance ToText LookupAttributeKey where
    toText = \case
        EventId -> "eventid"
        EventName -> "eventname"
        ResourceName -> "resourcename"
        ResourceType -> "resourcetype"
        Username -> "username"

instance Hashable LookupAttributeKey
instance ToQuery LookupAttributeKey
instance ToHeader LookupAttributeKey

instance ToJSON LookupAttributeKey where
    toJSON = toJSONText
