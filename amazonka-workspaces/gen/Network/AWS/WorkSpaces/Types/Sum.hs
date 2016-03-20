{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Sum where

import           Network.AWS.Prelude

data Compute
    = Performance
    | Standard
    | Value
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Compute where
    parser = takeLowerText >>= \case
        "performance" -> pure Performance
        "standard" -> pure Standard
        "value" -> pure Value
        e -> fromTextError $ "Failure parsing Compute from value: '" <> e
           <> "'. Accepted values: PERFORMANCE, STANDARD, VALUE"

instance ToText Compute where
    toText = \case
        Performance -> "PERFORMANCE"
        Standard -> "STANDARD"
        Value -> "VALUE"

instance Hashable     Compute
instance ToByteString Compute
instance ToQuery      Compute
instance ToHeader     Compute

instance FromJSON Compute where
    parseJSON = parseJSONText "Compute"

data WorkspaceDirectoryState
    = Deregistered
    | Deregistering
    | Error'
    | Registered
    | Registering
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText WorkspaceDirectoryState where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "deregistering" -> pure Deregistering
        "error" -> pure Error'
        "registered" -> pure Registered
        "registering" -> pure Registering
        e -> fromTextError $ "Failure parsing WorkspaceDirectoryState from value: '" <> e
           <> "'. Accepted values: DEREGISTERED, DEREGISTERING, ERROR, REGISTERED, REGISTERING"

instance ToText WorkspaceDirectoryState where
    toText = \case
        Deregistered -> "DEREGISTERED"
        Deregistering -> "DEREGISTERING"
        Error' -> "ERROR"
        Registered -> "REGISTERED"
        Registering -> "REGISTERING"

instance Hashable     WorkspaceDirectoryState
instance ToByteString WorkspaceDirectoryState
instance ToQuery      WorkspaceDirectoryState
instance ToHeader     WorkspaceDirectoryState

instance FromJSON WorkspaceDirectoryState where
    parseJSON = parseJSONText "WorkspaceDirectoryState"

data WorkspaceDirectoryType
    = AdConnector
    | SimpleAd
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText WorkspaceDirectoryType where
    parser = takeLowerText >>= \case
        "ad_connector" -> pure AdConnector
        "simple_ad" -> pure SimpleAd
        e -> fromTextError $ "Failure parsing WorkspaceDirectoryType from value: '" <> e
           <> "'. Accepted values: AD_CONNECTOR, SIMPLE_AD"

instance ToText WorkspaceDirectoryType where
    toText = \case
        AdConnector -> "AD_CONNECTOR"
        SimpleAd -> "SIMPLE_AD"

instance Hashable     WorkspaceDirectoryType
instance ToByteString WorkspaceDirectoryType
instance ToQuery      WorkspaceDirectoryType
instance ToHeader     WorkspaceDirectoryType

instance FromJSON WorkspaceDirectoryType where
    parseJSON = parseJSONText "WorkspaceDirectoryType"

data WorkspaceState
    = WSAvailable
    | WSError'
    | WSImpaired
    | WSPending
    | WSRebooting
    | WSRebuilding
    | WSSuspended
    | WSTerminated
    | WSTerminating
    | WSUnhealthy
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText WorkspaceState where
    parser = takeLowerText >>= \case
        "available" -> pure WSAvailable
        "error" -> pure WSError'
        "impaired" -> pure WSImpaired
        "pending" -> pure WSPending
        "rebooting" -> pure WSRebooting
        "rebuilding" -> pure WSRebuilding
        "suspended" -> pure WSSuspended
        "terminated" -> pure WSTerminated
        "terminating" -> pure WSTerminating
        "unhealthy" -> pure WSUnhealthy
        e -> fromTextError $ "Failure parsing WorkspaceState from value: '" <> e
           <> "'. Accepted values: AVAILABLE, ERROR, IMPAIRED, PENDING, REBOOTING, REBUILDING, SUSPENDED, TERMINATED, TERMINATING, UNHEALTHY"

instance ToText WorkspaceState where
    toText = \case
        WSAvailable -> "AVAILABLE"
        WSError' -> "ERROR"
        WSImpaired -> "IMPAIRED"
        WSPending -> "PENDING"
        WSRebooting -> "REBOOTING"
        WSRebuilding -> "REBUILDING"
        WSSuspended -> "SUSPENDED"
        WSTerminated -> "TERMINATED"
        WSTerminating -> "TERMINATING"
        WSUnhealthy -> "UNHEALTHY"

instance Hashable     WorkspaceState
instance ToByteString WorkspaceState
instance ToQuery      WorkspaceState
instance ToHeader     WorkspaceState

instance FromJSON WorkspaceState where
    parseJSON = parseJSONText "WorkspaceState"
