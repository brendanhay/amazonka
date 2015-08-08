{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Sum where

import           Network.AWS.Prelude

data Compute
    = Performance
    | Value
    | Standard
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Compute where
    parser = takeLowerText >>= \case
        "performance" -> pure Performance
        "standard" -> pure Standard
        "value" -> pure Value
        e -> fromTextError $ "Failure parsing Compute from value: '" <> e
           <> "'. Accepted values: performance, standard, value"

instance ToText Compute where
    toText = \case
        Performance -> "performance"
        Standard -> "standard"
        Value -> "value"

instance Hashable     Compute
instance ToByteString Compute
instance ToQuery      Compute
instance ToHeader     Compute

instance FromJSON Compute where
    parseJSON = parseJSONText "Compute"

data WorkspaceDirectoryState
    = Error'
    | Deregistering
    | Registered
    | Registering
    | Deregistered
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText WorkspaceDirectoryState where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "deregistering" -> pure Deregistering
        "error" -> pure Error'
        "registered" -> pure Registered
        "registering" -> pure Registering
        e -> fromTextError $ "Failure parsing WorkspaceDirectoryState from value: '" <> e
           <> "'. Accepted values: deregistered, deregistering, error, registered, registering"

instance ToText WorkspaceDirectoryState where
    toText = \case
        Deregistered -> "deregistered"
        Deregistering -> "deregistering"
        Error' -> "error"
        Registered -> "registered"
        Registering -> "registering"

instance Hashable     WorkspaceDirectoryState
instance ToByteString WorkspaceDirectoryState
instance ToQuery      WorkspaceDirectoryState
instance ToHeader     WorkspaceDirectoryState

instance FromJSON WorkspaceDirectoryState where
    parseJSON = parseJSONText "WorkspaceDirectoryState"

data WorkspaceDirectoryType
    = AdConnector
    | SimpleAd
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText WorkspaceDirectoryType where
    parser = takeLowerText >>= \case
        "ad_connector" -> pure AdConnector
        "simple_ad" -> pure SimpleAd
        e -> fromTextError $ "Failure parsing WorkspaceDirectoryType from value: '" <> e
           <> "'. Accepted values: ad_connector, simple_ad"

instance ToText WorkspaceDirectoryType where
    toText = \case
        AdConnector -> "ad_connector"
        SimpleAd -> "simple_ad"

instance Hashable     WorkspaceDirectoryType
instance ToByteString WorkspaceDirectoryType
instance ToQuery      WorkspaceDirectoryType
instance ToHeader     WorkspaceDirectoryType

instance FromJSON WorkspaceDirectoryType where
    parseJSON = parseJSONText "WorkspaceDirectoryType"

data WorkspaceState
    = WSSuspended
    | WSUnhealthy
    | WSRebooting
    | WSTerminating
    | WSImpaired
    | WSError'
    | WSPending
    | WSRebuilding
    | WSAvailable
    | WSTerminated
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
           <> "'. Accepted values: available, error, impaired, pending, rebooting, rebuilding, suspended, terminated, terminating, unhealthy"

instance ToText WorkspaceState where
    toText = \case
        WSAvailable -> "available"
        WSError' -> "error"
        WSImpaired -> "impaired"
        WSPending -> "pending"
        WSRebooting -> "rebooting"
        WSRebuilding -> "rebuilding"
        WSSuspended -> "suspended"
        WSTerminated -> "terminated"
        WSTerminating -> "terminating"
        WSUnhealthy -> "unhealthy"

instance Hashable     WorkspaceState
instance ToByteString WorkspaceState
instance ToQuery      WorkspaceState
instance ToHeader     WorkspaceState

instance FromJSON WorkspaceState where
    parseJSON = parseJSONText "WorkspaceState"
