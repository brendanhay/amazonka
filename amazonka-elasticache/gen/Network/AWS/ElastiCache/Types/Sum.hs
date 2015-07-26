{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.Sum where

import           Network.AWS.Prelude

data AZMode
    = SingleAz
    | CrossAz
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AZMode where
    parser = takeLowerText >>= \case
        "cross-az" -> pure CrossAz
        "single-az" -> pure SingleAz
        e -> fromTextError $ "Failure parsing AZMode from value: '" <> e
           <> "'. Accepted values: cross-az, single-az"

instance ToText AZMode where
    toText = \case
        CrossAz -> "cross-az"
        SingleAz -> "single-az"

instance Hashable     AZMode
instance ToByteString AZMode
instance ToPath       AZMode
instance ToQuery      AZMode
instance ToHeader     AZMode

data AutomaticFailoverStatus
    = AFSEnabling
    | AFSDisabled
    | AFSDisabling
    | AFSEnabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AutomaticFailoverStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure AFSDisabled
        "disabling" -> pure AFSDisabling
        "enabled" -> pure AFSEnabled
        "enabling" -> pure AFSEnabling
        e -> fromTextError $ "Failure parsing AutomaticFailoverStatus from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText AutomaticFailoverStatus where
    toText = \case
        AFSDisabled -> "disabled"
        AFSDisabling -> "disabling"
        AFSEnabled -> "enabled"
        AFSEnabling -> "enabling"

instance Hashable     AutomaticFailoverStatus
instance ToByteString AutomaticFailoverStatus
instance ToPath       AutomaticFailoverStatus
instance ToQuery      AutomaticFailoverStatus
instance ToHeader     AutomaticFailoverStatus

instance FromXML AutomaticFailoverStatus where
    parseXML = parseXMLText "AutomaticFailoverStatus"

data PendingAutomaticFailoverStatus
    = Enabled
    | Disabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PendingAutomaticFailoverStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing PendingAutomaticFailoverStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText PendingAutomaticFailoverStatus where
    toText = \case
        Disabled -> "disabled"
        Enabled -> "enabled"

instance Hashable     PendingAutomaticFailoverStatus
instance ToByteString PendingAutomaticFailoverStatus
instance ToPath       PendingAutomaticFailoverStatus
instance ToQuery      PendingAutomaticFailoverStatus
instance ToHeader     PendingAutomaticFailoverStatus

instance FromXML PendingAutomaticFailoverStatus where
    parseXML = parseXMLText "PendingAutomaticFailoverStatus"

data SourceType
    = CacheSubnetGroup
    | CacheCluster
    | CacheParameterGroup
    | CacheSecurityGroup
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "cache-cluster" -> pure CacheCluster
        "cache-parameter-group" -> pure CacheParameterGroup
        "cache-security-group" -> pure CacheSecurityGroup
        "cache-subnet-group" -> pure CacheSubnetGroup
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: cache-cluster, cache-parameter-group, cache-security-group, cache-subnet-group"

instance ToText SourceType where
    toText = \case
        CacheCluster -> "cache-cluster"
        CacheParameterGroup -> "cache-parameter-group"
        CacheSecurityGroup -> "cache-security-group"
        CacheSubnetGroup -> "cache-subnet-group"

instance Hashable     SourceType
instance ToByteString SourceType
instance ToPath       SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"
