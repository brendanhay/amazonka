{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Sum where

import           Network.AWS.Prelude

data ParameterApplyType
    = Dynamic
    | Static
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ParameterApplyType where
    parser = takeLowerText >>= \case
        "dynamic" -> pure Dynamic
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing ParameterApplyType from value: '" <> e
           <> "'. Accepted values: dynamic, static"

instance ToText ParameterApplyType where
    toText = \case
        Dynamic -> "dynamic"
        Static -> "static"

instance Hashable     ParameterApplyType
instance ToByteString ParameterApplyType
instance ToQuery      ParameterApplyType
instance ToHeader     ParameterApplyType

instance FromXML ParameterApplyType where
    parseXML = parseXMLText "ParameterApplyType"

data SourceType
    = Cluster
    | ClusterParameterGroup
    | ClusterSecurityGroup
    | ClusterSnapshot
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        "cluster-parameter-group" -> pure ClusterParameterGroup
        "cluster-security-group" -> pure ClusterSecurityGroup
        "cluster-snapshot" -> pure ClusterSnapshot
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: cluster, cluster-parameter-group, cluster-security-group, cluster-snapshot"

instance ToText SourceType where
    toText = \case
        Cluster -> "cluster"
        ClusterParameterGroup -> "cluster-parameter-group"
        ClusterSecurityGroup -> "cluster-security-group"
        ClusterSnapshot -> "cluster-snapshot"

instance Hashable     SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"
