{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Sum where

import           Network.AWS.Prelude

data ApplyMethod
    = Immediate
    | PendingReboot
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ApplyMethod where
    parser = takeLowerText >>= \case
        "immediate" -> pure Immediate
        "pending-reboot" -> pure PendingReboot
        e -> fromTextError $ "Failure parsing ApplyMethod from value: '" <> e
           <> "'. Accepted values: immediate, pending-reboot"

instance ToText ApplyMethod where
    toText = \case
        Immediate -> "immediate"
        PendingReboot -> "pending-reboot"

instance Hashable     ApplyMethod
instance ToByteString ApplyMethod
instance ToQuery      ApplyMethod
instance ToHeader     ApplyMethod

instance FromXML ApplyMethod where
    parseXML = parseXMLText "ApplyMethod"

data SourceType
    = DBCluster
    | DBClusterSnapshot
    | DBInstance
    | DBParameterGroup
    | DBSecurityGroup
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "db-cluster" -> pure DBCluster
        "db-cluster-snapshot" -> pure DBClusterSnapshot
        "db-instance" -> pure DBInstance
        "db-parameter-group" -> pure DBParameterGroup
        "db-security-group" -> pure DBSecurityGroup
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: db-cluster, db-cluster-snapshot, db-instance, db-parameter-group, db-security-group"

instance ToText SourceType where
    toText = \case
        DBCluster -> "db-cluster"
        DBClusterSnapshot -> "db-cluster-snapshot"
        DBInstance -> "db-instance"
        DBParameterGroup -> "db-parameter-group"
        DBSecurityGroup -> "db-security-group"

instance Hashable     SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"
