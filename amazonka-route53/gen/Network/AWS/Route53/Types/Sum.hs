{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Sum where

import           Network.AWS.Prelude
import           Network.AWS.Route53.Internal

data ChangeAction
    = Create
    | Upsert
    | Delete
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "create" -> pure Create
        "delete" -> pure Delete
        "upsert" -> pure Upsert
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: create, delete, upsert"

instance ToText ChangeAction where
    toText = \case
        Create -> "create"
        Delete -> "delete"
        Upsert -> "upsert"

instance Hashable ChangeAction
instance ToQuery ChangeAction
instance ToHeader ChangeAction

instance ToXML ChangeAction where
    toXML = toXMLText

data ChangeStatus
    = Pending
    | Insync
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ChangeStatus where
    parser = takeLowerText >>= \case
        "insync" -> pure Insync
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing ChangeStatus from value: '" <> e
           <> "'. Accepted values: insync, pending"

instance ToText ChangeStatus where
    toText = \case
        Insync -> "insync"
        Pending -> "pending"

instance Hashable ChangeStatus
instance ToQuery ChangeStatus
instance ToHeader ChangeStatus

instance FromXML ChangeStatus where
    parseXML = parseXMLText "ChangeStatus"

data Failover
    = Secondary
    | Primary
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Failover where
    parser = takeLowerText >>= \case
        "primary" -> pure Primary
        "secondary" -> pure Secondary
        e -> fromTextError $ "Failure parsing Failover from value: '" <> e
           <> "'. Accepted values: primary, secondary"

instance ToText Failover where
    toText = \case
        Primary -> "primary"
        Secondary -> "secondary"

instance Hashable Failover
instance ToQuery Failover
instance ToHeader Failover

instance FromXML Failover where
    parseXML = parseXMLText "Failover"

instance ToXML Failover where
    toXML = toXMLText

data HealthCheckType
    = HTTPS
    | TCP
    | HTTPSStrMatch
    | HTTP
    | HTTPStrMatch
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText HealthCheckType where
    parser = takeLowerText >>= \case
        "http" -> pure HTTP
        "https" -> pure HTTPS
        "https_str_match" -> pure HTTPSStrMatch
        "http_str_match" -> pure HTTPStrMatch
        "tcp" -> pure TCP
        e -> fromTextError $ "Failure parsing HealthCheckType from value: '" <> e
           <> "'. Accepted values: http, https, https_str_match, http_str_match, tcp"

instance ToText HealthCheckType where
    toText = \case
        HTTP -> "http"
        HTTPS -> "https"
        HTTPSStrMatch -> "https_str_match"
        HTTPStrMatch -> "http_str_match"
        TCP -> "tcp"

instance Hashable HealthCheckType
instance ToQuery HealthCheckType
instance ToHeader HealthCheckType

instance FromXML HealthCheckType where
    parseXML = parseXMLText "HealthCheckType"

instance ToXML HealthCheckType where
    toXML = toXMLText

data RecordType
    = Cname
    | Srv
    | MX
    | NS
    | Aaaa
    | A
    | Spf
    | Soa
    | Txt
    | Ptr
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RecordType where
    parser = takeLowerText >>= \case
        "a" -> pure A
        "aaaa" -> pure Aaaa
        "cname" -> pure Cname
        "mx" -> pure MX
        "ns" -> pure NS
        "ptr" -> pure Ptr
        "soa" -> pure Soa
        "spf" -> pure Spf
        "srv" -> pure Srv
        "txt" -> pure Txt
        e -> fromTextError $ "Failure parsing RecordType from value: '" <> e
           <> "'. Accepted values: a, aaaa, cname, mx, ns, ptr, soa, spf, srv, txt"

instance ToText RecordType where
    toText = \case
        A -> "a"
        Aaaa -> "aaaa"
        Cname -> "cname"
        MX -> "mx"
        NS -> "ns"
        Ptr -> "ptr"
        Soa -> "soa"
        Spf -> "spf"
        Srv -> "srv"
        Txt -> "txt"

instance Hashable RecordType
instance ToQuery RecordType
instance ToHeader RecordType

instance FromXML RecordType where
    parseXML = parseXMLText "RecordType"

instance ToXML RecordType where
    toXML = toXMLText

data TagResourceType
    = Healthcheck
    | Hostedzone
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TagResourceType where
    parser = takeLowerText >>= \case
        "healthcheck" -> pure Healthcheck
        "hostedzone" -> pure Hostedzone
        e -> fromTextError $ "Failure parsing TagResourceType from value: '" <> e
           <> "'. Accepted values: healthcheck, hostedzone"

instance ToText TagResourceType where
    toText = \case
        Healthcheck -> "healthcheck"
        Hostedzone -> "hostedzone"

instance Hashable TagResourceType
instance ToQuery TagResourceType
instance ToHeader TagResourceType

instance FromXML TagResourceType where
    parseXML = parseXMLText "TagResourceType"

instance ToXML TagResourceType where
    toXML = toXMLText

data VPCRegion
    = ApNortheast1
    | SaEast1
    | CnNorth1
    | UsWest2
    | UsEast1
    | EuWest1
    | EuCentral1
    | UsWest1
    | ApSoutheast2
    | ApSoutheast1
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VPCRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure ApNortheast1
        "ap-southeast-1" -> pure ApSoutheast1
        "ap-southeast-2" -> pure ApSoutheast2
        "cn-north-1" -> pure CnNorth1
        "eu-central-1" -> pure EuCentral1
        "eu-west-1" -> pure EuWest1
        "sa-east-1" -> pure SaEast1
        "us-east-1" -> pure UsEast1
        "us-west-1" -> pure UsWest1
        "us-west-2" -> pure UsWest2
        e -> fromTextError $ "Failure parsing VPCRegion from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-southeast-1, ap-southeast-2, cn-north-1, eu-central-1, eu-west-1, sa-east-1, us-east-1, us-west-1, us-west-2"

instance ToText VPCRegion where
    toText = \case
        ApNortheast1 -> "ap-northeast-1"
        ApSoutheast1 -> "ap-southeast-1"
        ApSoutheast2 -> "ap-southeast-2"
        CnNorth1 -> "cn-north-1"
        EuCentral1 -> "eu-central-1"
        EuWest1 -> "eu-west-1"
        SaEast1 -> "sa-east-1"
        UsEast1 -> "us-east-1"
        UsWest1 -> "us-west-1"
        UsWest2 -> "us-west-2"

instance Hashable VPCRegion
instance ToQuery VPCRegion
instance ToHeader VPCRegion

instance FromXML VPCRegion where
    parseXML = parseXMLText "VPCRegion"

instance ToXML VPCRegion where
    toXML = toXMLText
