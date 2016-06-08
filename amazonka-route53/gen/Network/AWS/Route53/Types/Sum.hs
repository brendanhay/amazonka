{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Sum where

import           Network.AWS.Prelude
import           Network.AWS.Route53.Internal

data ChangeAction
    = Create
    | Delete
    | Upsert
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "create" -> pure Create
        "delete" -> pure Delete
        "upsert" -> pure Upsert
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: CREATE, DELETE, UPSERT"

instance ToText ChangeAction where
    toText = \case
        Create -> "CREATE"
        Delete -> "DELETE"
        Upsert -> "UPSERT"

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance ToXML ChangeAction where
    toXML = toXMLText

data ChangeStatus
    = Insync
    | Pending
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ChangeStatus where
    parser = takeLowerText >>= \case
        "insync" -> pure Insync
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing ChangeStatus from value: '" <> e
           <> "'. Accepted values: INSYNC, PENDING"

instance ToText ChangeStatus where
    toText = \case
        Insync -> "INSYNC"
        Pending -> "PENDING"

instance Hashable     ChangeStatus
instance NFData       ChangeStatus
instance ToByteString ChangeStatus
instance ToQuery      ChangeStatus
instance ToHeader     ChangeStatus

instance FromXML ChangeStatus where
    parseXML = parseXMLText "ChangeStatus"

data CloudWatchRegion
    = CWRApNortheast1
    | CWRApNortheast2
    | CWRApSoutheast1
    | CWRApSoutheast2
    | CWREuCentral1
    | CWREuWest1
    | CWRSaEast1
    | CWRUsEast1
    | CWRUsWest1
    | CWRUsWest2
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CloudWatchRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure CWRApNortheast1
        "ap-northeast-2" -> pure CWRApNortheast2
        "ap-southeast-1" -> pure CWRApSoutheast1
        "ap-southeast-2" -> pure CWRApSoutheast2
        "eu-central-1" -> pure CWREuCentral1
        "eu-west-1" -> pure CWREuWest1
        "sa-east-1" -> pure CWRSaEast1
        "us-east-1" -> pure CWRUsEast1
        "us-west-1" -> pure CWRUsWest1
        "us-west-2" -> pure CWRUsWest2
        e -> fromTextError $ "Failure parsing CloudWatchRegion from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-northeast-2, ap-southeast-1, ap-southeast-2, eu-central-1, eu-west-1, sa-east-1, us-east-1, us-west-1, us-west-2"

instance ToText CloudWatchRegion where
    toText = \case
        CWRApNortheast1 -> "ap-northeast-1"
        CWRApNortheast2 -> "ap-northeast-2"
        CWRApSoutheast1 -> "ap-southeast-1"
        CWRApSoutheast2 -> "ap-southeast-2"
        CWREuCentral1 -> "eu-central-1"
        CWREuWest1 -> "eu-west-1"
        CWRSaEast1 -> "sa-east-1"
        CWRUsEast1 -> "us-east-1"
        CWRUsWest1 -> "us-west-1"
        CWRUsWest2 -> "us-west-2"

instance Hashable     CloudWatchRegion
instance NFData       CloudWatchRegion
instance ToByteString CloudWatchRegion
instance ToQuery      CloudWatchRegion
instance ToHeader     CloudWatchRegion

instance FromXML CloudWatchRegion where
    parseXML = parseXMLText "CloudWatchRegion"

instance ToXML CloudWatchRegion where
    toXML = toXMLText

data ComparisonOperator
    = GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanOrEqualToThreshold
    | LessThanThreshold
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
        "greaterthanthreshold" -> pure GreaterThanThreshold
        "lessthanorequaltothreshold" -> pure LessThanOrEqualToThreshold
        "lessthanthreshold" -> pure LessThanThreshold
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: GreaterThanOrEqualToThreshold, GreaterThanThreshold, LessThanOrEqualToThreshold, LessThanThreshold"

instance ToText ComparisonOperator where
    toText = \case
        GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
        GreaterThanThreshold -> "GreaterThanThreshold"
        LessThanOrEqualToThreshold -> "LessThanOrEqualToThreshold"
        LessThanThreshold -> "LessThanThreshold"

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance FromXML ComparisonOperator where
    parseXML = parseXMLText "ComparisonOperator"

data Failover
    = Primary
    | Secondary
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Failover where
    parser = takeLowerText >>= \case
        "primary" -> pure Primary
        "secondary" -> pure Secondary
        e -> fromTextError $ "Failure parsing Failover from value: '" <> e
           <> "'. Accepted values: PRIMARY, SECONDARY"

instance ToText Failover where
    toText = \case
        Primary -> "PRIMARY"
        Secondary -> "SECONDARY"

instance Hashable     Failover
instance NFData       Failover
instance ToByteString Failover
instance ToQuery      Failover
instance ToHeader     Failover

instance FromXML Failover where
    parseXML = parseXMLText "Failover"

instance ToXML Failover where
    toXML = toXMLText

-- | An Amazon EC2 region that you want Amazon Route 53 to use to perform health checks.
data HealthCheckRegion
    = HCRApNortheast1
    | HCRApSoutheast1
    | HCRApSoutheast2
    | HCREuWest1
    | HCRSaEast1
    | HCRUsEast1
    | HCRUsWest1
    | HCRUsWest2
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText HealthCheckRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure HCRApNortheast1
        "ap-southeast-1" -> pure HCRApSoutheast1
        "ap-southeast-2" -> pure HCRApSoutheast2
        "eu-west-1" -> pure HCREuWest1
        "sa-east-1" -> pure HCRSaEast1
        "us-east-1" -> pure HCRUsEast1
        "us-west-1" -> pure HCRUsWest1
        "us-west-2" -> pure HCRUsWest2
        e -> fromTextError $ "Failure parsing HealthCheckRegion from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1, sa-east-1, us-east-1, us-west-1, us-west-2"

instance ToText HealthCheckRegion where
    toText = \case
        HCRApNortheast1 -> "ap-northeast-1"
        HCRApSoutheast1 -> "ap-southeast-1"
        HCRApSoutheast2 -> "ap-southeast-2"
        HCREuWest1 -> "eu-west-1"
        HCRSaEast1 -> "sa-east-1"
        HCRUsEast1 -> "us-east-1"
        HCRUsWest1 -> "us-west-1"
        HCRUsWest2 -> "us-west-2"

instance Hashable     HealthCheckRegion
instance NFData       HealthCheckRegion
instance ToByteString HealthCheckRegion
instance ToQuery      HealthCheckRegion
instance ToHeader     HealthCheckRegion

instance FromXML HealthCheckRegion where
    parseXML = parseXMLText "HealthCheckRegion"

instance ToXML HealthCheckRegion where
    toXML = toXMLText

data HealthCheckType
    = Calculated
    | CloudwatchMetric
    | HTTP
    | HTTPS
    | HTTPSStrMatch
    | HTTPStrMatch
    | TCP
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText HealthCheckType where
    parser = takeLowerText >>= \case
        "calculated" -> pure Calculated
        "cloudwatch_metric" -> pure CloudwatchMetric
        "http" -> pure HTTP
        "https" -> pure HTTPS
        "https_str_match" -> pure HTTPSStrMatch
        "http_str_match" -> pure HTTPStrMatch
        "tcp" -> pure TCP
        e -> fromTextError $ "Failure parsing HealthCheckType from value: '" <> e
           <> "'. Accepted values: CALCULATED, CLOUDWATCH_METRIC, HTTP, HTTPS, HTTPS_STR_MATCH, HTTP_STR_MATCH, TCP"

instance ToText HealthCheckType where
    toText = \case
        Calculated -> "CALCULATED"
        CloudwatchMetric -> "CLOUDWATCH_METRIC"
        HTTP -> "HTTP"
        HTTPS -> "HTTPS"
        HTTPSStrMatch -> "HTTPS_STR_MATCH"
        HTTPStrMatch -> "HTTP_STR_MATCH"
        TCP -> "TCP"

instance Hashable     HealthCheckType
instance NFData       HealthCheckType
instance ToByteString HealthCheckType
instance ToQuery      HealthCheckType
instance ToHeader     HealthCheckType

instance FromXML HealthCheckType where
    parseXML = parseXMLText "HealthCheckType"

instance ToXML HealthCheckType where
    toXML = toXMLText

data InsufficientDataHealthStatus
    = Healthy
    | LastKnownStatus
    | Unhealthy
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InsufficientDataHealthStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure Healthy
        "lastknownstatus" -> pure LastKnownStatus
        "unhealthy" -> pure Unhealthy
        e -> fromTextError $ "Failure parsing InsufficientDataHealthStatus from value: '" <> e
           <> "'. Accepted values: Healthy, LastKnownStatus, Unhealthy"

instance ToText InsufficientDataHealthStatus where
    toText = \case
        Healthy -> "Healthy"
        LastKnownStatus -> "LastKnownStatus"
        Unhealthy -> "Unhealthy"

instance Hashable     InsufficientDataHealthStatus
instance NFData       InsufficientDataHealthStatus
instance ToByteString InsufficientDataHealthStatus
instance ToQuery      InsufficientDataHealthStatus
instance ToHeader     InsufficientDataHealthStatus

instance FromXML InsufficientDataHealthStatus where
    parseXML = parseXMLText "InsufficientDataHealthStatus"

instance ToXML InsufficientDataHealthStatus where
    toXML = toXMLText

data RecordType
    = A
    | Aaaa
    | Cname
    | MX
    | NS
    | Ptr
    | Soa
    | Spf
    | Srv
    | Txt
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: A, AAAA, CNAME, MX, NS, PTR, SOA, SPF, SRV, TXT"

instance ToText RecordType where
    toText = \case
        A -> "A"
        Aaaa -> "AAAA"
        Cname -> "CNAME"
        MX -> "MX"
        NS -> "NS"
        Ptr -> "PTR"
        Soa -> "SOA"
        Spf -> "SPF"
        Srv -> "SRV"
        Txt -> "TXT"

instance Hashable     RecordType
instance NFData       RecordType
instance ToByteString RecordType
instance ToQuery      RecordType
instance ToHeader     RecordType

instance FromXML RecordType where
    parseXML = parseXMLText "RecordType"

instance ToXML RecordType where
    toXML = toXMLText

data Statistic
    = Average
    | Maximum
    | Minimum
    | SampleCount
    | Sum
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Statistic where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        "samplecount" -> pure SampleCount
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing Statistic from value: '" <> e
           <> "'. Accepted values: Average, Maximum, Minimum, SampleCount, Sum"

instance ToText Statistic where
    toText = \case
        Average -> "Average"
        Maximum -> "Maximum"
        Minimum -> "Minimum"
        SampleCount -> "SampleCount"
        Sum -> "Sum"

instance Hashable     Statistic
instance NFData       Statistic
instance ToByteString Statistic
instance ToQuery      Statistic
instance ToHeader     Statistic

instance FromXML Statistic where
    parseXML = parseXMLText "Statistic"

data TagResourceType
    = Healthcheck
    | Hostedzone
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

instance Hashable     TagResourceType
instance NFData       TagResourceType
instance ToByteString TagResourceType
instance ToQuery      TagResourceType
instance ToHeader     TagResourceType

instance FromXML TagResourceType where
    parseXML = parseXMLText "TagResourceType"

instance ToXML TagResourceType where
    toXML = toXMLText

data VPCRegion
    = ApNortheast1
    | ApNortheast2
    | ApSoutheast1
    | ApSoutheast2
    | CnNorth1
    | EuCentral1
    | EuWest1
    | SaEast1
    | UsEast1
    | UsWest1
    | UsWest2
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText VPCRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure ApNortheast1
        "ap-northeast-2" -> pure ApNortheast2
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
           <> "'. Accepted values: ap-northeast-1, ap-northeast-2, ap-southeast-1, ap-southeast-2, cn-north-1, eu-central-1, eu-west-1, sa-east-1, us-east-1, us-west-1, us-west-2"

instance ToText VPCRegion where
    toText = \case
        ApNortheast1 -> "ap-northeast-1"
        ApNortheast2 -> "ap-northeast-2"
        ApSoutheast1 -> "ap-southeast-1"
        ApSoutheast2 -> "ap-southeast-2"
        CnNorth1 -> "cn-north-1"
        EuCentral1 -> "eu-central-1"
        EuWest1 -> "eu-west-1"
        SaEast1 -> "sa-east-1"
        UsEast1 -> "us-east-1"
        UsWest1 -> "us-west-1"
        UsWest2 -> "us-west-2"

instance Hashable     VPCRegion
instance NFData       VPCRegion
instance ToByteString VPCRegion
instance ToQuery      VPCRegion
instance ToHeader     VPCRegion

instance FromXML VPCRegion where
    parseXML = parseXMLText "VPCRegion"

instance ToXML VPCRegion where
    toXML = toXMLText
