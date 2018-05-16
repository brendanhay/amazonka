{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.Sum where

import Network.AWS.Prelude

data CustomHealthStatus
  = CHSHealthy
  | CHSUnhealthy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CustomHealthStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure CHSHealthy
        "unhealthy" -> pure CHSUnhealthy
        e -> fromTextError $ "Failure parsing CustomHealthStatus from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy"

instance ToText CustomHealthStatus where
    toText = \case
        CHSHealthy -> "HEALTHY"
        CHSUnhealthy -> "UNHEALTHY"

instance Hashable     CustomHealthStatus
instance NFData       CustomHealthStatus
instance ToByteString CustomHealthStatus
instance ToQuery      CustomHealthStatus
instance ToHeader     CustomHealthStatus

instance ToJSON CustomHealthStatus where
    toJSON = toJSONText

data FilterCondition
  = Between
  | EQ'
  | IN
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FilterCondition where
    parser = takeLowerText >>= \case
        "between" -> pure Between
        "eq" -> pure EQ'
        "in" -> pure IN
        e -> fromTextError $ "Failure parsing FilterCondition from value: '" <> e
           <> "'. Accepted values: between, eq, in"

instance ToText FilterCondition where
    toText = \case
        Between -> "BETWEEN"
        EQ' -> "EQ"
        IN -> "IN"

instance Hashable     FilterCondition
instance NFData       FilterCondition
instance ToByteString FilterCondition
instance ToQuery      FilterCondition
instance ToHeader     FilterCondition

instance ToJSON FilterCondition where
    toJSON = toJSONText

data HealthCheckType
  = HTTP
  | HTTPS
  | TCP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HealthCheckType where
    parser = takeLowerText >>= \case
        "http" -> pure HTTP
        "https" -> pure HTTPS
        "tcp" -> pure TCP
        e -> fromTextError $ "Failure parsing HealthCheckType from value: '" <> e
           <> "'. Accepted values: http, https, tcp"

instance ToText HealthCheckType where
    toText = \case
        HTTP -> "HTTP"
        HTTPS -> "HTTPS"
        TCP -> "TCP"

instance Hashable     HealthCheckType
instance NFData       HealthCheckType
instance ToByteString HealthCheckType
instance ToQuery      HealthCheckType
instance ToHeader     HealthCheckType

instance ToJSON HealthCheckType where
    toJSON = toJSONText

instance FromJSON HealthCheckType where
    parseJSON = parseJSONText "HealthCheckType"

data HealthStatus
  = Healthy
  | Unhealthy
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HealthStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure Healthy
        "unhealthy" -> pure Unhealthy
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing HealthStatus from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy, unknown"

instance ToText HealthStatus where
    toText = \case
        Healthy -> "HEALTHY"
        Unhealthy -> "UNHEALTHY"
        Unknown -> "UNKNOWN"

instance Hashable     HealthStatus
instance NFData       HealthStatus
instance ToByteString HealthStatus
instance ToQuery      HealthStatus
instance ToHeader     HealthStatus

instance FromJSON HealthStatus where
    parseJSON = parseJSONText "HealthStatus"

data NamespaceFilterName =
  Type
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NamespaceFilterName where
    parser = takeLowerText >>= \case
        "type" -> pure Type
        e -> fromTextError $ "Failure parsing NamespaceFilterName from value: '" <> e
           <> "'. Accepted values: type"

instance ToText NamespaceFilterName where
    toText = \case
        Type -> "TYPE"

instance Hashable     NamespaceFilterName
instance NFData       NamespaceFilterName
instance ToByteString NamespaceFilterName
instance ToQuery      NamespaceFilterName
instance ToHeader     NamespaceFilterName

instance ToJSON NamespaceFilterName where
    toJSON = toJSONText

data NamespaceType
  = DNSPrivate
  | DNSPublic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NamespaceType where
    parser = takeLowerText >>= \case
        "dns_private" -> pure DNSPrivate
        "dns_public" -> pure DNSPublic
        e -> fromTextError $ "Failure parsing NamespaceType from value: '" <> e
           <> "'. Accepted values: dns_private, dns_public"

instance ToText NamespaceType where
    toText = \case
        DNSPrivate -> "DNS_PRIVATE"
        DNSPublic -> "DNS_PUBLIC"

instance Hashable     NamespaceType
instance NFData       NamespaceType
instance ToByteString NamespaceType
instance ToQuery      NamespaceType
instance ToHeader     NamespaceType

instance FromJSON NamespaceType where
    parseJSON = parseJSONText "NamespaceType"

data OperationFilterName
  = OFNNamespaceId
  | OFNServiceId
  | OFNStatus
  | OFNType
  | OFNUpdateDate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationFilterName where
    parser = takeLowerText >>= \case
        "namespace_id" -> pure OFNNamespaceId
        "service_id" -> pure OFNServiceId
        "status" -> pure OFNStatus
        "type" -> pure OFNType
        "update_date" -> pure OFNUpdateDate
        e -> fromTextError $ "Failure parsing OperationFilterName from value: '" <> e
           <> "'. Accepted values: namespace_id, service_id, status, type, update_date"

instance ToText OperationFilterName where
    toText = \case
        OFNNamespaceId -> "NAMESPACE_ID"
        OFNServiceId -> "SERVICE_ID"
        OFNStatus -> "STATUS"
        OFNType -> "TYPE"
        OFNUpdateDate -> "UPDATE_DATE"

instance Hashable     OperationFilterName
instance NFData       OperationFilterName
instance ToByteString OperationFilterName
instance ToQuery      OperationFilterName
instance ToHeader     OperationFilterName

instance ToJSON OperationFilterName where
    toJSON = toJSONText

data OperationStatus
  = Fail
  | Pending
  | Submitted
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "fail" -> pure Fail
        "pending" -> pure Pending
        "submitted" -> pure Submitted
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing OperationStatus from value: '" <> e
           <> "'. Accepted values: fail, pending, submitted, success"

instance ToText OperationStatus where
    toText = \case
        Fail -> "FAIL"
        Pending -> "PENDING"
        Submitted -> "SUBMITTED"
        Success -> "SUCCESS"

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

data OperationTargetType
  = OTTInstance
  | OTTNamespace
  | OTTService
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationTargetType where
    parser = takeLowerText >>= \case
        "instance" -> pure OTTInstance
        "namespace" -> pure OTTNamespace
        "service" -> pure OTTService
        e -> fromTextError $ "Failure parsing OperationTargetType from value: '" <> e
           <> "'. Accepted values: instance, namespace, service"

instance ToText OperationTargetType where
    toText = \case
        OTTInstance -> "INSTANCE"
        OTTNamespace -> "NAMESPACE"
        OTTService -> "SERVICE"

instance Hashable     OperationTargetType
instance NFData       OperationTargetType
instance ToByteString OperationTargetType
instance ToQuery      OperationTargetType
instance ToHeader     OperationTargetType

instance FromJSON OperationTargetType where
    parseJSON = parseJSONText "OperationTargetType"

data OperationType
  = CreateNamespace
  | DeleteNamespace
  | DeregisterInstance
  | RegisterInstance
  | UpdateService
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationType where
    parser = takeLowerText >>= \case
        "create_namespace" -> pure CreateNamespace
        "delete_namespace" -> pure DeleteNamespace
        "deregister_instance" -> pure DeregisterInstance
        "register_instance" -> pure RegisterInstance
        "update_service" -> pure UpdateService
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: create_namespace, delete_namespace, deregister_instance, register_instance, update_service"

instance ToText OperationType where
    toText = \case
        CreateNamespace -> "CREATE_NAMESPACE"
        DeleteNamespace -> "DELETE_NAMESPACE"
        DeregisterInstance -> "DEREGISTER_INSTANCE"
        RegisterInstance -> "REGISTER_INSTANCE"
        UpdateService -> "UPDATE_SERVICE"

instance Hashable     OperationType
instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

data RecordType
  = A
  | Aaaa
  | Cname
  | Srv
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecordType where
    parser = takeLowerText >>= \case
        "a" -> pure A
        "aaaa" -> pure Aaaa
        "cname" -> pure Cname
        "srv" -> pure Srv
        e -> fromTextError $ "Failure parsing RecordType from value: '" <> e
           <> "'. Accepted values: a, aaaa, cname, srv"

instance ToText RecordType where
    toText = \case
        A -> "A"
        Aaaa -> "AAAA"
        Cname -> "CNAME"
        Srv -> "SRV"

instance Hashable     RecordType
instance NFData       RecordType
instance ToByteString RecordType
instance ToQuery      RecordType
instance ToHeader     RecordType

instance ToJSON RecordType where
    toJSON = toJSONText

instance FromJSON RecordType where
    parseJSON = parseJSONText "RecordType"

data RoutingPolicy
  = Multivalue
  | Weighted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RoutingPolicy where
    parser = takeLowerText >>= \case
        "multivalue" -> pure Multivalue
        "weighted" -> pure Weighted
        e -> fromTextError $ "Failure parsing RoutingPolicy from value: '" <> e
           <> "'. Accepted values: multivalue, weighted"

instance ToText RoutingPolicy where
    toText = \case
        Multivalue -> "MULTIVALUE"
        Weighted -> "WEIGHTED"

instance Hashable     RoutingPolicy
instance NFData       RoutingPolicy
instance ToByteString RoutingPolicy
instance ToQuery      RoutingPolicy
instance ToHeader     RoutingPolicy

instance ToJSON RoutingPolicy where
    toJSON = toJSONText

instance FromJSON RoutingPolicy where
    parseJSON = parseJSONText "RoutingPolicy"

data ServiceFilterName =
  NamespaceId
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceFilterName where
    parser = takeLowerText >>= \case
        "namespace_id" -> pure NamespaceId
        e -> fromTextError $ "Failure parsing ServiceFilterName from value: '" <> e
           <> "'. Accepted values: namespace_id"

instance ToText ServiceFilterName where
    toText = \case
        NamespaceId -> "NAMESPACE_ID"

instance Hashable     ServiceFilterName
instance NFData       ServiceFilterName
instance ToByteString ServiceFilterName
instance ToQuery      ServiceFilterName
instance ToHeader     ServiceFilterName

instance ToJSON ServiceFilterName where
    toJSON = toJSONText
