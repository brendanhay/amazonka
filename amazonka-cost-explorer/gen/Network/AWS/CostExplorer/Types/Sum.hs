{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Sum where

import Network.AWS.Prelude

data AccountScope
  = Linked
  | Payer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountScope where
    parser = takeLowerText >>= \case
        "linked" -> pure Linked
        "payer" -> pure Payer
        e -> fromTextError $ "Failure parsing AccountScope from value: '" <> e
           <> "'. Accepted values: linked, payer"

instance ToText AccountScope where
    toText = \case
        Linked -> "LINKED"
        Payer -> "PAYER"

instance Hashable     AccountScope
instance NFData       AccountScope
instance ToByteString AccountScope
instance ToQuery      AccountScope
instance ToHeader     AccountScope

instance ToJSON AccountScope where
    toJSON = toJSONText

instance FromJSON AccountScope where
    parseJSON = parseJSONText "AccountScope"

data Context
  = CostAndUsage
  | Reservations
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Context where
    parser = takeLowerText >>= \case
        "cost_and_usage" -> pure CostAndUsage
        "reservations" -> pure Reservations
        e -> fromTextError $ "Failure parsing Context from value: '" <> e
           <> "'. Accepted values: cost_and_usage, reservations"

instance ToText Context where
    toText = \case
        CostAndUsage -> "COST_AND_USAGE"
        Reservations -> "RESERVATIONS"

instance Hashable     Context
instance NFData       Context
instance ToByteString Context
instance ToQuery      Context
instance ToHeader     Context

instance ToJSON Context where
    toJSON = toJSONText

data Dimension
  = DimensionAZ
  | DimensionBillingEntity
  | DimensionCacheEngine
  | DimensionDatabaseEngine
  | DimensionDeploymentOption
  | DimensionInstanceType
  | DimensionInstanceTypeFamily
  | DimensionLegalEntityName
  | DimensionLinkedAccount
  | DimensionOperatingSystem
  | DimensionOperation
  | DimensionPlatform
  | DimensionPurchaseType
  | DimensionRecordType
  | DimensionRegion
  | DimensionReservationId
  | DimensionScope
  | DimensionService
  | DimensionSubscriptionId
  | DimensionTenancy
  | DimensionUsageType
  | DimensionUsageTypeGroup
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Dimension where
    parser = takeLowerText >>= \case
        "az" -> pure DimensionAZ
        "billing_entity" -> pure DimensionBillingEntity
        "cache_engine" -> pure DimensionCacheEngine
        "database_engine" -> pure DimensionDatabaseEngine
        "deployment_option" -> pure DimensionDeploymentOption
        "instance_type" -> pure DimensionInstanceType
        "instance_type_family" -> pure DimensionInstanceTypeFamily
        "legal_entity_name" -> pure DimensionLegalEntityName
        "linked_account" -> pure DimensionLinkedAccount
        "operating_system" -> pure DimensionOperatingSystem
        "operation" -> pure DimensionOperation
        "platform" -> pure DimensionPlatform
        "purchase_type" -> pure DimensionPurchaseType
        "record_type" -> pure DimensionRecordType
        "region" -> pure DimensionRegion
        "reservation_id" -> pure DimensionReservationId
        "scope" -> pure DimensionScope
        "service" -> pure DimensionService
        "subscription_id" -> pure DimensionSubscriptionId
        "tenancy" -> pure DimensionTenancy
        "usage_type" -> pure DimensionUsageType
        "usage_type_group" -> pure DimensionUsageTypeGroup
        e -> fromTextError $ "Failure parsing Dimension from value: '" <> e
           <> "'. Accepted values: az, billing_entity, cache_engine, database_engine, deployment_option, instance_type, instance_type_family, legal_entity_name, linked_account, operating_system, operation, platform, purchase_type, record_type, region, reservation_id, scope, service, subscription_id, tenancy, usage_type, usage_type_group"

instance ToText Dimension where
    toText = \case
        DimensionAZ -> "AZ"
        DimensionBillingEntity -> "BILLING_ENTITY"
        DimensionCacheEngine -> "CACHE_ENGINE"
        DimensionDatabaseEngine -> "DATABASE_ENGINE"
        DimensionDeploymentOption -> "DEPLOYMENT_OPTION"
        DimensionInstanceType -> "INSTANCE_TYPE"
        DimensionInstanceTypeFamily -> "INSTANCE_TYPE_FAMILY"
        DimensionLegalEntityName -> "LEGAL_ENTITY_NAME"
        DimensionLinkedAccount -> "LINKED_ACCOUNT"
        DimensionOperatingSystem -> "OPERATING_SYSTEM"
        DimensionOperation -> "OPERATION"
        DimensionPlatform -> "PLATFORM"
        DimensionPurchaseType -> "PURCHASE_TYPE"
        DimensionRecordType -> "RECORD_TYPE"
        DimensionRegion -> "REGION"
        DimensionReservationId -> "RESERVATION_ID"
        DimensionScope -> "SCOPE"
        DimensionService -> "SERVICE"
        DimensionSubscriptionId -> "SUBSCRIPTION_ID"
        DimensionTenancy -> "TENANCY"
        DimensionUsageType -> "USAGE_TYPE"
        DimensionUsageTypeGroup -> "USAGE_TYPE_GROUP"

instance Hashable     Dimension
instance NFData       Dimension
instance ToByteString Dimension
instance ToQuery      Dimension
instance ToHeader     Dimension

instance ToJSON Dimension where
    toJSON = toJSONText

data Granularity
  = Daily
  | Hourly
  | Monthly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Granularity where
    parser = takeLowerText >>= \case
        "daily" -> pure Daily
        "hourly" -> pure Hourly
        "monthly" -> pure Monthly
        e -> fromTextError $ "Failure parsing Granularity from value: '" <> e
           <> "'. Accepted values: daily, hourly, monthly"

instance ToText Granularity where
    toText = \case
        Daily -> "DAILY"
        Hourly -> "HOURLY"
        Monthly -> "MONTHLY"

instance Hashable     Granularity
instance NFData       Granularity
instance ToByteString Granularity
instance ToQuery      Granularity
instance ToHeader     Granularity

instance ToJSON Granularity where
    toJSON = toJSONText

data GroupDefinitionType
  = Dimension
  | Tag
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GroupDefinitionType where
    parser = takeLowerText >>= \case
        "dimension" -> pure Dimension
        "tag" -> pure Tag
        e -> fromTextError $ "Failure parsing GroupDefinitionType from value: '" <> e
           <> "'. Accepted values: dimension, tag"

instance ToText GroupDefinitionType where
    toText = \case
        Dimension -> "DIMENSION"
        Tag -> "TAG"

instance Hashable     GroupDefinitionType
instance NFData       GroupDefinitionType
instance ToByteString GroupDefinitionType
instance ToQuery      GroupDefinitionType
instance ToHeader     GroupDefinitionType

instance ToJSON GroupDefinitionType where
    toJSON = toJSONText

instance FromJSON GroupDefinitionType where
    parseJSON = parseJSONText "GroupDefinitionType"

data LookbackPeriodInDays
  = SevenDays
  | SixtyDays
  | ThirtyDays
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LookbackPeriodInDays where
    parser = takeLowerText >>= \case
        "seven_days" -> pure SevenDays
        "sixty_days" -> pure SixtyDays
        "thirty_days" -> pure ThirtyDays
        e -> fromTextError $ "Failure parsing LookbackPeriodInDays from value: '" <> e
           <> "'. Accepted values: seven_days, sixty_days, thirty_days"

instance ToText LookbackPeriodInDays where
    toText = \case
        SevenDays -> "SEVEN_DAYS"
        SixtyDays -> "SIXTY_DAYS"
        ThirtyDays -> "THIRTY_DAYS"

instance Hashable     LookbackPeriodInDays
instance NFData       LookbackPeriodInDays
instance ToByteString LookbackPeriodInDays
instance ToQuery      LookbackPeriodInDays
instance ToHeader     LookbackPeriodInDays

instance ToJSON LookbackPeriodInDays where
    toJSON = toJSONText

instance FromJSON LookbackPeriodInDays where
    parseJSON = parseJSONText "LookbackPeriodInDays"

data Metric
  = AmortizedCost
  | BlendedCost
  | NetAmortizedCost
  | NetUnblendedCost
  | NormalizedUsageAmount
  | UnblendedCost
  | UsageQuantity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Metric where
    parser = takeLowerText >>= \case
        "amortized_cost" -> pure AmortizedCost
        "blended_cost" -> pure BlendedCost
        "net_amortized_cost" -> pure NetAmortizedCost
        "net_unblended_cost" -> pure NetUnblendedCost
        "normalized_usage_amount" -> pure NormalizedUsageAmount
        "unblended_cost" -> pure UnblendedCost
        "usage_quantity" -> pure UsageQuantity
        e -> fromTextError $ "Failure parsing Metric from value: '" <> e
           <> "'. Accepted values: amortized_cost, blended_cost, net_amortized_cost, net_unblended_cost, normalized_usage_amount, unblended_cost, usage_quantity"

instance ToText Metric where
    toText = \case
        AmortizedCost -> "AMORTIZED_COST"
        BlendedCost -> "BLENDED_COST"
        NetAmortizedCost -> "NET_AMORTIZED_COST"
        NetUnblendedCost -> "NET_UNBLENDED_COST"
        NormalizedUsageAmount -> "NORMALIZED_USAGE_AMOUNT"
        UnblendedCost -> "UNBLENDED_COST"
        UsageQuantity -> "USAGE_QUANTITY"

instance Hashable     Metric
instance NFData       Metric
instance ToByteString Metric
instance ToQuery      Metric
instance ToHeader     Metric

instance ToJSON Metric where
    toJSON = toJSONText

data OfferingClass
  = Convertible
  | Standard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OfferingClass where
    parser = takeLowerText >>= \case
        "convertible" -> pure Convertible
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing OfferingClass from value: '" <> e
           <> "'. Accepted values: convertible, standard"

instance ToText OfferingClass where
    toText = \case
        Convertible -> "CONVERTIBLE"
        Standard -> "STANDARD"

instance Hashable     OfferingClass
instance NFData       OfferingClass
instance ToByteString OfferingClass
instance ToQuery      OfferingClass
instance ToHeader     OfferingClass

instance ToJSON OfferingClass where
    toJSON = toJSONText

instance FromJSON OfferingClass where
    parseJSON = parseJSONText "OfferingClass"

data PaymentOption
  = AllUpfront
  | HeavyUtilization
  | LightUtilization
  | MediumUtilization
  | NoUpfront
  | PartialUpfront
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PaymentOption where
    parser = takeLowerText >>= \case
        "all_upfront" -> pure AllUpfront
        "heavy_utilization" -> pure HeavyUtilization
        "light_utilization" -> pure LightUtilization
        "medium_utilization" -> pure MediumUtilization
        "no_upfront" -> pure NoUpfront
        "partial_upfront" -> pure PartialUpfront
        e -> fromTextError $ "Failure parsing PaymentOption from value: '" <> e
           <> "'. Accepted values: all_upfront, heavy_utilization, light_utilization, medium_utilization, no_upfront, partial_upfront"

instance ToText PaymentOption where
    toText = \case
        AllUpfront -> "ALL_UPFRONT"
        HeavyUtilization -> "HEAVY_UTILIZATION"
        LightUtilization -> "LIGHT_UTILIZATION"
        MediumUtilization -> "MEDIUM_UTILIZATION"
        NoUpfront -> "NO_UPFRONT"
        PartialUpfront -> "PARTIAL_UPFRONT"

instance Hashable     PaymentOption
instance NFData       PaymentOption
instance ToByteString PaymentOption
instance ToQuery      PaymentOption
instance ToHeader     PaymentOption

instance ToJSON PaymentOption where
    toJSON = toJSONText

instance FromJSON PaymentOption where
    parseJSON = parseJSONText "PaymentOption"

data TermInYears
  = OneYear
  | ThreeYears
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TermInYears where
    parser = takeLowerText >>= \case
        "one_year" -> pure OneYear
        "three_years" -> pure ThreeYears
        e -> fromTextError $ "Failure parsing TermInYears from value: '" <> e
           <> "'. Accepted values: one_year, three_years"

instance ToText TermInYears where
    toText = \case
        OneYear -> "ONE_YEAR"
        ThreeYears -> "THREE_YEARS"

instance Hashable     TermInYears
instance NFData       TermInYears
instance ToByteString TermInYears
instance ToQuery      TermInYears
instance ToHeader     TermInYears

instance ToJSON TermInYears where
    toJSON = toJSONText

instance FromJSON TermInYears where
    parseJSON = parseJSONText "TermInYears"
