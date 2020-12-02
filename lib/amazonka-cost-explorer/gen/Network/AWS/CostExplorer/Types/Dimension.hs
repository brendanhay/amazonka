{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Dimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Dimension where

import Network.AWS.Prelude

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
  | DimensionLinkedAccountName
  | DimensionOperatingSystem
  | DimensionOperation
  | DimensionPaymentOption
  | DimensionPlatform
  | DimensionPurchaseType
  | DimensionRecordType
  | DimensionRegion
  | DimensionReservationId
  | DimensionResourceId
  | DimensionRightsizingType
  | DimensionSavingsPlanARN
  | DimensionSavingsPlansType
  | DimensionScope
  | DimensionService
  | DimensionServiceCode
  | DimensionSubscriptionId
  | DimensionTenancy
  | DimensionUsageType
  | DimensionUsageTypeGroup
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText Dimension where
  parser =
    takeLowerText >>= \case
      "az" -> pure DimensionAZ
      "billing_entity" -> pure DimensionBillingEntity
      "cache_engine" -> pure DimensionCacheEngine
      "database_engine" -> pure DimensionDatabaseEngine
      "deployment_option" -> pure DimensionDeploymentOption
      "instance_type" -> pure DimensionInstanceType
      "instance_type_family" -> pure DimensionInstanceTypeFamily
      "legal_entity_name" -> pure DimensionLegalEntityName
      "linked_account" -> pure DimensionLinkedAccount
      "linked_account_name" -> pure DimensionLinkedAccountName
      "operating_system" -> pure DimensionOperatingSystem
      "operation" -> pure DimensionOperation
      "payment_option" -> pure DimensionPaymentOption
      "platform" -> pure DimensionPlatform
      "purchase_type" -> pure DimensionPurchaseType
      "record_type" -> pure DimensionRecordType
      "region" -> pure DimensionRegion
      "reservation_id" -> pure DimensionReservationId
      "resource_id" -> pure DimensionResourceId
      "rightsizing_type" -> pure DimensionRightsizingType
      "savings_plan_arn" -> pure DimensionSavingsPlanARN
      "savings_plans_type" -> pure DimensionSavingsPlansType
      "scope" -> pure DimensionScope
      "service" -> pure DimensionService
      "service_code" -> pure DimensionServiceCode
      "subscription_id" -> pure DimensionSubscriptionId
      "tenancy" -> pure DimensionTenancy
      "usage_type" -> pure DimensionUsageType
      "usage_type_group" -> pure DimensionUsageTypeGroup
      e ->
        fromTextError $
          "Failure parsing Dimension from value: '" <> e
            <> "'. Accepted values: az, billing_entity, cache_engine, database_engine, deployment_option, instance_type, instance_type_family, legal_entity_name, linked_account, linked_account_name, operating_system, operation, payment_option, platform, purchase_type, record_type, region, reservation_id, resource_id, rightsizing_type, savings_plan_arn, savings_plans_type, scope, service, service_code, subscription_id, tenancy, usage_type, usage_type_group"

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
    DimensionLinkedAccountName -> "LINKED_ACCOUNT_NAME"
    DimensionOperatingSystem -> "OPERATING_SYSTEM"
    DimensionOperation -> "OPERATION"
    DimensionPaymentOption -> "PAYMENT_OPTION"
    DimensionPlatform -> "PLATFORM"
    DimensionPurchaseType -> "PURCHASE_TYPE"
    DimensionRecordType -> "RECORD_TYPE"
    DimensionRegion -> "REGION"
    DimensionReservationId -> "RESERVATION_ID"
    DimensionResourceId -> "RESOURCE_ID"
    DimensionRightsizingType -> "RIGHTSIZING_TYPE"
    DimensionSavingsPlanARN -> "SAVINGS_PLAN_ARN"
    DimensionSavingsPlansType -> "SAVINGS_PLANS_TYPE"
    DimensionScope -> "SCOPE"
    DimensionService -> "SERVICE"
    DimensionServiceCode -> "SERVICE_CODE"
    DimensionSubscriptionId -> "SUBSCRIPTION_ID"
    DimensionTenancy -> "TENANCY"
    DimensionUsageType -> "USAGE_TYPE"
    DimensionUsageTypeGroup -> "USAGE_TYPE_GROUP"

instance Hashable Dimension

instance NFData Dimension

instance ToByteString Dimension

instance ToQuery Dimension

instance ToHeader Dimension

instance ToJSON Dimension where
  toJSON = toJSONText

instance FromJSON Dimension where
  parseJSON = parseJSONText "Dimension"
