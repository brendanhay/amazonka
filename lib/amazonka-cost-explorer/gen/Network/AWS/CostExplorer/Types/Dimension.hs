{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Dimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Dimension
  ( Dimension
    ( Dimension'
    , DimensionDimensionAZ
    , DimensionDimensionInstanceType
    , DimensionDimensionLinkedAccount
    , DimensionDimensionLinkedAccountName
    , DimensionDimensionOperation
    , DimensionDimensionPurchaseType
    , DimensionDimensionRegion
    , DimensionDimensionService
    , DimensionDimensionServiceCode
    , DimensionDimensionUsageType
    , DimensionDimensionUsageTypeGroup
    , DimensionDimensionRecordType
    , DimensionDimensionOperatingSystem
    , DimensionDimensionTenancy
    , DimensionDimensionScope
    , DimensionDimensionPlatform
    , DimensionDimensionSubscriptionId
    , DimensionDimensionLegalEntityName
    , DimensionDimensionDeploymentOption
    , DimensionDimensionDatabaseEngine
    , DimensionDimensionCacheEngine
    , DimensionDimensionInstanceTypeFamily
    , DimensionDimensionBillingEntity
    , DimensionDimensionReservationId
    , DimensionDimensionResourceId
    , DimensionDimensionRightsizingType
    , DimensionDimensionSavingsPlansType
    , DimensionDimensionSavingsPlanArn
    , DimensionDimensionPaymentOption
    , fromDimension
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Dimension = Dimension'{fromDimension :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern DimensionDimensionAZ :: Dimension
pattern DimensionDimensionAZ = Dimension' "AZ"

pattern DimensionDimensionInstanceType :: Dimension
pattern DimensionDimensionInstanceType = Dimension' "INSTANCE_TYPE"

pattern DimensionDimensionLinkedAccount :: Dimension
pattern DimensionDimensionLinkedAccount = Dimension' "LINKED_ACCOUNT"

pattern DimensionDimensionLinkedAccountName :: Dimension
pattern DimensionDimensionLinkedAccountName = Dimension' "LINKED_ACCOUNT_NAME"

pattern DimensionDimensionOperation :: Dimension
pattern DimensionDimensionOperation = Dimension' "OPERATION"

pattern DimensionDimensionPurchaseType :: Dimension
pattern DimensionDimensionPurchaseType = Dimension' "PURCHASE_TYPE"

pattern DimensionDimensionRegion :: Dimension
pattern DimensionDimensionRegion = Dimension' "REGION"

pattern DimensionDimensionService :: Dimension
pattern DimensionDimensionService = Dimension' "SERVICE"

pattern DimensionDimensionServiceCode :: Dimension
pattern DimensionDimensionServiceCode = Dimension' "SERVICE_CODE"

pattern DimensionDimensionUsageType :: Dimension
pattern DimensionDimensionUsageType = Dimension' "USAGE_TYPE"

pattern DimensionDimensionUsageTypeGroup :: Dimension
pattern DimensionDimensionUsageTypeGroup = Dimension' "USAGE_TYPE_GROUP"

pattern DimensionDimensionRecordType :: Dimension
pattern DimensionDimensionRecordType = Dimension' "RECORD_TYPE"

pattern DimensionDimensionOperatingSystem :: Dimension
pattern DimensionDimensionOperatingSystem = Dimension' "OPERATING_SYSTEM"

pattern DimensionDimensionTenancy :: Dimension
pattern DimensionDimensionTenancy = Dimension' "TENANCY"

pattern DimensionDimensionScope :: Dimension
pattern DimensionDimensionScope = Dimension' "SCOPE"

pattern DimensionDimensionPlatform :: Dimension
pattern DimensionDimensionPlatform = Dimension' "PLATFORM"

pattern DimensionDimensionSubscriptionId :: Dimension
pattern DimensionDimensionSubscriptionId = Dimension' "SUBSCRIPTION_ID"

pattern DimensionDimensionLegalEntityName :: Dimension
pattern DimensionDimensionLegalEntityName = Dimension' "LEGAL_ENTITY_NAME"

pattern DimensionDimensionDeploymentOption :: Dimension
pattern DimensionDimensionDeploymentOption = Dimension' "DEPLOYMENT_OPTION"

pattern DimensionDimensionDatabaseEngine :: Dimension
pattern DimensionDimensionDatabaseEngine = Dimension' "DATABASE_ENGINE"

pattern DimensionDimensionCacheEngine :: Dimension
pattern DimensionDimensionCacheEngine = Dimension' "CACHE_ENGINE"

pattern DimensionDimensionInstanceTypeFamily :: Dimension
pattern DimensionDimensionInstanceTypeFamily = Dimension' "INSTANCE_TYPE_FAMILY"

pattern DimensionDimensionBillingEntity :: Dimension
pattern DimensionDimensionBillingEntity = Dimension' "BILLING_ENTITY"

pattern DimensionDimensionReservationId :: Dimension
pattern DimensionDimensionReservationId = Dimension' "RESERVATION_ID"

pattern DimensionDimensionResourceId :: Dimension
pattern DimensionDimensionResourceId = Dimension' "RESOURCE_ID"

pattern DimensionDimensionRightsizingType :: Dimension
pattern DimensionDimensionRightsizingType = Dimension' "RIGHTSIZING_TYPE"

pattern DimensionDimensionSavingsPlansType :: Dimension
pattern DimensionDimensionSavingsPlansType = Dimension' "SAVINGS_PLANS_TYPE"

pattern DimensionDimensionSavingsPlanArn :: Dimension
pattern DimensionDimensionSavingsPlanArn = Dimension' "SAVINGS_PLAN_ARN"

pattern DimensionDimensionPaymentOption :: Dimension
pattern DimensionDimensionPaymentOption = Dimension' "PAYMENT_OPTION"

{-# COMPLETE 
  DimensionDimensionAZ,

  DimensionDimensionInstanceType,

  DimensionDimensionLinkedAccount,

  DimensionDimensionLinkedAccountName,

  DimensionDimensionOperation,

  DimensionDimensionPurchaseType,

  DimensionDimensionRegion,

  DimensionDimensionService,

  DimensionDimensionServiceCode,

  DimensionDimensionUsageType,

  DimensionDimensionUsageTypeGroup,

  DimensionDimensionRecordType,

  DimensionDimensionOperatingSystem,

  DimensionDimensionTenancy,

  DimensionDimensionScope,

  DimensionDimensionPlatform,

  DimensionDimensionSubscriptionId,

  DimensionDimensionLegalEntityName,

  DimensionDimensionDeploymentOption,

  DimensionDimensionDatabaseEngine,

  DimensionDimensionCacheEngine,

  DimensionDimensionInstanceTypeFamily,

  DimensionDimensionBillingEntity,

  DimensionDimensionReservationId,

  DimensionDimensionResourceId,

  DimensionDimensionRightsizingType,

  DimensionDimensionSavingsPlansType,

  DimensionDimensionSavingsPlanArn,

  DimensionDimensionPaymentOption,
  Dimension'
  #-}
