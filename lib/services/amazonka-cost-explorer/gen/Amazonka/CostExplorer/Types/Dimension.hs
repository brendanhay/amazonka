{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostExplorer.Types.Dimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Dimension
  ( Dimension
      ( ..,
        Dimension_AGREEMENT_END_DATE_TIME_AFTER,
        Dimension_AGREEMENT_END_DATE_TIME_BEFORE,
        Dimension_AZ,
        Dimension_BILLING_ENTITY,
        Dimension_CACHE_ENGINE,
        Dimension_DATABASE_ENGINE,
        Dimension_DEPLOYMENT_OPTION,
        Dimension_INSTANCE_TYPE,
        Dimension_INSTANCE_TYPE_FAMILY,
        Dimension_INVOICING_ENTITY,
        Dimension_LEGAL_ENTITY_NAME,
        Dimension_LINKED_ACCOUNT,
        Dimension_LINKED_ACCOUNT_NAME,
        Dimension_OPERATING_SYSTEM,
        Dimension_OPERATION,
        Dimension_PAYMENT_OPTION,
        Dimension_PLATFORM,
        Dimension_PURCHASE_TYPE,
        Dimension_RECORD_TYPE,
        Dimension_REGION,
        Dimension_RESERVATION_ID,
        Dimension_RESOURCE_ID,
        Dimension_RIGHTSIZING_TYPE,
        Dimension_SAVINGS_PLANS_TYPE,
        Dimension_SAVINGS_PLAN_ARN,
        Dimension_SCOPE,
        Dimension_SERVICE,
        Dimension_SERVICE_CODE,
        Dimension_SUBSCRIPTION_ID,
        Dimension_TENANCY,
        Dimension_USAGE_TYPE,
        Dimension_USAGE_TYPE_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Dimension = Dimension'
  { fromDimension ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern Dimension_AGREEMENT_END_DATE_TIME_AFTER :: Dimension
pattern Dimension_AGREEMENT_END_DATE_TIME_AFTER = Dimension' "AGREEMENT_END_DATE_TIME_AFTER"

pattern Dimension_AGREEMENT_END_DATE_TIME_BEFORE :: Dimension
pattern Dimension_AGREEMENT_END_DATE_TIME_BEFORE = Dimension' "AGREEMENT_END_DATE_TIME_BEFORE"

pattern Dimension_AZ :: Dimension
pattern Dimension_AZ = Dimension' "AZ"

pattern Dimension_BILLING_ENTITY :: Dimension
pattern Dimension_BILLING_ENTITY = Dimension' "BILLING_ENTITY"

pattern Dimension_CACHE_ENGINE :: Dimension
pattern Dimension_CACHE_ENGINE = Dimension' "CACHE_ENGINE"

pattern Dimension_DATABASE_ENGINE :: Dimension
pattern Dimension_DATABASE_ENGINE = Dimension' "DATABASE_ENGINE"

pattern Dimension_DEPLOYMENT_OPTION :: Dimension
pattern Dimension_DEPLOYMENT_OPTION = Dimension' "DEPLOYMENT_OPTION"

pattern Dimension_INSTANCE_TYPE :: Dimension
pattern Dimension_INSTANCE_TYPE = Dimension' "INSTANCE_TYPE"

pattern Dimension_INSTANCE_TYPE_FAMILY :: Dimension
pattern Dimension_INSTANCE_TYPE_FAMILY = Dimension' "INSTANCE_TYPE_FAMILY"

pattern Dimension_INVOICING_ENTITY :: Dimension
pattern Dimension_INVOICING_ENTITY = Dimension' "INVOICING_ENTITY"

pattern Dimension_LEGAL_ENTITY_NAME :: Dimension
pattern Dimension_LEGAL_ENTITY_NAME = Dimension' "LEGAL_ENTITY_NAME"

pattern Dimension_LINKED_ACCOUNT :: Dimension
pattern Dimension_LINKED_ACCOUNT = Dimension' "LINKED_ACCOUNT"

pattern Dimension_LINKED_ACCOUNT_NAME :: Dimension
pattern Dimension_LINKED_ACCOUNT_NAME = Dimension' "LINKED_ACCOUNT_NAME"

pattern Dimension_OPERATING_SYSTEM :: Dimension
pattern Dimension_OPERATING_SYSTEM = Dimension' "OPERATING_SYSTEM"

pattern Dimension_OPERATION :: Dimension
pattern Dimension_OPERATION = Dimension' "OPERATION"

pattern Dimension_PAYMENT_OPTION :: Dimension
pattern Dimension_PAYMENT_OPTION = Dimension' "PAYMENT_OPTION"

pattern Dimension_PLATFORM :: Dimension
pattern Dimension_PLATFORM = Dimension' "PLATFORM"

pattern Dimension_PURCHASE_TYPE :: Dimension
pattern Dimension_PURCHASE_TYPE = Dimension' "PURCHASE_TYPE"

pattern Dimension_RECORD_TYPE :: Dimension
pattern Dimension_RECORD_TYPE = Dimension' "RECORD_TYPE"

pattern Dimension_REGION :: Dimension
pattern Dimension_REGION = Dimension' "REGION"

pattern Dimension_RESERVATION_ID :: Dimension
pattern Dimension_RESERVATION_ID = Dimension' "RESERVATION_ID"

pattern Dimension_RESOURCE_ID :: Dimension
pattern Dimension_RESOURCE_ID = Dimension' "RESOURCE_ID"

pattern Dimension_RIGHTSIZING_TYPE :: Dimension
pattern Dimension_RIGHTSIZING_TYPE = Dimension' "RIGHTSIZING_TYPE"

pattern Dimension_SAVINGS_PLANS_TYPE :: Dimension
pattern Dimension_SAVINGS_PLANS_TYPE = Dimension' "SAVINGS_PLANS_TYPE"

pattern Dimension_SAVINGS_PLAN_ARN :: Dimension
pattern Dimension_SAVINGS_PLAN_ARN = Dimension' "SAVINGS_PLAN_ARN"

pattern Dimension_SCOPE :: Dimension
pattern Dimension_SCOPE = Dimension' "SCOPE"

pattern Dimension_SERVICE :: Dimension
pattern Dimension_SERVICE = Dimension' "SERVICE"

pattern Dimension_SERVICE_CODE :: Dimension
pattern Dimension_SERVICE_CODE = Dimension' "SERVICE_CODE"

pattern Dimension_SUBSCRIPTION_ID :: Dimension
pattern Dimension_SUBSCRIPTION_ID = Dimension' "SUBSCRIPTION_ID"

pattern Dimension_TENANCY :: Dimension
pattern Dimension_TENANCY = Dimension' "TENANCY"

pattern Dimension_USAGE_TYPE :: Dimension
pattern Dimension_USAGE_TYPE = Dimension' "USAGE_TYPE"

pattern Dimension_USAGE_TYPE_GROUP :: Dimension
pattern Dimension_USAGE_TYPE_GROUP = Dimension' "USAGE_TYPE_GROUP"

{-# COMPLETE
  Dimension_AGREEMENT_END_DATE_TIME_AFTER,
  Dimension_AGREEMENT_END_DATE_TIME_BEFORE,
  Dimension_AZ,
  Dimension_BILLING_ENTITY,
  Dimension_CACHE_ENGINE,
  Dimension_DATABASE_ENGINE,
  Dimension_DEPLOYMENT_OPTION,
  Dimension_INSTANCE_TYPE,
  Dimension_INSTANCE_TYPE_FAMILY,
  Dimension_INVOICING_ENTITY,
  Dimension_LEGAL_ENTITY_NAME,
  Dimension_LINKED_ACCOUNT,
  Dimension_LINKED_ACCOUNT_NAME,
  Dimension_OPERATING_SYSTEM,
  Dimension_OPERATION,
  Dimension_PAYMENT_OPTION,
  Dimension_PLATFORM,
  Dimension_PURCHASE_TYPE,
  Dimension_RECORD_TYPE,
  Dimension_REGION,
  Dimension_RESERVATION_ID,
  Dimension_RESOURCE_ID,
  Dimension_RIGHTSIZING_TYPE,
  Dimension_SAVINGS_PLANS_TYPE,
  Dimension_SAVINGS_PLAN_ARN,
  Dimension_SCOPE,
  Dimension_SERVICE,
  Dimension_SERVICE_CODE,
  Dimension_SUBSCRIPTION_ID,
  Dimension_TENANCY,
  Dimension_USAGE_TYPE,
  Dimension_USAGE_TYPE_GROUP,
  Dimension'
  #-}
