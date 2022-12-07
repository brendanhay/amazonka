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
-- Module      : Amazonka.MarketplaceAnalytics.Types.DataSetType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceAnalytics.Types.DataSetType
  ( DataSetType
      ( ..,
        DataSetType_Customer_profile_by_geography,
        DataSetType_Customer_profile_by_industry,
        DataSetType_Customer_profile_by_revenue,
        DataSetType_Customer_subscriber_annual_subscriptions,
        DataSetType_Customer_subscriber_hourly_monthly_subscriptions,
        DataSetType_Daily_business_canceled_product_subscribers,
        DataSetType_Daily_business_fees,
        DataSetType_Daily_business_free_trial_conversions,
        DataSetType_Daily_business_new_instances,
        DataSetType_Daily_business_new_product_subscribers,
        DataSetType_Daily_business_usage_by_instance_type,
        DataSetType_Disbursed_amount_by_age_of_disbursed_funds,
        DataSetType_Disbursed_amount_by_age_of_past_due_funds,
        DataSetType_Disbursed_amount_by_age_of_uncollected_funds,
        DataSetType_Disbursed_amount_by_customer_geo,
        DataSetType_Disbursed_amount_by_instance_hours,
        DataSetType_Disbursed_amount_by_product,
        DataSetType_Disbursed_amount_by_product_with_uncollected_funds,
        DataSetType_Disbursed_amount_by_uncollected_funds_breakdown,
        DataSetType_Monthly_revenue_annual_subscriptions,
        DataSetType_Monthly_revenue_billing_and_revenue_data,
        DataSetType_Monthly_revenue_field_demonstration_usage,
        DataSetType_Monthly_revenue_flexible_payment_schedule,
        DataSetType_Sales_compensation_billed_revenue,
        DataSetType_Us_sales_and_use_tax_records
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSetType = DataSetType'
  { fromDataSetType ::
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

pattern DataSetType_Customer_profile_by_geography :: DataSetType
pattern DataSetType_Customer_profile_by_geography = DataSetType' "customer_profile_by_geography"

pattern DataSetType_Customer_profile_by_industry :: DataSetType
pattern DataSetType_Customer_profile_by_industry = DataSetType' "customer_profile_by_industry"

pattern DataSetType_Customer_profile_by_revenue :: DataSetType
pattern DataSetType_Customer_profile_by_revenue = DataSetType' "customer_profile_by_revenue"

pattern DataSetType_Customer_subscriber_annual_subscriptions :: DataSetType
pattern DataSetType_Customer_subscriber_annual_subscriptions = DataSetType' "customer_subscriber_annual_subscriptions"

pattern DataSetType_Customer_subscriber_hourly_monthly_subscriptions :: DataSetType
pattern DataSetType_Customer_subscriber_hourly_monthly_subscriptions = DataSetType' "customer_subscriber_hourly_monthly_subscriptions"

pattern DataSetType_Daily_business_canceled_product_subscribers :: DataSetType
pattern DataSetType_Daily_business_canceled_product_subscribers = DataSetType' "daily_business_canceled_product_subscribers"

pattern DataSetType_Daily_business_fees :: DataSetType
pattern DataSetType_Daily_business_fees = DataSetType' "daily_business_fees"

pattern DataSetType_Daily_business_free_trial_conversions :: DataSetType
pattern DataSetType_Daily_business_free_trial_conversions = DataSetType' "daily_business_free_trial_conversions"

pattern DataSetType_Daily_business_new_instances :: DataSetType
pattern DataSetType_Daily_business_new_instances = DataSetType' "daily_business_new_instances"

pattern DataSetType_Daily_business_new_product_subscribers :: DataSetType
pattern DataSetType_Daily_business_new_product_subscribers = DataSetType' "daily_business_new_product_subscribers"

pattern DataSetType_Daily_business_usage_by_instance_type :: DataSetType
pattern DataSetType_Daily_business_usage_by_instance_type = DataSetType' "daily_business_usage_by_instance_type"

pattern DataSetType_Disbursed_amount_by_age_of_disbursed_funds :: DataSetType
pattern DataSetType_Disbursed_amount_by_age_of_disbursed_funds = DataSetType' "disbursed_amount_by_age_of_disbursed_funds"

pattern DataSetType_Disbursed_amount_by_age_of_past_due_funds :: DataSetType
pattern DataSetType_Disbursed_amount_by_age_of_past_due_funds = DataSetType' "disbursed_amount_by_age_of_past_due_funds"

pattern DataSetType_Disbursed_amount_by_age_of_uncollected_funds :: DataSetType
pattern DataSetType_Disbursed_amount_by_age_of_uncollected_funds = DataSetType' "disbursed_amount_by_age_of_uncollected_funds"

pattern DataSetType_Disbursed_amount_by_customer_geo :: DataSetType
pattern DataSetType_Disbursed_amount_by_customer_geo = DataSetType' "disbursed_amount_by_customer_geo"

pattern DataSetType_Disbursed_amount_by_instance_hours :: DataSetType
pattern DataSetType_Disbursed_amount_by_instance_hours = DataSetType' "disbursed_amount_by_instance_hours"

pattern DataSetType_Disbursed_amount_by_product :: DataSetType
pattern DataSetType_Disbursed_amount_by_product = DataSetType' "disbursed_amount_by_product"

pattern DataSetType_Disbursed_amount_by_product_with_uncollected_funds :: DataSetType
pattern DataSetType_Disbursed_amount_by_product_with_uncollected_funds = DataSetType' "disbursed_amount_by_product_with_uncollected_funds"

pattern DataSetType_Disbursed_amount_by_uncollected_funds_breakdown :: DataSetType
pattern DataSetType_Disbursed_amount_by_uncollected_funds_breakdown = DataSetType' "disbursed_amount_by_uncollected_funds_breakdown"

pattern DataSetType_Monthly_revenue_annual_subscriptions :: DataSetType
pattern DataSetType_Monthly_revenue_annual_subscriptions = DataSetType' "monthly_revenue_annual_subscriptions"

pattern DataSetType_Monthly_revenue_billing_and_revenue_data :: DataSetType
pattern DataSetType_Monthly_revenue_billing_and_revenue_data = DataSetType' "monthly_revenue_billing_and_revenue_data"

pattern DataSetType_Monthly_revenue_field_demonstration_usage :: DataSetType
pattern DataSetType_Monthly_revenue_field_demonstration_usage = DataSetType' "monthly_revenue_field_demonstration_usage"

pattern DataSetType_Monthly_revenue_flexible_payment_schedule :: DataSetType
pattern DataSetType_Monthly_revenue_flexible_payment_schedule = DataSetType' "monthly_revenue_flexible_payment_schedule"

pattern DataSetType_Sales_compensation_billed_revenue :: DataSetType
pattern DataSetType_Sales_compensation_billed_revenue = DataSetType' "sales_compensation_billed_revenue"

pattern DataSetType_Us_sales_and_use_tax_records :: DataSetType
pattern DataSetType_Us_sales_and_use_tax_records = DataSetType' "us_sales_and_use_tax_records"

{-# COMPLETE
  DataSetType_Customer_profile_by_geography,
  DataSetType_Customer_profile_by_industry,
  DataSetType_Customer_profile_by_revenue,
  DataSetType_Customer_subscriber_annual_subscriptions,
  DataSetType_Customer_subscriber_hourly_monthly_subscriptions,
  DataSetType_Daily_business_canceled_product_subscribers,
  DataSetType_Daily_business_fees,
  DataSetType_Daily_business_free_trial_conversions,
  DataSetType_Daily_business_new_instances,
  DataSetType_Daily_business_new_product_subscribers,
  DataSetType_Daily_business_usage_by_instance_type,
  DataSetType_Disbursed_amount_by_age_of_disbursed_funds,
  DataSetType_Disbursed_amount_by_age_of_past_due_funds,
  DataSetType_Disbursed_amount_by_age_of_uncollected_funds,
  DataSetType_Disbursed_amount_by_customer_geo,
  DataSetType_Disbursed_amount_by_instance_hours,
  DataSetType_Disbursed_amount_by_product,
  DataSetType_Disbursed_amount_by_product_with_uncollected_funds,
  DataSetType_Disbursed_amount_by_uncollected_funds_breakdown,
  DataSetType_Monthly_revenue_annual_subscriptions,
  DataSetType_Monthly_revenue_billing_and_revenue_data,
  DataSetType_Monthly_revenue_field_demonstration_usage,
  DataSetType_Monthly_revenue_flexible_payment_schedule,
  DataSetType_Sales_compensation_billed_revenue,
  DataSetType_Us_sales_and_use_tax_records,
  DataSetType'
  #-}
