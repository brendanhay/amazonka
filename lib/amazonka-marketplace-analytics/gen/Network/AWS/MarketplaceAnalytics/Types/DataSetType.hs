{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Types.DataSetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceAnalytics.Types.DataSetType
  ( DataSetType
      ( DataSetType',
        CustomerProfileByGeography,
        CustomerProfileByIndustry,
        CustomerProfileByRevenue,
        CustomerSubscriberAnnualSubscriptions,
        CustomerSubscriberHourlyMonthlySubscriptions,
        DailyBusinessCanceledProductSubscribers,
        DailyBusinessFees,
        DailyBusinessFreeTrialConversions,
        DailyBusinessNewInstances,
        DailyBusinessNewProductSubscribers,
        DailyBusinessUsageByInstanceType,
        DisbursedAmountByAgeOfDisbursedFunds,
        DisbursedAmountByAgeOfPastDueFunds,
        DisbursedAmountByAgeOfUncollectedFunds,
        DisbursedAmountByCustomerGeo,
        DisbursedAmountByInstanceHours,
        DisbursedAmountByProduct,
        DisbursedAmountByProductWithUncollectedFunds,
        DisbursedAmountByUncollectedFundsBreakdown,
        MonthlyRevenueAnnualSubscriptions,
        MonthlyRevenueBillingAndRevenueData,
        MonthlyRevenueFieldDemonstrationUsage,
        MonthlyRevenueFlexiblePaymentSchedule,
        SalesCompensationBilledRevenue,
        UsSalesAndUseTaxRecords
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DataSetType = DataSetType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CustomerProfileByGeography :: DataSetType
pattern CustomerProfileByGeography = DataSetType' "customer_profile_by_geography"

pattern CustomerProfileByIndustry :: DataSetType
pattern CustomerProfileByIndustry = DataSetType' "customer_profile_by_industry"

pattern CustomerProfileByRevenue :: DataSetType
pattern CustomerProfileByRevenue = DataSetType' "customer_profile_by_revenue"

pattern CustomerSubscriberAnnualSubscriptions :: DataSetType
pattern CustomerSubscriberAnnualSubscriptions = DataSetType' "customer_subscriber_annual_subscriptions"

pattern CustomerSubscriberHourlyMonthlySubscriptions :: DataSetType
pattern CustomerSubscriberHourlyMonthlySubscriptions = DataSetType' "customer_subscriber_hourly_monthly_subscriptions"

pattern DailyBusinessCanceledProductSubscribers :: DataSetType
pattern DailyBusinessCanceledProductSubscribers = DataSetType' "daily_business_canceled_product_subscribers"

pattern DailyBusinessFees :: DataSetType
pattern DailyBusinessFees = DataSetType' "daily_business_fees"

pattern DailyBusinessFreeTrialConversions :: DataSetType
pattern DailyBusinessFreeTrialConversions = DataSetType' "daily_business_free_trial_conversions"

pattern DailyBusinessNewInstances :: DataSetType
pattern DailyBusinessNewInstances = DataSetType' "daily_business_new_instances"

pattern DailyBusinessNewProductSubscribers :: DataSetType
pattern DailyBusinessNewProductSubscribers = DataSetType' "daily_business_new_product_subscribers"

pattern DailyBusinessUsageByInstanceType :: DataSetType
pattern DailyBusinessUsageByInstanceType = DataSetType' "daily_business_usage_by_instance_type"

pattern DisbursedAmountByAgeOfDisbursedFunds :: DataSetType
pattern DisbursedAmountByAgeOfDisbursedFunds = DataSetType' "disbursed_amount_by_age_of_disbursed_funds"

pattern DisbursedAmountByAgeOfPastDueFunds :: DataSetType
pattern DisbursedAmountByAgeOfPastDueFunds = DataSetType' "disbursed_amount_by_age_of_past_due_funds"

pattern DisbursedAmountByAgeOfUncollectedFunds :: DataSetType
pattern DisbursedAmountByAgeOfUncollectedFunds = DataSetType' "disbursed_amount_by_age_of_uncollected_funds"

pattern DisbursedAmountByCustomerGeo :: DataSetType
pattern DisbursedAmountByCustomerGeo = DataSetType' "disbursed_amount_by_customer_geo"

pattern DisbursedAmountByInstanceHours :: DataSetType
pattern DisbursedAmountByInstanceHours = DataSetType' "disbursed_amount_by_instance_hours"

pattern DisbursedAmountByProduct :: DataSetType
pattern DisbursedAmountByProduct = DataSetType' "disbursed_amount_by_product"

pattern DisbursedAmountByProductWithUncollectedFunds :: DataSetType
pattern DisbursedAmountByProductWithUncollectedFunds = DataSetType' "disbursed_amount_by_product_with_uncollected_funds"

pattern DisbursedAmountByUncollectedFundsBreakdown :: DataSetType
pattern DisbursedAmountByUncollectedFundsBreakdown = DataSetType' "disbursed_amount_by_uncollected_funds_breakdown"

pattern MonthlyRevenueAnnualSubscriptions :: DataSetType
pattern MonthlyRevenueAnnualSubscriptions = DataSetType' "monthly_revenue_annual_subscriptions"

pattern MonthlyRevenueBillingAndRevenueData :: DataSetType
pattern MonthlyRevenueBillingAndRevenueData = DataSetType' "monthly_revenue_billing_and_revenue_data"

pattern MonthlyRevenueFieldDemonstrationUsage :: DataSetType
pattern MonthlyRevenueFieldDemonstrationUsage = DataSetType' "monthly_revenue_field_demonstration_usage"

pattern MonthlyRevenueFlexiblePaymentSchedule :: DataSetType
pattern MonthlyRevenueFlexiblePaymentSchedule = DataSetType' "monthly_revenue_flexible_payment_schedule"

pattern SalesCompensationBilledRevenue :: DataSetType
pattern SalesCompensationBilledRevenue = DataSetType' "sales_compensation_billed_revenue"

pattern UsSalesAndUseTaxRecords :: DataSetType
pattern UsSalesAndUseTaxRecords = DataSetType' "us_sales_and_use_tax_records"

{-# COMPLETE
  CustomerProfileByGeography,
  CustomerProfileByIndustry,
  CustomerProfileByRevenue,
  CustomerSubscriberAnnualSubscriptions,
  CustomerSubscriberHourlyMonthlySubscriptions,
  DailyBusinessCanceledProductSubscribers,
  DailyBusinessFees,
  DailyBusinessFreeTrialConversions,
  DailyBusinessNewInstances,
  DailyBusinessNewProductSubscribers,
  DailyBusinessUsageByInstanceType,
  DisbursedAmountByAgeOfDisbursedFunds,
  DisbursedAmountByAgeOfPastDueFunds,
  DisbursedAmountByAgeOfUncollectedFunds,
  DisbursedAmountByCustomerGeo,
  DisbursedAmountByInstanceHours,
  DisbursedAmountByProduct,
  DisbursedAmountByProductWithUncollectedFunds,
  DisbursedAmountByUncollectedFundsBreakdown,
  MonthlyRevenueAnnualSubscriptions,
  MonthlyRevenueBillingAndRevenueData,
  MonthlyRevenueFieldDemonstrationUsage,
  MonthlyRevenueFlexiblePaymentSchedule,
  SalesCompensationBilledRevenue,
  UsSalesAndUseTaxRecords,
  DataSetType'
  #-}
