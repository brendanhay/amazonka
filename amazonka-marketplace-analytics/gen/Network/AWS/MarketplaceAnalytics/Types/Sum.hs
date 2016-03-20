{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceAnalytics.Types.Sum where

import           Network.AWS.Prelude

data DataSetType
    = CustomerProfileByGeography
    | CustomerProfileByIndustry
    | CustomerProfileByRevenue
    | CustomerSubscriberAnnualSubscriptions
    | CustomerSubscriberHourlyMonthlySubscriptions
    | DailyBusinessCanceledProductSubscribers
    | DailyBusinessFees
    | DailyBusinessFreeTrialConversions
    | DailyBusinessNewInstances
    | DailyBusinessNewProductSubscribers
    | DailyBusinessUsageByInstanceType
    | DisbursedAmountByAgeOfDisbursedFunds
    | DisbursedAmountByAgeOfUncollectedFunds
    | DisbursedAmountByCustomerGeo
    | DisbursedAmountByProduct
    | DisbursedAmountByProductWithUncollectedFunds
    | MonthlyRevenueAnnualSubscriptions
    | MonthlyRevenueBillingAndRevenueData
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DataSetType where
    parser = takeLowerText >>= \case
        "customer_profile_by_geography" -> pure CustomerProfileByGeography
        "customer_profile_by_industry" -> pure CustomerProfileByIndustry
        "customer_profile_by_revenue" -> pure CustomerProfileByRevenue
        "customer_subscriber_annual_subscriptions" -> pure CustomerSubscriberAnnualSubscriptions
        "customer_subscriber_hourly_monthly_subscriptions" -> pure CustomerSubscriberHourlyMonthlySubscriptions
        "daily_business_canceled_product_subscribers" -> pure DailyBusinessCanceledProductSubscribers
        "daily_business_fees" -> pure DailyBusinessFees
        "daily_business_free_trial_conversions" -> pure DailyBusinessFreeTrialConversions
        "daily_business_new_instances" -> pure DailyBusinessNewInstances
        "daily_business_new_product_subscribers" -> pure DailyBusinessNewProductSubscribers
        "daily_business_usage_by_instance_type" -> pure DailyBusinessUsageByInstanceType
        "disbursed_amount_by_age_of_disbursed_funds" -> pure DisbursedAmountByAgeOfDisbursedFunds
        "disbursed_amount_by_age_of_uncollected_funds" -> pure DisbursedAmountByAgeOfUncollectedFunds
        "disbursed_amount_by_customer_geo" -> pure DisbursedAmountByCustomerGeo
        "disbursed_amount_by_product" -> pure DisbursedAmountByProduct
        "disbursed_amount_by_product_with_uncollected_funds" -> pure DisbursedAmountByProductWithUncollectedFunds
        "monthly_revenue_annual_subscriptions" -> pure MonthlyRevenueAnnualSubscriptions
        "monthly_revenue_billing_and_revenue_data" -> pure MonthlyRevenueBillingAndRevenueData
        e -> fromTextError $ "Failure parsing DataSetType from value: '" <> e
           <> "'. Accepted values: customer_profile_by_geography, customer_profile_by_industry, customer_profile_by_revenue, customer_subscriber_annual_subscriptions, customer_subscriber_hourly_monthly_subscriptions, daily_business_canceled_product_subscribers, daily_business_fees, daily_business_free_trial_conversions, daily_business_new_instances, daily_business_new_product_subscribers, daily_business_usage_by_instance_type, disbursed_amount_by_age_of_disbursed_funds, disbursed_amount_by_age_of_uncollected_funds, disbursed_amount_by_customer_geo, disbursed_amount_by_product, disbursed_amount_by_product_with_uncollected_funds, monthly_revenue_annual_subscriptions, monthly_revenue_billing_and_revenue_data"

instance ToText DataSetType where
    toText = \case
        CustomerProfileByGeography -> "customer_profile_by_geography"
        CustomerProfileByIndustry -> "customer_profile_by_industry"
        CustomerProfileByRevenue -> "customer_profile_by_revenue"
        CustomerSubscriberAnnualSubscriptions -> "customer_subscriber_annual_subscriptions"
        CustomerSubscriberHourlyMonthlySubscriptions -> "customer_subscriber_hourly_monthly_subscriptions"
        DailyBusinessCanceledProductSubscribers -> "daily_business_canceled_product_subscribers"
        DailyBusinessFees -> "daily_business_fees"
        DailyBusinessFreeTrialConversions -> "daily_business_free_trial_conversions"
        DailyBusinessNewInstances -> "daily_business_new_instances"
        DailyBusinessNewProductSubscribers -> "daily_business_new_product_subscribers"
        DailyBusinessUsageByInstanceType -> "daily_business_usage_by_instance_type"
        DisbursedAmountByAgeOfDisbursedFunds -> "disbursed_amount_by_age_of_disbursed_funds"
        DisbursedAmountByAgeOfUncollectedFunds -> "disbursed_amount_by_age_of_uncollected_funds"
        DisbursedAmountByCustomerGeo -> "disbursed_amount_by_customer_geo"
        DisbursedAmountByProduct -> "disbursed_amount_by_product"
        DisbursedAmountByProductWithUncollectedFunds -> "disbursed_amount_by_product_with_uncollected_funds"
        MonthlyRevenueAnnualSubscriptions -> "monthly_revenue_annual_subscriptions"
        MonthlyRevenueBillingAndRevenueData -> "monthly_revenue_billing_and_revenue_data"

instance Hashable     DataSetType
instance ToByteString DataSetType
instance ToQuery      DataSetType
instance ToHeader     DataSetType

instance ToJSON DataSetType where
    toJSON = toJSONText
