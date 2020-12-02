{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.Sum where

import Network.AWS.Prelude

-- | The type of a budget. It should be COST, USAGE, or RI_UTILIZATION.
--
--
data BudgetType
  = Cost
  | RiCoverage
  | RiUtilization
  | Usage
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BudgetType where
    parser = takeLowerText >>= \case
        "cost" -> pure Cost
        "ri_coverage" -> pure RiCoverage
        "ri_utilization" -> pure RiUtilization
        "usage" -> pure Usage
        e -> fromTextError $ "Failure parsing BudgetType from value: '" <> e
           <> "'. Accepted values: cost, ri_coverage, ri_utilization, usage"

instance ToText BudgetType where
    toText = \case
        Cost -> "COST"
        RiCoverage -> "RI_COVERAGE"
        RiUtilization -> "RI_UTILIZATION"
        Usage -> "USAGE"

instance Hashable     BudgetType
instance NFData       BudgetType
instance ToByteString BudgetType
instance ToQuery      BudgetType
instance ToHeader     BudgetType

instance ToJSON BudgetType where
    toJSON = toJSONText

instance FromJSON BudgetType where
    parseJSON = parseJSONText "BudgetType"

-- | The comparison operator of a notification. Currently we support less than, equal to and greater than.
--
--
data ComparisonOperator
  = EqualTo
  | GreaterThan
  | LessThan
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "equal_to" -> pure EqualTo
        "greater_than" -> pure GreaterThan
        "less_than" -> pure LessThan
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: equal_to, greater_than, less_than"

instance ToText ComparisonOperator where
    toText = \case
        EqualTo -> "EQUAL_TO"
        GreaterThan -> "GREATER_THAN"
        LessThan -> "LESS_THAN"

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"

-- | The type of a notification. It should be ACTUAL or FORECASTED.
--
--
data NotificationType
  = Actual
  | Forecasted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotificationType where
    parser = takeLowerText >>= \case
        "actual" -> pure Actual
        "forecasted" -> pure Forecasted
        e -> fromTextError $ "Failure parsing NotificationType from value: '" <> e
           <> "'. Accepted values: actual, forecasted"

instance ToText NotificationType where
    toText = \case
        Actual -> "ACTUAL"
        Forecasted -> "FORECASTED"

instance Hashable     NotificationType
instance NFData       NotificationType
instance ToByteString NotificationType
instance ToQuery      NotificationType
instance ToHeader     NotificationType

instance ToJSON NotificationType where
    toJSON = toJSONText

instance FromJSON NotificationType where
    parseJSON = parseJSONText "NotificationType"

-- | The subscription type of the subscriber. It can be SMS or EMAIL.
--
--
data SubscriptionType
  = Email
  | SNS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubscriptionType where
    parser = takeLowerText >>= \case
        "email" -> pure Email
        "sns" -> pure SNS
        e -> fromTextError $ "Failure parsing SubscriptionType from value: '" <> e
           <> "'. Accepted values: email, sns"

instance ToText SubscriptionType where
    toText = \case
        Email -> "EMAIL"
        SNS -> "SNS"

instance Hashable     SubscriptionType
instance NFData       SubscriptionType
instance ToByteString SubscriptionType
instance ToQuery      SubscriptionType
instance ToHeader     SubscriptionType

instance ToJSON SubscriptionType where
    toJSON = toJSONText

instance FromJSON SubscriptionType where
    parseJSON = parseJSONText "SubscriptionType"

-- | The type of threshold for a notification. It can be PERCENTAGE or ABSOLUTE_VALUE.
--
--
data ThresholdType
  = AbsoluteValue
  | Percentage
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ThresholdType where
    parser = takeLowerText >>= \case
        "absolute_value" -> pure AbsoluteValue
        "percentage" -> pure Percentage
        e -> fromTextError $ "Failure parsing ThresholdType from value: '" <> e
           <> "'. Accepted values: absolute_value, percentage"

instance ToText ThresholdType where
    toText = \case
        AbsoluteValue -> "ABSOLUTE_VALUE"
        Percentage -> "PERCENTAGE"

instance Hashable     ThresholdType
instance NFData       ThresholdType
instance ToByteString ThresholdType
instance ToQuery      ThresholdType
instance ToHeader     ThresholdType

instance ToJSON ThresholdType where
    toJSON = toJSONText

instance FromJSON ThresholdType where
    parseJSON = parseJSONText "ThresholdType"

-- | The time unit of the budget. e.g. MONTHLY, QUARTERLY, etc.
--
--
data TimeUnit
  = Annually
  | Daily
  | Monthly
  | Quarterly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TimeUnit where
    parser = takeLowerText >>= \case
        "annually" -> pure Annually
        "daily" -> pure Daily
        "monthly" -> pure Monthly
        "quarterly" -> pure Quarterly
        e -> fromTextError $ "Failure parsing TimeUnit from value: '" <> e
           <> "'. Accepted values: annually, daily, monthly, quarterly"

instance ToText TimeUnit where
    toText = \case
        Annually -> "ANNUALLY"
        Daily -> "DAILY"
        Monthly -> "MONTHLY"
        Quarterly -> "QUARTERLY"

instance Hashable     TimeUnit
instance NFData       TimeUnit
instance ToByteString TimeUnit
instance ToQuery      TimeUnit
instance ToHeader     TimeUnit

instance ToJSON TimeUnit where
    toJSON = toJSONText

instance FromJSON TimeUnit where
    parseJSON = parseJSONText "TimeUnit"
