{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.Product where

import Network.AWS.Budgets.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of the @CreateBudget@ operation. The content consists of the detailed metadata and data file information, and the current status of the @budget@ object.
--
--
-- This is the ARN pattern for a budget:
--
-- @arn:aws:budgetservice::AccountId:budget/budgetName@
--
--
-- /See:/ 'budget' smart constructor.
data Budget = Budget'
  { _bCalculatedSpend :: !(Maybe CalculatedSpend)
  , _bLastUpdatedTime :: !(Maybe POSIX)
  , _bBudgetLimit     :: !(Maybe Spend)
  , _bTimePeriod      :: !(Maybe TimePeriod)
  , _bCostTypes       :: !(Maybe CostTypes)
  , _bCostFilters     :: !(Maybe (Map Text [Text]))
  , _bBudgetName      :: !Text
  , _bTimeUnit        :: !TimeUnit
  , _bBudgetType      :: !BudgetType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Budget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCalculatedSpend' - The actual and forecasted cost or usage that the budget tracks.
--
-- * 'bLastUpdatedTime' - The last time that you updated this budget.
--
-- * 'bBudgetLimit' - The total amount of cost, usage, RI utilization, or RI coverage that you want to track with your budget. @BudgetLimit@ is required for cost or usage budgets, but optional for RI utilization or coverage budgets. RI utilization or coverage budgets default to @100@ , which is the only valid value for RI utilization or coverage budgets.
--
-- * 'bTimePeriod' - The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .  If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.  You can change either date with the @UpdateBudget@ operation. After the end date, AWS deletes the budget and all associated notifications and subscribers.
--
-- * 'bCostTypes' - The types of costs that are included in this @COST@ budget. @USAGE@ , @RI_UTILIZATION@ , and @RI_COVERAGE@ budgets do not have @CostTypes@ .
--
-- * 'bCostFilters' - The cost filters, such as service or region, that are applied to a budget. AWS Budgets supports the following services as a filter for RI budgets:     * Amazon Elastic Compute Cloud - Compute     * Amazon Redshift     * Amazon Relational Database Service     * Amazon ElastiCache     * Amazon Elasticsearch Service
--
-- * 'bBudgetName' - The name of a budget. The name must be unique within accounts. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
--
-- * 'bTimeUnit' - The length of time until a budget resets the actual and forecasted spend. @DAILY@ is available only for @RI_UTILIZATION@ and @RI_COVERAGE@ budgets.
--
-- * 'bBudgetType' - Whether this budget tracks monetary costs, usage, RI utilization, or RI coverage.
budget
    :: Text -- ^ 'bBudgetName'
    -> TimeUnit -- ^ 'bTimeUnit'
    -> BudgetType -- ^ 'bBudgetType'
    -> Budget
budget pBudgetName_ pTimeUnit_ pBudgetType_ =
  Budget'
    { _bCalculatedSpend = Nothing
    , _bLastUpdatedTime = Nothing
    , _bBudgetLimit = Nothing
    , _bTimePeriod = Nothing
    , _bCostTypes = Nothing
    , _bCostFilters = Nothing
    , _bBudgetName = pBudgetName_
    , _bTimeUnit = pTimeUnit_
    , _bBudgetType = pBudgetType_
    }


-- | The actual and forecasted cost or usage that the budget tracks.
bCalculatedSpend :: Lens' Budget (Maybe CalculatedSpend)
bCalculatedSpend = lens _bCalculatedSpend (\ s a -> s{_bCalculatedSpend = a})

-- | The last time that you updated this budget.
bLastUpdatedTime :: Lens' Budget (Maybe UTCTime)
bLastUpdatedTime = lens _bLastUpdatedTime (\ s a -> s{_bLastUpdatedTime = a}) . mapping _Time

-- | The total amount of cost, usage, RI utilization, or RI coverage that you want to track with your budget. @BudgetLimit@ is required for cost or usage budgets, but optional for RI utilization or coverage budgets. RI utilization or coverage budgets default to @100@ , which is the only valid value for RI utilization or coverage budgets.
bBudgetLimit :: Lens' Budget (Maybe Spend)
bBudgetLimit = lens _bBudgetLimit (\ s a -> s{_bBudgetLimit = a})

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. The end date must come before @06/15/87 00:00 UTC@ .  If you create your budget and don't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API.  You can change either date with the @UpdateBudget@ operation. After the end date, AWS deletes the budget and all associated notifications and subscribers.
bTimePeriod :: Lens' Budget (Maybe TimePeriod)
bTimePeriod = lens _bTimePeriod (\ s a -> s{_bTimePeriod = a})

-- | The types of costs that are included in this @COST@ budget. @USAGE@ , @RI_UTILIZATION@ , and @RI_COVERAGE@ budgets do not have @CostTypes@ .
bCostTypes :: Lens' Budget (Maybe CostTypes)
bCostTypes = lens _bCostTypes (\ s a -> s{_bCostTypes = a})

-- | The cost filters, such as service or region, that are applied to a budget. AWS Budgets supports the following services as a filter for RI budgets:     * Amazon Elastic Compute Cloud - Compute     * Amazon Redshift     * Amazon Relational Database Service     * Amazon ElastiCache     * Amazon Elasticsearch Service
bCostFilters :: Lens' Budget (HashMap Text [Text])
bCostFilters = lens _bCostFilters (\ s a -> s{_bCostFilters = a}) . _Default . _Map

-- | The name of a budget. The name must be unique within accounts. The @:@ and @\@ characters aren't allowed in @BudgetName@ .
bBudgetName :: Lens' Budget Text
bBudgetName = lens _bBudgetName (\ s a -> s{_bBudgetName = a})

-- | The length of time until a budget resets the actual and forecasted spend. @DAILY@ is available only for @RI_UTILIZATION@ and @RI_COVERAGE@ budgets.
bTimeUnit :: Lens' Budget TimeUnit
bTimeUnit = lens _bTimeUnit (\ s a -> s{_bTimeUnit = a})

-- | Whether this budget tracks monetary costs, usage, RI utilization, or RI coverage.
bBudgetType :: Lens' Budget BudgetType
bBudgetType = lens _bBudgetType (\ s a -> s{_bBudgetType = a})

instance FromJSON Budget where
        parseJSON
          = withObject "Budget"
              (\ x ->
                 Budget' <$>
                   (x .:? "CalculatedSpend") <*>
                     (x .:? "LastUpdatedTime")
                     <*> (x .:? "BudgetLimit")
                     <*> (x .:? "TimePeriod")
                     <*> (x .:? "CostTypes")
                     <*> (x .:? "CostFilters" .!= mempty)
                     <*> (x .: "BudgetName")
                     <*> (x .: "TimeUnit")
                     <*> (x .: "BudgetType"))

instance Hashable Budget where

instance NFData Budget where

instance ToJSON Budget where
        toJSON Budget'{..}
          = object
              (catMaybes
                 [("CalculatedSpend" .=) <$> _bCalculatedSpend,
                  ("LastUpdatedTime" .=) <$> _bLastUpdatedTime,
                  ("BudgetLimit" .=) <$> _bBudgetLimit,
                  ("TimePeriod" .=) <$> _bTimePeriod,
                  ("CostTypes" .=) <$> _bCostTypes,
                  ("CostFilters" .=) <$> _bCostFilters,
                  Just ("BudgetName" .= _bBudgetName),
                  Just ("TimeUnit" .= _bTimeUnit),
                  Just ("BudgetType" .= _bBudgetType)])

-- | A history of the state of a budget at the end of the budget's specified time period.
--
--
--
-- /See:/ 'budgetPerformanceHistory' smart constructor.
data BudgetPerformanceHistory = BudgetPerformanceHistory'
  { _bphBudgetedAndActualAmountsList :: !(Maybe [BudgetedAndActualAmounts])
  , _bphTimeUnit                     :: !(Maybe TimeUnit)
  , _bphBudgetName                   :: !(Maybe Text)
  , _bphBudgetType                   :: !(Maybe BudgetType)
  , _bphCostTypes                    :: !(Maybe CostTypes)
  , _bphCostFilters                  :: !(Maybe (Map Text [Text]))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BudgetPerformanceHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bphBudgetedAndActualAmountsList' - A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
--
-- * 'bphTimeUnit' - Undocumented member.
--
-- * 'bphBudgetName' - Undocumented member.
--
-- * 'bphBudgetType' - Undocumented member.
--
-- * 'bphCostTypes' - The history of the cost types for a budget during the specified time period.
--
-- * 'bphCostFilters' - The history of the cost filters for a budget during the specified time period.
budgetPerformanceHistory
    :: BudgetPerformanceHistory
budgetPerformanceHistory =
  BudgetPerformanceHistory'
    { _bphBudgetedAndActualAmountsList = Nothing
    , _bphTimeUnit = Nothing
    , _bphBudgetName = Nothing
    , _bphBudgetType = Nothing
    , _bphCostTypes = Nothing
    , _bphCostFilters = Nothing
    }


-- | A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
bphBudgetedAndActualAmountsList :: Lens' BudgetPerformanceHistory [BudgetedAndActualAmounts]
bphBudgetedAndActualAmountsList = lens _bphBudgetedAndActualAmountsList (\ s a -> s{_bphBudgetedAndActualAmountsList = a}) . _Default . _Coerce

-- | Undocumented member.
bphTimeUnit :: Lens' BudgetPerformanceHistory (Maybe TimeUnit)
bphTimeUnit = lens _bphTimeUnit (\ s a -> s{_bphTimeUnit = a})

-- | Undocumented member.
bphBudgetName :: Lens' BudgetPerformanceHistory (Maybe Text)
bphBudgetName = lens _bphBudgetName (\ s a -> s{_bphBudgetName = a})

-- | Undocumented member.
bphBudgetType :: Lens' BudgetPerformanceHistory (Maybe BudgetType)
bphBudgetType = lens _bphBudgetType (\ s a -> s{_bphBudgetType = a})

-- | The history of the cost types for a budget during the specified time period.
bphCostTypes :: Lens' BudgetPerformanceHistory (Maybe CostTypes)
bphCostTypes = lens _bphCostTypes (\ s a -> s{_bphCostTypes = a})

-- | The history of the cost filters for a budget during the specified time period.
bphCostFilters :: Lens' BudgetPerformanceHistory (HashMap Text [Text])
bphCostFilters = lens _bphCostFilters (\ s a -> s{_bphCostFilters = a}) . _Default . _Map

instance FromJSON BudgetPerformanceHistory where
        parseJSON
          = withObject "BudgetPerformanceHistory"
              (\ x ->
                 BudgetPerformanceHistory' <$>
                   (x .:? "BudgetedAndActualAmountsList" .!= mempty) <*>
                     (x .:? "TimeUnit")
                     <*> (x .:? "BudgetName")
                     <*> (x .:? "BudgetType")
                     <*> (x .:? "CostTypes")
                     <*> (x .:? "CostFilters" .!= mempty))

instance Hashable BudgetPerformanceHistory where

instance NFData BudgetPerformanceHistory where

-- | The amount of cost or usage that you created the budget for, compared to your actual costs or usage.
--
--
--
-- /See:/ 'budgetedAndActualAmounts' smart constructor.
data BudgetedAndActualAmounts = BudgetedAndActualAmounts'
  { _baaaTimePeriod     :: !(Maybe TimePeriod)
  , _baaaActualAmount   :: !(Maybe Spend)
  , _baaaBudgetedAmount :: !(Maybe Spend)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BudgetedAndActualAmounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baaaTimePeriod' - The time period covered by this budget comparison.
--
-- * 'baaaActualAmount' - Your actual costs or usage for a budget period.
--
-- * 'baaaBudgetedAmount' - The amount of cost or usage that you created the budget for.
budgetedAndActualAmounts
    :: BudgetedAndActualAmounts
budgetedAndActualAmounts =
  BudgetedAndActualAmounts'
    { _baaaTimePeriod = Nothing
    , _baaaActualAmount = Nothing
    , _baaaBudgetedAmount = Nothing
    }


-- | The time period covered by this budget comparison.
baaaTimePeriod :: Lens' BudgetedAndActualAmounts (Maybe TimePeriod)
baaaTimePeriod = lens _baaaTimePeriod (\ s a -> s{_baaaTimePeriod = a})

-- | Your actual costs or usage for a budget period.
baaaActualAmount :: Lens' BudgetedAndActualAmounts (Maybe Spend)
baaaActualAmount = lens _baaaActualAmount (\ s a -> s{_baaaActualAmount = a})

-- | The amount of cost or usage that you created the budget for.
baaaBudgetedAmount :: Lens' BudgetedAndActualAmounts (Maybe Spend)
baaaBudgetedAmount = lens _baaaBudgetedAmount (\ s a -> s{_baaaBudgetedAmount = a})

instance FromJSON BudgetedAndActualAmounts where
        parseJSON
          = withObject "BudgetedAndActualAmounts"
              (\ x ->
                 BudgetedAndActualAmounts' <$>
                   (x .:? "TimePeriod") <*> (x .:? "ActualAmount") <*>
                     (x .:? "BudgetedAmount"))

instance Hashable BudgetedAndActualAmounts where

instance NFData BudgetedAndActualAmounts where

-- | The spend objects that are associated with this budget. The @actualSpend@ tracks how much you've used, cost, usage, or RI units, and the @forecastedSpend@ tracks how much you are predicted to spend if your current usage remains steady.
--
--
-- For example, if it is the 20th of the month and you have spent @50@ dollars on Amazon EC2, your @actualSpend@ is @50 USD@ , and your @forecastedSpend@ is @75 USD@ .
--
--
-- /See:/ 'calculatedSpend' smart constructor.
data CalculatedSpend = CalculatedSpend'
  { _csForecastedSpend :: !(Maybe Spend)
  , _csActualSpend     :: !Spend
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CalculatedSpend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csForecastedSpend' - The amount of cost, usage, or RI units that you are forecasted to use.
--
-- * 'csActualSpend' - The amount of cost, usage, or RI units that you have used.
calculatedSpend
    :: Spend -- ^ 'csActualSpend'
    -> CalculatedSpend
calculatedSpend pActualSpend_ =
  CalculatedSpend'
    {_csForecastedSpend = Nothing, _csActualSpend = pActualSpend_}


-- | The amount of cost, usage, or RI units that you are forecasted to use.
csForecastedSpend :: Lens' CalculatedSpend (Maybe Spend)
csForecastedSpend = lens _csForecastedSpend (\ s a -> s{_csForecastedSpend = a})

-- | The amount of cost, usage, or RI units that you have used.
csActualSpend :: Lens' CalculatedSpend Spend
csActualSpend = lens _csActualSpend (\ s a -> s{_csActualSpend = a})

instance FromJSON CalculatedSpend where
        parseJSON
          = withObject "CalculatedSpend"
              (\ x ->
                 CalculatedSpend' <$>
                   (x .:? "ForecastedSpend") <*> (x .: "ActualSpend"))

instance Hashable CalculatedSpend where

instance NFData CalculatedSpend where

instance ToJSON CalculatedSpend where
        toJSON CalculatedSpend'{..}
          = object
              (catMaybes
                 [("ForecastedSpend" .=) <$> _csForecastedSpend,
                  Just ("ActualSpend" .= _csActualSpend)])

-- | The types of cost that are included in a @COST@ budget, such as tax and subscriptions.
--
--
-- @USAGE@ , @RI_UTILIZATION@ , and @RI_COVERAGE@ budgets do not have @CostTypes@ .
--
--
-- /See:/ 'costTypes' smart constructor.
data CostTypes = CostTypes'
  { _ctUseAmortized             :: !(Maybe Bool)
  , _ctIncludeRecurring         :: !(Maybe Bool)
  , _ctUseBlended               :: !(Maybe Bool)
  , _ctIncludeSupport           :: !(Maybe Bool)
  , _ctIncludeDiscount          :: !(Maybe Bool)
  , _ctIncludeSubscription      :: !(Maybe Bool)
  , _ctIncludeRefund            :: !(Maybe Bool)
  , _ctIncludeUpfront           :: !(Maybe Bool)
  , _ctIncludeOtherSubscription :: !(Maybe Bool)
  , _ctIncludeTax               :: !(Maybe Bool)
  , _ctIncludeCredit            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CostTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctUseAmortized' - Specifies whether a budget uses the amortized rate. The default value is @false@ .
--
-- * 'ctIncludeRecurring' - Specifies whether a budget includes recurring fees such as monthly RI fees. The default value is @true@ .
--
-- * 'ctUseBlended' - Specifies whether a budget uses a blended rate. The default value is @false@ .
--
-- * 'ctIncludeSupport' - Specifies whether a budget includes support subscription fees. The default value is @true@ .
--
-- * 'ctIncludeDiscount' - Specifies whether a budget includes discounts. The default value is @true@ .
--
-- * 'ctIncludeSubscription' - Specifies whether a budget includes subscriptions. The default value is @true@ .
--
-- * 'ctIncludeRefund' - Specifies whether a budget includes refunds. The default value is @true@ .
--
-- * 'ctIncludeUpfront' - Specifies whether a budget includes upfront RI costs. The default value is @true@ .
--
-- * 'ctIncludeOtherSubscription' - Specifies whether a budget includes non-RI subscription costs. The default value is @true@ .
--
-- * 'ctIncludeTax' - Specifies whether a budget includes taxes. The default value is @true@ .
--
-- * 'ctIncludeCredit' - Specifies whether a budget includes credits. The default value is @true@ .
costTypes
    :: CostTypes
costTypes =
  CostTypes'
    { _ctUseAmortized = Nothing
    , _ctIncludeRecurring = Nothing
    , _ctUseBlended = Nothing
    , _ctIncludeSupport = Nothing
    , _ctIncludeDiscount = Nothing
    , _ctIncludeSubscription = Nothing
    , _ctIncludeRefund = Nothing
    , _ctIncludeUpfront = Nothing
    , _ctIncludeOtherSubscription = Nothing
    , _ctIncludeTax = Nothing
    , _ctIncludeCredit = Nothing
    }


-- | Specifies whether a budget uses the amortized rate. The default value is @false@ .
ctUseAmortized :: Lens' CostTypes (Maybe Bool)
ctUseAmortized = lens _ctUseAmortized (\ s a -> s{_ctUseAmortized = a})

-- | Specifies whether a budget includes recurring fees such as monthly RI fees. The default value is @true@ .
ctIncludeRecurring :: Lens' CostTypes (Maybe Bool)
ctIncludeRecurring = lens _ctIncludeRecurring (\ s a -> s{_ctIncludeRecurring = a})

-- | Specifies whether a budget uses a blended rate. The default value is @false@ .
ctUseBlended :: Lens' CostTypes (Maybe Bool)
ctUseBlended = lens _ctUseBlended (\ s a -> s{_ctUseBlended = a})

-- | Specifies whether a budget includes support subscription fees. The default value is @true@ .
ctIncludeSupport :: Lens' CostTypes (Maybe Bool)
ctIncludeSupport = lens _ctIncludeSupport (\ s a -> s{_ctIncludeSupport = a})

-- | Specifies whether a budget includes discounts. The default value is @true@ .
ctIncludeDiscount :: Lens' CostTypes (Maybe Bool)
ctIncludeDiscount = lens _ctIncludeDiscount (\ s a -> s{_ctIncludeDiscount = a})

-- | Specifies whether a budget includes subscriptions. The default value is @true@ .
ctIncludeSubscription :: Lens' CostTypes (Maybe Bool)
ctIncludeSubscription = lens _ctIncludeSubscription (\ s a -> s{_ctIncludeSubscription = a})

-- | Specifies whether a budget includes refunds. The default value is @true@ .
ctIncludeRefund :: Lens' CostTypes (Maybe Bool)
ctIncludeRefund = lens _ctIncludeRefund (\ s a -> s{_ctIncludeRefund = a})

-- | Specifies whether a budget includes upfront RI costs. The default value is @true@ .
ctIncludeUpfront :: Lens' CostTypes (Maybe Bool)
ctIncludeUpfront = lens _ctIncludeUpfront (\ s a -> s{_ctIncludeUpfront = a})

-- | Specifies whether a budget includes non-RI subscription costs. The default value is @true@ .
ctIncludeOtherSubscription :: Lens' CostTypes (Maybe Bool)
ctIncludeOtherSubscription = lens _ctIncludeOtherSubscription (\ s a -> s{_ctIncludeOtherSubscription = a})

-- | Specifies whether a budget includes taxes. The default value is @true@ .
ctIncludeTax :: Lens' CostTypes (Maybe Bool)
ctIncludeTax = lens _ctIncludeTax (\ s a -> s{_ctIncludeTax = a})

-- | Specifies whether a budget includes credits. The default value is @true@ .
ctIncludeCredit :: Lens' CostTypes (Maybe Bool)
ctIncludeCredit = lens _ctIncludeCredit (\ s a -> s{_ctIncludeCredit = a})

instance FromJSON CostTypes where
        parseJSON
          = withObject "CostTypes"
              (\ x ->
                 CostTypes' <$>
                   (x .:? "UseAmortized") <*> (x .:? "IncludeRecurring")
                     <*> (x .:? "UseBlended")
                     <*> (x .:? "IncludeSupport")
                     <*> (x .:? "IncludeDiscount")
                     <*> (x .:? "IncludeSubscription")
                     <*> (x .:? "IncludeRefund")
                     <*> (x .:? "IncludeUpfront")
                     <*> (x .:? "IncludeOtherSubscription")
                     <*> (x .:? "IncludeTax")
                     <*> (x .:? "IncludeCredit"))

instance Hashable CostTypes where

instance NFData CostTypes where

instance ToJSON CostTypes where
        toJSON CostTypes'{..}
          = object
              (catMaybes
                 [("UseAmortized" .=) <$> _ctUseAmortized,
                  ("IncludeRecurring" .=) <$> _ctIncludeRecurring,
                  ("UseBlended" .=) <$> _ctUseBlended,
                  ("IncludeSupport" .=) <$> _ctIncludeSupport,
                  ("IncludeDiscount" .=) <$> _ctIncludeDiscount,
                  ("IncludeSubscription" .=) <$>
                    _ctIncludeSubscription,
                  ("IncludeRefund" .=) <$> _ctIncludeRefund,
                  ("IncludeUpfront" .=) <$> _ctIncludeUpfront,
                  ("IncludeOtherSubscription" .=) <$>
                    _ctIncludeOtherSubscription,
                  ("IncludeTax" .=) <$> _ctIncludeTax,
                  ("IncludeCredit" .=) <$> _ctIncludeCredit])

-- | A notification that is associated with a budget. A budget can have up to five notifications.
--
--
-- Each notification must have at least one subscriber. A notification can have one SNS subscriber and up to 10 email subscribers, for a total of 11 subscribers.
--
-- For example, if you have a budget for 200 dollars and you want to be notified when you go over 160 dollars, create a notification with the following parameters:
--
--     * A notificationType of @ACTUAL@
--
--     * A @thresholdType@ of @PERCENTAGE@
--
--     * A @comparisonOperator@ of @GREATER_THAN@
--
--     * A notification @threshold@ of @80@
--
--
--
--
-- /See:/ 'notification' smart constructor.
data Notification = Notification'
  { _nThresholdType      :: !(Maybe ThresholdType)
  , _nNotificationState  :: !(Maybe NotificationState)
  , _nNotificationType   :: !NotificationType
  , _nComparisonOperator :: !ComparisonOperator
  , _nThreshold          :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nThresholdType' - The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
--
-- * 'nNotificationState' - Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
--
-- * 'nNotificationType' - Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
--
-- * 'nComparisonOperator' - The comparison that is used for this notification.
--
-- * 'nThreshold' - The threshold that is associated with a notification. Thresholds are always a percentage.
notification
    :: NotificationType -- ^ 'nNotificationType'
    -> ComparisonOperator -- ^ 'nComparisonOperator'
    -> Double -- ^ 'nThreshold'
    -> Notification
notification pNotificationType_ pComparisonOperator_ pThreshold_ =
  Notification'
    { _nThresholdType = Nothing
    , _nNotificationState = Nothing
    , _nNotificationType = pNotificationType_
    , _nComparisonOperator = pComparisonOperator_
    , _nThreshold = pThreshold_
    }


-- | The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
nThresholdType :: Lens' Notification (Maybe ThresholdType)
nThresholdType = lens _nThresholdType (\ s a -> s{_nThresholdType = a})

-- | Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
nNotificationState :: Lens' Notification (Maybe NotificationState)
nNotificationState = lens _nNotificationState (\ s a -> s{_nNotificationState = a})

-- | Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
nNotificationType :: Lens' Notification NotificationType
nNotificationType = lens _nNotificationType (\ s a -> s{_nNotificationType = a})

-- | The comparison that is used for this notification.
nComparisonOperator :: Lens' Notification ComparisonOperator
nComparisonOperator = lens _nComparisonOperator (\ s a -> s{_nComparisonOperator = a})

-- | The threshold that is associated with a notification. Thresholds are always a percentage.
nThreshold :: Lens' Notification Double
nThreshold = lens _nThreshold (\ s a -> s{_nThreshold = a})

instance FromJSON Notification where
        parseJSON
          = withObject "Notification"
              (\ x ->
                 Notification' <$>
                   (x .:? "ThresholdType") <*>
                     (x .:? "NotificationState")
                     <*> (x .: "NotificationType")
                     <*> (x .: "ComparisonOperator")
                     <*> (x .: "Threshold"))

instance Hashable Notification where

instance NFData Notification where

instance ToJSON Notification where
        toJSON Notification'{..}
          = object
              (catMaybes
                 [("ThresholdType" .=) <$> _nThresholdType,
                  ("NotificationState" .=) <$> _nNotificationState,
                  Just ("NotificationType" .= _nNotificationType),
                  Just ("ComparisonOperator" .= _nComparisonOperator),
                  Just ("Threshold" .= _nThreshold)])

-- | A notification with subscribers. A notification can have one SNS subscriber and up to 10 email subscribers, for a total of 11 subscribers.
--
--
--
-- /See:/ 'notificationWithSubscribers' smart constructor.
data NotificationWithSubscribers = NotificationWithSubscribers'
  { _nwsNotification :: !Notification
  , _nwsSubscribers  :: !(List1 Subscriber)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationWithSubscribers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nwsNotification' - The notification that is associated with a budget.
--
-- * 'nwsSubscribers' - A list of subscribers who are subscribed to this notification.
notificationWithSubscribers
    :: Notification -- ^ 'nwsNotification'
    -> NonEmpty Subscriber -- ^ 'nwsSubscribers'
    -> NotificationWithSubscribers
notificationWithSubscribers pNotification_ pSubscribers_ =
  NotificationWithSubscribers'
    { _nwsNotification = pNotification_
    , _nwsSubscribers = _List1 # pSubscribers_
    }


-- | The notification that is associated with a budget.
nwsNotification :: Lens' NotificationWithSubscribers Notification
nwsNotification = lens _nwsNotification (\ s a -> s{_nwsNotification = a})

-- | A list of subscribers who are subscribed to this notification.
nwsSubscribers :: Lens' NotificationWithSubscribers (NonEmpty Subscriber)
nwsSubscribers = lens _nwsSubscribers (\ s a -> s{_nwsSubscribers = a}) . _List1

instance Hashable NotificationWithSubscribers where

instance NFData NotificationWithSubscribers where

instance ToJSON NotificationWithSubscribers where
        toJSON NotificationWithSubscribers'{..}
          = object
              (catMaybes
                 [Just ("Notification" .= _nwsNotification),
                  Just ("Subscribers" .= _nwsSubscribers)])

-- | The amount of cost or usage that is measured for a budget.
--
--
-- For example, a @Spend@ for @3 GB@ of S3 usage would have the following parameters:
--
--     * An @Amount@ of @3@
--
--     * A @unit@ of @GB@
--
--
--
--
-- /See:/ 'spend' smart constructor.
data Spend = Spend'
  { _sAmount :: !Text
  , _sUnit   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Spend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAmount' - The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
--
-- * 'sUnit' - The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
spend
    :: Text -- ^ 'sAmount'
    -> Text -- ^ 'sUnit'
    -> Spend
spend pAmount_ pUnit_ = Spend' {_sAmount = pAmount_, _sUnit = pUnit_}


-- | The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
sAmount :: Lens' Spend Text
sAmount = lens _sAmount (\ s a -> s{_sAmount = a})

-- | The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
sUnit :: Lens' Spend Text
sUnit = lens _sUnit (\ s a -> s{_sUnit = a})

instance FromJSON Spend where
        parseJSON
          = withObject "Spend"
              (\ x -> Spend' <$> (x .: "Amount") <*> (x .: "Unit"))

instance Hashable Spend where

instance NFData Spend where

instance ToJSON Spend where
        toJSON Spend'{..}
          = object
              (catMaybes
                 [Just ("Amount" .= _sAmount),
                  Just ("Unit" .= _sUnit)])

-- | The subscriber to a budget notification. The subscriber consists of a subscription type and either an Amazon SNS topic or an email address.
--
--
-- For example, an email subscriber would have the following parameters:
--
--     * A @subscriptionType@ of @EMAIL@
--
--     * An @address@ of @example@example.com@
--
--
--
--
-- /See:/ 'subscriber' smart constructor.
data Subscriber = Subscriber'
  { _sSubscriptionType :: !SubscriptionType
  , _sAddress          :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscriber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubscriptionType' - The type of notification that AWS sends to a subscriber.
--
-- * 'sAddress' - The address that AWS sends budget notifications to, either an SNS topic or an email.
subscriber
    :: SubscriptionType -- ^ 'sSubscriptionType'
    -> Text -- ^ 'sAddress'
    -> Subscriber
subscriber pSubscriptionType_ pAddress_ =
  Subscriber'
    { _sSubscriptionType = pSubscriptionType_
    , _sAddress = _Sensitive # pAddress_
    }


-- | The type of notification that AWS sends to a subscriber.
sSubscriptionType :: Lens' Subscriber SubscriptionType
sSubscriptionType = lens _sSubscriptionType (\ s a -> s{_sSubscriptionType = a})

-- | The address that AWS sends budget notifications to, either an SNS topic or an email.
sAddress :: Lens' Subscriber Text
sAddress = lens _sAddress (\ s a -> s{_sAddress = a}) . _Sensitive

instance FromJSON Subscriber where
        parseJSON
          = withObject "Subscriber"
              (\ x ->
                 Subscriber' <$>
                   (x .: "SubscriptionType") <*> (x .: "Address"))

instance Hashable Subscriber where

instance NFData Subscriber where

instance ToJSON Subscriber where
        toJSON Subscriber'{..}
          = object
              (catMaybes
                 [Just ("SubscriptionType" .= _sSubscriptionType),
                  Just ("Address" .= _sAddress)])

-- | The period of time that is covered by a budget. The period has a start date and an end date. The start date must come before the end date. There are no restrictions on the end date.
--
--
--
-- /See:/ 'timePeriod' smart constructor.
data TimePeriod = TimePeriod'
  { _tpStart :: !(Maybe POSIX)
  , _tpEnd   :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimePeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpStart' - The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. You can change your start date with the @UpdateBudget@ operation.
--
-- * 'tpEnd' - The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
timePeriod
    :: TimePeriod
timePeriod = TimePeriod' {_tpStart = Nothing, _tpEnd = Nothing}


-- | The start date for a budget. If you created your budget and didn't specify a start date, AWS defaults to the start of your chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you created your budget on January 24, 2018, chose @DAILY@ , and didn't set a start date, AWS set your start date to @01/24/18 00:00 UTC@ . If you chose @MONTHLY@ , AWS set your start date to @01/01/18 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. You can change your start date with the @UpdateBudget@ operation.
tpStart :: Lens' TimePeriod (Maybe UTCTime)
tpStart = lens _tpStart (\ s a -> s{_tpStart = a}) . mapping _Time

-- | The end date for a budget. If you didn't specify an end date, AWS set your end date to @06/15/87 00:00 UTC@ . The defaults are the same for the AWS Billing and Cost Management console and the API. After the end date, AWS deletes the budget and all associated notifications and subscribers. You can change your end date with the @UpdateBudget@ operation.
tpEnd :: Lens' TimePeriod (Maybe UTCTime)
tpEnd = lens _tpEnd (\ s a -> s{_tpEnd = a}) . mapping _Time

instance FromJSON TimePeriod where
        parseJSON
          = withObject "TimePeriod"
              (\ x ->
                 TimePeriod' <$> (x .:? "Start") <*> (x .:? "End"))

instance Hashable TimePeriod where

instance NFData TimePeriod where

instance ToJSON TimePeriod where
        toJSON TimePeriod'{..}
          = object
              (catMaybes
                 [("Start" .=) <$> _tpStart, ("End" .=) <$> _tpEnd])
