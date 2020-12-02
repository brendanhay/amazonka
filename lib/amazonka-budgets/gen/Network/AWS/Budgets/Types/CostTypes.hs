{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.CostTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.CostTypes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The types of cost that are included in a @COST@ budget, such as tax and subscriptions.
--
--
-- @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
--
--
-- /See:/ 'costTypes' smart constructor.
data CostTypes = CostTypes'
  { _ctUseAmortized :: !(Maybe Bool),
    _ctIncludeRecurring :: !(Maybe Bool),
    _ctUseBlended :: !(Maybe Bool),
    _ctIncludeSupport :: !(Maybe Bool),
    _ctIncludeDiscount :: !(Maybe Bool),
    _ctIncludeSubscription :: !(Maybe Bool),
    _ctIncludeRefund :: !(Maybe Bool),
    _ctIncludeUpfront :: !(Maybe Bool),
    _ctIncludeOtherSubscription :: !(Maybe Bool),
    _ctIncludeTax :: !(Maybe Bool),
    _ctIncludeCredit :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
costTypes ::
  CostTypes
costTypes =
  CostTypes'
    { _ctUseAmortized = Nothing,
      _ctIncludeRecurring = Nothing,
      _ctUseBlended = Nothing,
      _ctIncludeSupport = Nothing,
      _ctIncludeDiscount = Nothing,
      _ctIncludeSubscription = Nothing,
      _ctIncludeRefund = Nothing,
      _ctIncludeUpfront = Nothing,
      _ctIncludeOtherSubscription = Nothing,
      _ctIncludeTax = Nothing,
      _ctIncludeCredit = Nothing
    }

-- | Specifies whether a budget uses the amortized rate. The default value is @false@ .
ctUseAmortized :: Lens' CostTypes (Maybe Bool)
ctUseAmortized = lens _ctUseAmortized (\s a -> s {_ctUseAmortized = a})

-- | Specifies whether a budget includes recurring fees such as monthly RI fees. The default value is @true@ .
ctIncludeRecurring :: Lens' CostTypes (Maybe Bool)
ctIncludeRecurring = lens _ctIncludeRecurring (\s a -> s {_ctIncludeRecurring = a})

-- | Specifies whether a budget uses a blended rate. The default value is @false@ .
ctUseBlended :: Lens' CostTypes (Maybe Bool)
ctUseBlended = lens _ctUseBlended (\s a -> s {_ctUseBlended = a})

-- | Specifies whether a budget includes support subscription fees. The default value is @true@ .
ctIncludeSupport :: Lens' CostTypes (Maybe Bool)
ctIncludeSupport = lens _ctIncludeSupport (\s a -> s {_ctIncludeSupport = a})

-- | Specifies whether a budget includes discounts. The default value is @true@ .
ctIncludeDiscount :: Lens' CostTypes (Maybe Bool)
ctIncludeDiscount = lens _ctIncludeDiscount (\s a -> s {_ctIncludeDiscount = a})

-- | Specifies whether a budget includes subscriptions. The default value is @true@ .
ctIncludeSubscription :: Lens' CostTypes (Maybe Bool)
ctIncludeSubscription = lens _ctIncludeSubscription (\s a -> s {_ctIncludeSubscription = a})

-- | Specifies whether a budget includes refunds. The default value is @true@ .
ctIncludeRefund :: Lens' CostTypes (Maybe Bool)
ctIncludeRefund = lens _ctIncludeRefund (\s a -> s {_ctIncludeRefund = a})

-- | Specifies whether a budget includes upfront RI costs. The default value is @true@ .
ctIncludeUpfront :: Lens' CostTypes (Maybe Bool)
ctIncludeUpfront = lens _ctIncludeUpfront (\s a -> s {_ctIncludeUpfront = a})

-- | Specifies whether a budget includes non-RI subscription costs. The default value is @true@ .
ctIncludeOtherSubscription :: Lens' CostTypes (Maybe Bool)
ctIncludeOtherSubscription = lens _ctIncludeOtherSubscription (\s a -> s {_ctIncludeOtherSubscription = a})

-- | Specifies whether a budget includes taxes. The default value is @true@ .
ctIncludeTax :: Lens' CostTypes (Maybe Bool)
ctIncludeTax = lens _ctIncludeTax (\s a -> s {_ctIncludeTax = a})

-- | Specifies whether a budget includes credits. The default value is @true@ .
ctIncludeCredit :: Lens' CostTypes (Maybe Bool)
ctIncludeCredit = lens _ctIncludeCredit (\s a -> s {_ctIncludeCredit = a})

instance FromJSON CostTypes where
  parseJSON =
    withObject
      "CostTypes"
      ( \x ->
          CostTypes'
            <$> (x .:? "UseAmortized")
            <*> (x .:? "IncludeRecurring")
            <*> (x .:? "UseBlended")
            <*> (x .:? "IncludeSupport")
            <*> (x .:? "IncludeDiscount")
            <*> (x .:? "IncludeSubscription")
            <*> (x .:? "IncludeRefund")
            <*> (x .:? "IncludeUpfront")
            <*> (x .:? "IncludeOtherSubscription")
            <*> (x .:? "IncludeTax")
            <*> (x .:? "IncludeCredit")
      )

instance Hashable CostTypes

instance NFData CostTypes

instance ToJSON CostTypes where
  toJSON CostTypes' {..} =
    object
      ( catMaybes
          [ ("UseAmortized" .=) <$> _ctUseAmortized,
            ("IncludeRecurring" .=) <$> _ctIncludeRecurring,
            ("UseBlended" .=) <$> _ctUseBlended,
            ("IncludeSupport" .=) <$> _ctIncludeSupport,
            ("IncludeDiscount" .=) <$> _ctIncludeDiscount,
            ("IncludeSubscription" .=) <$> _ctIncludeSubscription,
            ("IncludeRefund" .=) <$> _ctIncludeRefund,
            ("IncludeUpfront" .=) <$> _ctIncludeUpfront,
            ("IncludeOtherSubscription" .=) <$> _ctIncludeOtherSubscription,
            ("IncludeTax" .=) <$> _ctIncludeTax,
            ("IncludeCredit" .=) <$> _ctIncludeCredit
          ]
      )
