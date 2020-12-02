{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryRule where

import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Rules are processed in order. If there are multiple rules that match the line item, then the first rule to match is used to determine that Cost Category value.
--
--
--
-- /See:/ 'costCategoryRule' smart constructor.
data CostCategoryRule = CostCategoryRule'
  { _ccrValue :: !Text,
    _ccrRule :: !Expression
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CostCategoryRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrValue' - Undocumented member.
--
-- * 'ccrRule' - An <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object used to categorize costs. This supports dimensions, tags, and nested expressions. Currently the only dimensions supported are @LINKED_ACCOUNT@ , @SERVICE_CODE@ , @RECORD_TYPE@ , and @LINKED_ACCOUNT_NAME@ . Root level @OR@ is not supported. We recommend that you create a separate rule instead. @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also supported for Cost Category expressions. This dimension uses different terms, depending on whether you're using the console or API/JSON editor. For a detailed comparison, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons> in the /AWS Billing and Cost Management User Guide/ .
costCategoryRule ::
  -- | 'ccrValue'
  Text ->
  -- | 'ccrRule'
  Expression ->
  CostCategoryRule
costCategoryRule pValue_ pRule_ =
  CostCategoryRule' {_ccrValue = pValue_, _ccrRule = pRule_}

-- | Undocumented member.
ccrValue :: Lens' CostCategoryRule Text
ccrValue = lens _ccrValue (\s a -> s {_ccrValue = a})

-- | An <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object used to categorize costs. This supports dimensions, tags, and nested expressions. Currently the only dimensions supported are @LINKED_ACCOUNT@ , @SERVICE_CODE@ , @RECORD_TYPE@ , and @LINKED_ACCOUNT_NAME@ . Root level @OR@ is not supported. We recommend that you create a separate rule instead. @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also supported for Cost Category expressions. This dimension uses different terms, depending on whether you're using the console or API/JSON editor. For a detailed comparison, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons> in the /AWS Billing and Cost Management User Guide/ .
ccrRule :: Lens' CostCategoryRule Expression
ccrRule = lens _ccrRule (\s a -> s {_ccrRule = a})

instance FromJSON CostCategoryRule where
  parseJSON =
    withObject
      "CostCategoryRule"
      (\x -> CostCategoryRule' <$> (x .: "Value") <*> (x .: "Rule"))

instance Hashable CostCategoryRule

instance NFData CostCategoryRule

instance ToJSON CostCategoryRule where
  toJSON CostCategoryRule' {..} =
    object
      ( catMaybes
          [Just ("Value" .= _ccrValue), Just ("Rule" .= _ccrRule)]
      )
