{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Cost Category. Changes made to the Cost Category rules will be used to categorize the current month’s expenses and future expenses. This won’t change categorization for the previous months.
module Network.AWS.CostExplorer.UpdateCostCategoryDefinition
  ( -- * Creating a Request
    updateCostCategoryDefinition,
    UpdateCostCategoryDefinition,

    -- * Request Lenses
    uccdCostCategoryARN,
    uccdRuleVersion,
    uccdRules,

    -- * Destructuring the Response
    updateCostCategoryDefinitionResponse,
    UpdateCostCategoryDefinitionResponse,

    -- * Response Lenses
    uccdrsEffectiveStart,
    uccdrsCostCategoryARN,
    uccdrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCostCategoryDefinition' smart constructor.
data UpdateCostCategoryDefinition = UpdateCostCategoryDefinition'
  { _uccdCostCategoryARN ::
      !Text,
    _uccdRuleVersion ::
      !CostCategoryRuleVersion,
    _uccdRules ::
      !(List1 CostCategoryRule)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCostCategoryDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccdCostCategoryARN' - The unique identifier for your Cost Category.
--
-- * 'uccdRuleVersion' - Undocumented member.
--
-- * 'uccdRules' - The @Expression@ object used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule > .
updateCostCategoryDefinition ::
  -- | 'uccdCostCategoryARN'
  Text ->
  -- | 'uccdRuleVersion'
  CostCategoryRuleVersion ->
  -- | 'uccdRules'
  NonEmpty CostCategoryRule ->
  UpdateCostCategoryDefinition
updateCostCategoryDefinition
  pCostCategoryARN_
  pRuleVersion_
  pRules_ =
    UpdateCostCategoryDefinition'
      { _uccdCostCategoryARN =
          pCostCategoryARN_,
        _uccdRuleVersion = pRuleVersion_,
        _uccdRules = _List1 # pRules_
      }

-- | The unique identifier for your Cost Category.
uccdCostCategoryARN :: Lens' UpdateCostCategoryDefinition Text
uccdCostCategoryARN = lens _uccdCostCategoryARN (\s a -> s {_uccdCostCategoryARN = a})

-- | Undocumented member.
uccdRuleVersion :: Lens' UpdateCostCategoryDefinition CostCategoryRuleVersion
uccdRuleVersion = lens _uccdRuleVersion (\s a -> s {_uccdRuleVersion = a})

-- | The @Expression@ object used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule > .
uccdRules :: Lens' UpdateCostCategoryDefinition (NonEmpty CostCategoryRule)
uccdRules = lens _uccdRules (\s a -> s {_uccdRules = a}) . _List1

instance AWSRequest UpdateCostCategoryDefinition where
  type
    Rs UpdateCostCategoryDefinition =
      UpdateCostCategoryDefinitionResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          UpdateCostCategoryDefinitionResponse'
            <$> (x .?> "EffectiveStart")
            <*> (x .?> "CostCategoryArn")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateCostCategoryDefinition

instance NFData UpdateCostCategoryDefinition

instance ToHeaders UpdateCostCategoryDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.UpdateCostCategoryDefinition" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateCostCategoryDefinition where
  toJSON UpdateCostCategoryDefinition' {..} =
    object
      ( catMaybes
          [ Just ("CostCategoryArn" .= _uccdCostCategoryARN),
            Just ("RuleVersion" .= _uccdRuleVersion),
            Just ("Rules" .= _uccdRules)
          ]
      )

instance ToPath UpdateCostCategoryDefinition where
  toPath = const "/"

instance ToQuery UpdateCostCategoryDefinition where
  toQuery = const mempty

-- | /See:/ 'updateCostCategoryDefinitionResponse' smart constructor.
data UpdateCostCategoryDefinitionResponse = UpdateCostCategoryDefinitionResponse'
  { _uccdrsEffectiveStart ::
      !(Maybe Text),
    _uccdrsCostCategoryARN ::
      !(Maybe Text),
    _uccdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCostCategoryDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccdrsEffectiveStart' - The Cost Category's effective start date.
--
-- * 'uccdrsCostCategoryARN' - The unique identifier for your Cost Category.
--
-- * 'uccdrsResponseStatus' - -- | The response status code.
updateCostCategoryDefinitionResponse ::
  -- | 'uccdrsResponseStatus'
  Int ->
  UpdateCostCategoryDefinitionResponse
updateCostCategoryDefinitionResponse pResponseStatus_ =
  UpdateCostCategoryDefinitionResponse'
    { _uccdrsEffectiveStart =
        Nothing,
      _uccdrsCostCategoryARN = Nothing,
      _uccdrsResponseStatus = pResponseStatus_
    }

-- | The Cost Category's effective start date.
uccdrsEffectiveStart :: Lens' UpdateCostCategoryDefinitionResponse (Maybe Text)
uccdrsEffectiveStart = lens _uccdrsEffectiveStart (\s a -> s {_uccdrsEffectiveStart = a})

-- | The unique identifier for your Cost Category.
uccdrsCostCategoryARN :: Lens' UpdateCostCategoryDefinitionResponse (Maybe Text)
uccdrsCostCategoryARN = lens _uccdrsCostCategoryARN (\s a -> s {_uccdrsCostCategoryARN = a})

-- | -- | The response status code.
uccdrsResponseStatus :: Lens' UpdateCostCategoryDefinitionResponse Int
uccdrsResponseStatus = lens _uccdrsResponseStatus (\s a -> s {_uccdrsResponseStatus = a})

instance NFData UpdateCostCategoryDefinitionResponse
