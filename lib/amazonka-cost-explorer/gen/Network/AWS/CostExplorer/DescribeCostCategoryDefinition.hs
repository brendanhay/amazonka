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
-- Module      : Network.AWS.CostExplorer.DescribeCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, ARN, rules, definition, and effective dates of a Cost Category that's defined in the account.
--
--
-- You have the option to use @EffectiveOn@ to return a Cost Category that is active on a specific date. If there is no @EffectiveOn@ specified, you’ll see a Cost Category that is effective on the current date. If Cost Category is still effective, @EffectiveEnd@ is omitted in the response.
module Network.AWS.CostExplorer.DescribeCostCategoryDefinition
  ( -- * Creating a Request
    describeCostCategoryDefinition,
    DescribeCostCategoryDefinition,

    -- * Request Lenses
    dEffectiveOn,
    dCostCategoryARN,

    -- * Destructuring the Response
    describeCostCategoryDefinitionResponse,
    DescribeCostCategoryDefinitionResponse,

    -- * Response Lenses
    drsCostCategory,
    drsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCostCategoryDefinition' smart constructor.
data DescribeCostCategoryDefinition = DescribeCostCategoryDefinition'
  { _dEffectiveOn ::
      !(Maybe Text),
    _dCostCategoryARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCostCategoryDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dEffectiveOn' - The date when the Cost Category was effective.
--
-- * 'dCostCategoryARN' - The unique identifier for your Cost Category.
describeCostCategoryDefinition ::
  -- | 'dCostCategoryARN'
  Text ->
  DescribeCostCategoryDefinition
describeCostCategoryDefinition pCostCategoryARN_ =
  DescribeCostCategoryDefinition'
    { _dEffectiveOn = Nothing,
      _dCostCategoryARN = pCostCategoryARN_
    }

-- | The date when the Cost Category was effective.
dEffectiveOn :: Lens' DescribeCostCategoryDefinition (Maybe Text)
dEffectiveOn = lens _dEffectiveOn (\s a -> s {_dEffectiveOn = a})

-- | The unique identifier for your Cost Category.
dCostCategoryARN :: Lens' DescribeCostCategoryDefinition Text
dCostCategoryARN = lens _dCostCategoryARN (\s a -> s {_dCostCategoryARN = a})

instance AWSRequest DescribeCostCategoryDefinition where
  type
    Rs DescribeCostCategoryDefinition =
      DescribeCostCategoryDefinitionResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          DescribeCostCategoryDefinitionResponse'
            <$> (x .?> "CostCategory") <*> (pure (fromEnum s))
      )

instance Hashable DescribeCostCategoryDefinition

instance NFData DescribeCostCategoryDefinition

instance ToHeaders DescribeCostCategoryDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.DescribeCostCategoryDefinition" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeCostCategoryDefinition where
  toJSON DescribeCostCategoryDefinition' {..} =
    object
      ( catMaybes
          [ ("EffectiveOn" .=) <$> _dEffectiveOn,
            Just ("CostCategoryArn" .= _dCostCategoryARN)
          ]
      )

instance ToPath DescribeCostCategoryDefinition where
  toPath = const "/"

instance ToQuery DescribeCostCategoryDefinition where
  toQuery = const mempty

-- | /See:/ 'describeCostCategoryDefinitionResponse' smart constructor.
data DescribeCostCategoryDefinitionResponse = DescribeCostCategoryDefinitionResponse'
  { _drsCostCategory ::
      !( Maybe
           CostCategory
       ),
    _drsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCostCategoryDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCostCategory' - Undocumented member.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeCostCategoryDefinitionResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DescribeCostCategoryDefinitionResponse
describeCostCategoryDefinitionResponse pResponseStatus_ =
  DescribeCostCategoryDefinitionResponse'
    { _drsCostCategory =
        Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
drsCostCategory :: Lens' DescribeCostCategoryDefinitionResponse (Maybe CostCategory)
drsCostCategory = lens _drsCostCategory (\s a -> s {_drsCostCategory = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeCostCategoryDefinitionResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DescribeCostCategoryDefinitionResponse
