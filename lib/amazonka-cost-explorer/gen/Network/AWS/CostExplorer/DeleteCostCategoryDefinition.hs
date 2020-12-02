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
-- Module      : Network.AWS.CostExplorer.DeleteCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Cost Category. Expenses from this month going forward will no longer be categorized with this Cost Category.
module Network.AWS.CostExplorer.DeleteCostCategoryDefinition
  ( -- * Creating a Request
    deleteCostCategoryDefinition,
    DeleteCostCategoryDefinition,

    -- * Request Lenses
    dccdCostCategoryARN,

    -- * Destructuring the Response
    deleteCostCategoryDefinitionResponse,
    DeleteCostCategoryDefinitionResponse,

    -- * Response Lenses
    dccdrsCostCategoryARN,
    dccdrsEffectiveEnd,
    dccdrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCostCategoryDefinition' smart constructor.
newtype DeleteCostCategoryDefinition = DeleteCostCategoryDefinition'
  { _dccdCostCategoryARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCostCategoryDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccdCostCategoryARN' - The unique identifier for your Cost Category.
deleteCostCategoryDefinition ::
  -- | 'dccdCostCategoryARN'
  Text ->
  DeleteCostCategoryDefinition
deleteCostCategoryDefinition pCostCategoryARN_ =
  DeleteCostCategoryDefinition'
    { _dccdCostCategoryARN =
        pCostCategoryARN_
    }

-- | The unique identifier for your Cost Category.
dccdCostCategoryARN :: Lens' DeleteCostCategoryDefinition Text
dccdCostCategoryARN = lens _dccdCostCategoryARN (\s a -> s {_dccdCostCategoryARN = a})

instance AWSRequest DeleteCostCategoryDefinition where
  type
    Rs DeleteCostCategoryDefinition =
      DeleteCostCategoryDefinitionResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          DeleteCostCategoryDefinitionResponse'
            <$> (x .?> "CostCategoryArn")
            <*> (x .?> "EffectiveEnd")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteCostCategoryDefinition

instance NFData DeleteCostCategoryDefinition

instance ToHeaders DeleteCostCategoryDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.DeleteCostCategoryDefinition" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteCostCategoryDefinition where
  toJSON DeleteCostCategoryDefinition' {..} =
    object
      (catMaybes [Just ("CostCategoryArn" .= _dccdCostCategoryARN)])

instance ToPath DeleteCostCategoryDefinition where
  toPath = const "/"

instance ToQuery DeleteCostCategoryDefinition where
  toQuery = const mempty

-- | /See:/ 'deleteCostCategoryDefinitionResponse' smart constructor.
data DeleteCostCategoryDefinitionResponse = DeleteCostCategoryDefinitionResponse'
  { _dccdrsCostCategoryARN ::
      !(Maybe Text),
    _dccdrsEffectiveEnd ::
      !(Maybe Text),
    _dccdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCostCategoryDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccdrsCostCategoryARN' - The unique identifier for your Cost Category.
--
-- * 'dccdrsEffectiveEnd' - The effective end date of the Cost Category as a result of deleting it. No costs after this date will be categorized by the deleted Cost Category.
--
-- * 'dccdrsResponseStatus' - -- | The response status code.
deleteCostCategoryDefinitionResponse ::
  -- | 'dccdrsResponseStatus'
  Int ->
  DeleteCostCategoryDefinitionResponse
deleteCostCategoryDefinitionResponse pResponseStatus_ =
  DeleteCostCategoryDefinitionResponse'
    { _dccdrsCostCategoryARN =
        Nothing,
      _dccdrsEffectiveEnd = Nothing,
      _dccdrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for your Cost Category.
dccdrsCostCategoryARN :: Lens' DeleteCostCategoryDefinitionResponse (Maybe Text)
dccdrsCostCategoryARN = lens _dccdrsCostCategoryARN (\s a -> s {_dccdrsCostCategoryARN = a})

-- | The effective end date of the Cost Category as a result of deleting it. No costs after this date will be categorized by the deleted Cost Category.
dccdrsEffectiveEnd :: Lens' DeleteCostCategoryDefinitionResponse (Maybe Text)
dccdrsEffectiveEnd = lens _dccdrsEffectiveEnd (\s a -> s {_dccdrsEffectiveEnd = a})

-- | -- | The response status code.
dccdrsResponseStatus :: Lens' DeleteCostCategoryDefinitionResponse Int
dccdrsResponseStatus = lens _dccdrsResponseStatus (\s a -> s {_dccdrsResponseStatus = a})

instance NFData DeleteCostCategoryDefinitionResponse
