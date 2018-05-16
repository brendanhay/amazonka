{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateBudget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget. You can change every part of a budget except for the @budgetName@ and the @calculatedSpend@ . When a budget is modified, the @calculatedSpend@ drops to zero until AWS has new usage data to use for forecasting.
--
--
module Network.AWS.Budgets.UpdateBudget
    (
    -- * Creating a Request
      updateBudget
    , UpdateBudget
    -- * Request Lenses
    , ubAccountId
    , ubNewBudget

    -- * Destructuring the Response
    , updateBudgetResponse
    , UpdateBudgetResponse
    -- * Response Lenses
    , ubrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of UpdateBudget
--
--
--
-- /See:/ 'updateBudget' smart constructor.
data UpdateBudget = UpdateBudget'
  { _ubAccountId :: !Text
  , _ubNewBudget :: !Budget
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBudget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubAccountId' - The @accountId@ that is associated with the budget that you want to update.
--
-- * 'ubNewBudget' - The budget that you want to update your budget to.
updateBudget
    :: Text -- ^ 'ubAccountId'
    -> Budget -- ^ 'ubNewBudget'
    -> UpdateBudget
updateBudget pAccountId_ pNewBudget_ =
  UpdateBudget' {_ubAccountId = pAccountId_, _ubNewBudget = pNewBudget_}


-- | The @accountId@ that is associated with the budget that you want to update.
ubAccountId :: Lens' UpdateBudget Text
ubAccountId = lens _ubAccountId (\ s a -> s{_ubAccountId = a})

-- | The budget that you want to update your budget to.
ubNewBudget :: Lens' UpdateBudget Budget
ubNewBudget = lens _ubNewBudget (\ s a -> s{_ubNewBudget = a})

instance AWSRequest UpdateBudget where
        type Rs UpdateBudget = UpdateBudgetResponse
        request = postJSON budgets
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateBudgetResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateBudget where

instance NFData UpdateBudget where

instance ToHeaders UpdateBudget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.UpdateBudget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBudget where
        toJSON UpdateBudget'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _ubAccountId),
                  Just ("NewBudget" .= _ubNewBudget)])

instance ToPath UpdateBudget where
        toPath = const "/"

instance ToQuery UpdateBudget where
        toQuery = const mempty

-- | Response of UpdateBudget
--
--
--
-- /See:/ 'updateBudgetResponse' smart constructor.
newtype UpdateBudgetResponse = UpdateBudgetResponse'
  { _ubrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBudgetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrsResponseStatus' - -- | The response status code.
updateBudgetResponse
    :: Int -- ^ 'ubrsResponseStatus'
    -> UpdateBudgetResponse
updateBudgetResponse pResponseStatus_ =
  UpdateBudgetResponse' {_ubrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ubrsResponseStatus :: Lens' UpdateBudgetResponse Int
ubrsResponseStatus = lens _ubrsResponseStatus (\ s a -> s{_ubrsResponseStatus = a})

instance NFData UpdateBudgetResponse where
