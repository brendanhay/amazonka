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
-- Module      : Network.AWS.Budgets.DescribeBudget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget.
--
--
module Network.AWS.Budgets.DescribeBudget
    (
    -- * Creating a Request
      describeBudget
    , DescribeBudget
    -- * Request Lenses
    , desAccountId
    , desBudgetName

    -- * Destructuring the Response
    , describeBudgetResponse
    , DescribeBudgetResponse
    -- * Response Lenses
    , desrsBudget
    , desrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of DescribeBudget
--
--
--
-- /See:/ 'describeBudget' smart constructor.
data DescribeBudget = DescribeBudget'
  { _desAccountId  :: !Text
  , _desBudgetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBudget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desAccountId' - The @accountId@ that is associated with the budget that you want a description of.
--
-- * 'desBudgetName' - The name of the budget that you want a description of.
describeBudget
    :: Text -- ^ 'desAccountId'
    -> Text -- ^ 'desBudgetName'
    -> DescribeBudget
describeBudget pAccountId_ pBudgetName_ =
  DescribeBudget' {_desAccountId = pAccountId_, _desBudgetName = pBudgetName_}


-- | The @accountId@ that is associated with the budget that you want a description of.
desAccountId :: Lens' DescribeBudget Text
desAccountId = lens _desAccountId (\ s a -> s{_desAccountId = a})

-- | The name of the budget that you want a description of.
desBudgetName :: Lens' DescribeBudget Text
desBudgetName = lens _desBudgetName (\ s a -> s{_desBudgetName = a})

instance AWSRequest DescribeBudget where
        type Rs DescribeBudget = DescribeBudgetResponse
        request = postJSON budgets
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBudgetResponse' <$>
                   (x .?> "Budget") <*> (pure (fromEnum s)))

instance Hashable DescribeBudget where

instance NFData DescribeBudget where

instance ToHeaders DescribeBudget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.DescribeBudget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBudget where
        toJSON DescribeBudget'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _desAccountId),
                  Just ("BudgetName" .= _desBudgetName)])

instance ToPath DescribeBudget where
        toPath = const "/"

instance ToQuery DescribeBudget where
        toQuery = const mempty

-- | Response of DescribeBudget
--
--
--
-- /See:/ 'describeBudgetResponse' smart constructor.
data DescribeBudgetResponse = DescribeBudgetResponse'
  { _desrsBudget         :: !(Maybe Budget)
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBudgetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsBudget' - The description of the budget.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeBudgetResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeBudgetResponse
describeBudgetResponse pResponseStatus_ =
  DescribeBudgetResponse'
    {_desrsBudget = Nothing, _desrsResponseStatus = pResponseStatus_}


-- | The description of the budget.
desrsBudget :: Lens' DescribeBudgetResponse (Maybe Budget)
desrsBudget = lens _desrsBudget (\ s a -> s{_desrsBudget = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeBudgetResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeBudgetResponse where
