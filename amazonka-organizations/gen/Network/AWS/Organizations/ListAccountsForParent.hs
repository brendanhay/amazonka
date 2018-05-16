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
-- Module      : Network.AWS.Organizations.ListAccountsForParent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts in an organization that are contained by the specified target root or organizational unit (OU). If you specify the root, you get a list of all the accounts that are not in any OU. If you specify an OU, you get a list of all the accounts in only that OU, and not in any child OUs. To get a list of all accounts in the organization, use the 'ListAccounts' operation.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListAccountsForParent
    (
    -- * Creating a Request
      listAccountsForParent
    , ListAccountsForParent
    -- * Request Lenses
    , lafpNextToken
    , lafpMaxResults
    , lafpParentId

    -- * Destructuring the Response
    , listAccountsForParentResponse
    , ListAccountsForParentResponse
    -- * Response Lenses
    , lafprsAccounts
    , lafprsNextToken
    , lafprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAccountsForParent' smart constructor.
data ListAccountsForParent = ListAccountsForParent'
  { _lafpNextToken  :: !(Maybe Text)
  , _lafpMaxResults :: !(Maybe Nat)
  , _lafpParentId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccountsForParent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lafpNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lafpMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'lafpParentId' - The unique identifier (ID) for the parent root or organization unit (OU) whose accounts you want to list.
listAccountsForParent
    :: Text -- ^ 'lafpParentId'
    -> ListAccountsForParent
listAccountsForParent pParentId_ =
  ListAccountsForParent'
    { _lafpNextToken = Nothing
    , _lafpMaxResults = Nothing
    , _lafpParentId = pParentId_
    }


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lafpNextToken :: Lens' ListAccountsForParent (Maybe Text)
lafpNextToken = lens _lafpNextToken (\ s a -> s{_lafpNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lafpMaxResults :: Lens' ListAccountsForParent (Maybe Natural)
lafpMaxResults = lens _lafpMaxResults (\ s a -> s{_lafpMaxResults = a}) . mapping _Nat

-- | The unique identifier (ID) for the parent root or organization unit (OU) whose accounts you want to list.
lafpParentId :: Lens' ListAccountsForParent Text
lafpParentId = lens _lafpParentId (\ s a -> s{_lafpParentId = a})

instance AWSPager ListAccountsForParent where
        page rq rs
          | stop (rs ^. lafprsNextToken) = Nothing
          | stop (rs ^. lafprsAccounts) = Nothing
          | otherwise =
            Just $ rq & lafpNextToken .~ rs ^. lafprsNextToken

instance AWSRequest ListAccountsForParent where
        type Rs ListAccountsForParent =
             ListAccountsForParentResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListAccountsForParentResponse' <$>
                   (x .?> "Accounts" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListAccountsForParent where

instance NFData ListAccountsForParent where

instance ToHeaders ListAccountsForParent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListAccountsForParent" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAccountsForParent where
        toJSON ListAccountsForParent'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lafpNextToken,
                  ("MaxResults" .=) <$> _lafpMaxResults,
                  Just ("ParentId" .= _lafpParentId)])

instance ToPath ListAccountsForParent where
        toPath = const "/"

instance ToQuery ListAccountsForParent where
        toQuery = const mempty

-- | /See:/ 'listAccountsForParentResponse' smart constructor.
data ListAccountsForParentResponse = ListAccountsForParentResponse'
  { _lafprsAccounts       :: !(Maybe [Account])
  , _lafprsNextToken      :: !(Maybe Text)
  , _lafprsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccountsForParentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lafprsAccounts' - A list of the accounts in the specified root or OU.
--
-- * 'lafprsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lafprsResponseStatus' - -- | The response status code.
listAccountsForParentResponse
    :: Int -- ^ 'lafprsResponseStatus'
    -> ListAccountsForParentResponse
listAccountsForParentResponse pResponseStatus_ =
  ListAccountsForParentResponse'
    { _lafprsAccounts = Nothing
    , _lafprsNextToken = Nothing
    , _lafprsResponseStatus = pResponseStatus_
    }


-- | A list of the accounts in the specified root or OU.
lafprsAccounts :: Lens' ListAccountsForParentResponse [Account]
lafprsAccounts = lens _lafprsAccounts (\ s a -> s{_lafprsAccounts = a}) . _Default . _Coerce

-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lafprsNextToken :: Lens' ListAccountsForParentResponse (Maybe Text)
lafprsNextToken = lens _lafprsNextToken (\ s a -> s{_lafprsNextToken = a})

-- | -- | The response status code.
lafprsResponseStatus :: Lens' ListAccountsForParentResponse Int
lafprsResponseStatus = lens _lafprsResponseStatus (\ s a -> s{_lafprsResponseStatus = a})

instance NFData ListAccountsForParentResponse where
