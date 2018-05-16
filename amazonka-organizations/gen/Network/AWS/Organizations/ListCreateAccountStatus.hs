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
-- Module      : Network.AWS.Organizations.ListCreateAccountStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account creation requests that match the specified status that is currently being tracked for the organization.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListCreateAccountStatus
    (
    -- * Creating a Request
      listCreateAccountStatus
    , ListCreateAccountStatus
    -- * Request Lenses
    , lcasStates
    , lcasNextToken
    , lcasMaxResults

    -- * Destructuring the Response
    , listCreateAccountStatusResponse
    , ListCreateAccountStatusResponse
    -- * Response Lenses
    , lcasrsCreateAccountStatuses
    , lcasrsNextToken
    , lcasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCreateAccountStatus' smart constructor.
data ListCreateAccountStatus = ListCreateAccountStatus'
  { _lcasStates     :: !(Maybe [CreateAccountState])
  , _lcasNextToken  :: !(Maybe Text)
  , _lcasMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCreateAccountStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcasStates' - A list of one or more states that you want included in the response. If this parameter is not present, then all requests are included in the response.
--
-- * 'lcasNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lcasMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listCreateAccountStatus
    :: ListCreateAccountStatus
listCreateAccountStatus =
  ListCreateAccountStatus'
    {_lcasStates = Nothing, _lcasNextToken = Nothing, _lcasMaxResults = Nothing}


-- | A list of one or more states that you want included in the response. If this parameter is not present, then all requests are included in the response.
lcasStates :: Lens' ListCreateAccountStatus [CreateAccountState]
lcasStates = lens _lcasStates (\ s a -> s{_lcasStates = a}) . _Default . _Coerce

-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lcasNextToken :: Lens' ListCreateAccountStatus (Maybe Text)
lcasNextToken = lens _lcasNextToken (\ s a -> s{_lcasNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lcasMaxResults :: Lens' ListCreateAccountStatus (Maybe Natural)
lcasMaxResults = lens _lcasMaxResults (\ s a -> s{_lcasMaxResults = a}) . mapping _Nat

instance AWSPager ListCreateAccountStatus where
        page rq rs
          | stop (rs ^. lcasrsNextToken) = Nothing
          | stop (rs ^. lcasrsCreateAccountStatuses) = Nothing
          | otherwise =
            Just $ rq & lcasNextToken .~ rs ^. lcasrsNextToken

instance AWSRequest ListCreateAccountStatus where
        type Rs ListCreateAccountStatus =
             ListCreateAccountStatusResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListCreateAccountStatusResponse' <$>
                   (x .?> "CreateAccountStatuses" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCreateAccountStatus where

instance NFData ListCreateAccountStatus where

instance ToHeaders ListCreateAccountStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListCreateAccountStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCreateAccountStatus where
        toJSON ListCreateAccountStatus'{..}
          = object
              (catMaybes
                 [("States" .=) <$> _lcasStates,
                  ("NextToken" .=) <$> _lcasNextToken,
                  ("MaxResults" .=) <$> _lcasMaxResults])

instance ToPath ListCreateAccountStatus where
        toPath = const "/"

instance ToQuery ListCreateAccountStatus where
        toQuery = const mempty

-- | /See:/ 'listCreateAccountStatusResponse' smart constructor.
data ListCreateAccountStatusResponse = ListCreateAccountStatusResponse'
  { _lcasrsCreateAccountStatuses :: !(Maybe [CreateAccountStatus])
  , _lcasrsNextToken             :: !(Maybe Text)
  , _lcasrsResponseStatus        :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCreateAccountStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcasrsCreateAccountStatuses' - A list of objects with details about the requests. Certain elements, such as the accountId number, are present in the output only after the account has been successfully created.
--
-- * 'lcasrsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lcasrsResponseStatus' - -- | The response status code.
listCreateAccountStatusResponse
    :: Int -- ^ 'lcasrsResponseStatus'
    -> ListCreateAccountStatusResponse
listCreateAccountStatusResponse pResponseStatus_ =
  ListCreateAccountStatusResponse'
    { _lcasrsCreateAccountStatuses = Nothing
    , _lcasrsNextToken = Nothing
    , _lcasrsResponseStatus = pResponseStatus_
    }


-- | A list of objects with details about the requests. Certain elements, such as the accountId number, are present in the output only after the account has been successfully created.
lcasrsCreateAccountStatuses :: Lens' ListCreateAccountStatusResponse [CreateAccountStatus]
lcasrsCreateAccountStatuses = lens _lcasrsCreateAccountStatuses (\ s a -> s{_lcasrsCreateAccountStatuses = a}) . _Default . _Coerce

-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lcasrsNextToken :: Lens' ListCreateAccountStatusResponse (Maybe Text)
lcasrsNextToken = lens _lcasrsNextToken (\ s a -> s{_lcasrsNextToken = a})

-- | -- | The response status code.
lcasrsResponseStatus :: Lens' ListCreateAccountStatusResponse Int
lcasrsResponseStatus = lens _lcasrsResponseStatus (\ s a -> s{_lcasrsResponseStatus = a})

instance NFData ListCreateAccountStatusResponse where
