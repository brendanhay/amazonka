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
-- Module      : Network.AWS.SSM.ListCommands
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the commands requested by users of the AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommands
    (
    -- * Creating a Request
      listCommands
    , ListCommands
    -- * Request Lenses
    , lcInstanceId
    , lcFilters
    , lcNextToken
    , lcCommandId
    , lcMaxResults

    -- * Destructuring the Response
    , listCommandsResponse
    , ListCommandsResponse
    -- * Response Lenses
    , lcrsCommands
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listCommands' smart constructor.
data ListCommands = ListCommands'
  { _lcInstanceId :: !(Maybe Text)
  , _lcFilters    :: !(Maybe (List1 CommandFilter))
  , _lcNextToken  :: !(Maybe Text)
  , _lcCommandId  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCommands' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcInstanceId' - (Optional) Lists commands issued against this instance ID.
--
-- * 'lcFilters' - (Optional) One or more filters. Use a filter to return a more specific list of results.
--
-- * 'lcNextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'lcCommandId' - (Optional) If provided, lists only the specified command.
--
-- * 'lcMaxResults' - (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
listCommands
    :: ListCommands
listCommands =
  ListCommands'
    { _lcInstanceId = Nothing
    , _lcFilters = Nothing
    , _lcNextToken = Nothing
    , _lcCommandId = Nothing
    , _lcMaxResults = Nothing
    }


-- | (Optional) Lists commands issued against this instance ID.
lcInstanceId :: Lens' ListCommands (Maybe Text)
lcInstanceId = lens _lcInstanceId (\ s a -> s{_lcInstanceId = a})

-- | (Optional) One or more filters. Use a filter to return a more specific list of results.
lcFilters :: Lens' ListCommands (Maybe (NonEmpty CommandFilter))
lcFilters = lens _lcFilters (\ s a -> s{_lcFilters = a}) . mapping _List1

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
lcNextToken :: Lens' ListCommands (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | (Optional) If provided, lists only the specified command.
lcCommandId :: Lens' ListCommands (Maybe Text)
lcCommandId = lens _lcCommandId (\ s a -> s{_lcCommandId = a})

-- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lcMaxResults :: Lens' ListCommands (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSPager ListCommands where
        page rq rs
          | stop (rs ^. lcrsNextToken) = Nothing
          | stop (rs ^. lcrsCommands) = Nothing
          | otherwise =
            Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListCommands where
        type Rs ListCommands = ListCommandsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListCommandsResponse' <$>
                   (x .?> "Commands" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCommands where

instance NFData ListCommands where

instance ToHeaders ListCommands where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListCommands" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCommands where
        toJSON ListCommands'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _lcInstanceId,
                  ("Filters" .=) <$> _lcFilters,
                  ("NextToken" .=) <$> _lcNextToken,
                  ("CommandId" .=) <$> _lcCommandId,
                  ("MaxResults" .=) <$> _lcMaxResults])

instance ToPath ListCommands where
        toPath = const "/"

instance ToQuery ListCommands where
        toQuery = const mempty

-- | /See:/ 'listCommandsResponse' smart constructor.
data ListCommandsResponse = ListCommandsResponse'
  { _lcrsCommands       :: !(Maybe [Command])
  , _lcrsNextToken      :: !(Maybe Text)
  , _lcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCommandsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsCommands' - (Optional) The list of commands requested by the user.
--
-- * 'lcrsNextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listCommandsResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListCommandsResponse
listCommandsResponse pResponseStatus_ =
  ListCommandsResponse'
    { _lcrsCommands = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | (Optional) The list of commands requested by the user.
lcrsCommands :: Lens' ListCommandsResponse [Command]
lcrsCommands = lens _lcrsCommands (\ s a -> s{_lcrsCommands = a}) . _Default . _Coerce

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
lcrsNextToken :: Lens' ListCommandsResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListCommandsResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListCommandsResponse where
