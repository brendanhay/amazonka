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
-- Module      : Network.AWS.DirectoryService.ListLogSubscriptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active log subscriptions for the AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListLogSubscriptions
    (
    -- * Creating a Request
      listLogSubscriptions
    , ListLogSubscriptions
    -- * Request Lenses
    , llsDirectoryId
    , llsNextToken
    , llsLimit

    -- * Destructuring the Response
    , listLogSubscriptionsResponse
    , ListLogSubscriptionsResponse
    -- * Response Lenses
    , llsrsNextToken
    , llsrsLogSubscriptions
    , llsrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLogSubscriptions' smart constructor.
data ListLogSubscriptions = ListLogSubscriptions'
  { _llsDirectoryId :: !(Maybe Text)
  , _llsNextToken   :: !(Maybe Text)
  , _llsLimit       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLogSubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llsDirectoryId' - If a /DirectoryID/ is provided, lists only the log subscription associated with that directory. If no /DirectoryId/ is provided, lists all log subscriptions associated with your AWS account. If there are no log subscriptions for the AWS account or the directory, an empty list will be returned.
--
-- * 'llsNextToken' - The token for the next set of items to return.
--
-- * 'llsLimit' - The maximum number of items returned.
listLogSubscriptions
    :: ListLogSubscriptions
listLogSubscriptions =
  ListLogSubscriptions'
    {_llsDirectoryId = Nothing, _llsNextToken = Nothing, _llsLimit = Nothing}


-- | If a /DirectoryID/ is provided, lists only the log subscription associated with that directory. If no /DirectoryId/ is provided, lists all log subscriptions associated with your AWS account. If there are no log subscriptions for the AWS account or the directory, an empty list will be returned.
llsDirectoryId :: Lens' ListLogSubscriptions (Maybe Text)
llsDirectoryId = lens _llsDirectoryId (\ s a -> s{_llsDirectoryId = a})

-- | The token for the next set of items to return.
llsNextToken :: Lens' ListLogSubscriptions (Maybe Text)
llsNextToken = lens _llsNextToken (\ s a -> s{_llsNextToken = a})

-- | The maximum number of items returned.
llsLimit :: Lens' ListLogSubscriptions (Maybe Natural)
llsLimit = lens _llsLimit (\ s a -> s{_llsLimit = a}) . mapping _Nat

instance AWSPager ListLogSubscriptions where
        page rq rs
          | stop (rs ^. llsrsNextToken) = Nothing
          | stop (rs ^. llsrsLogSubscriptions) = Nothing
          | otherwise =
            Just $ rq & llsNextToken .~ rs ^. llsrsNextToken

instance AWSRequest ListLogSubscriptions where
        type Rs ListLogSubscriptions =
             ListLogSubscriptionsResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 ListLogSubscriptionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "LogSubscriptions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListLogSubscriptions where

instance NFData ListLogSubscriptions where

instance ToHeaders ListLogSubscriptions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.ListLogSubscriptions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListLogSubscriptions where
        toJSON ListLogSubscriptions'{..}
          = object
              (catMaybes
                 [("DirectoryId" .=) <$> _llsDirectoryId,
                  ("NextToken" .=) <$> _llsNextToken,
                  ("Limit" .=) <$> _llsLimit])

instance ToPath ListLogSubscriptions where
        toPath = const "/"

instance ToQuery ListLogSubscriptions where
        toQuery = const mempty

-- | /See:/ 'listLogSubscriptionsResponse' smart constructor.
data ListLogSubscriptionsResponse = ListLogSubscriptionsResponse'
  { _llsrsNextToken        :: !(Maybe Text)
  , _llsrsLogSubscriptions :: !(Maybe [LogSubscription])
  , _llsrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLogSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llsrsNextToken' - The token for the next set of items to return.
--
-- * 'llsrsLogSubscriptions' - A list of active 'LogSubscription' objects for calling the AWS account.
--
-- * 'llsrsResponseStatus' - -- | The response status code.
listLogSubscriptionsResponse
    :: Int -- ^ 'llsrsResponseStatus'
    -> ListLogSubscriptionsResponse
listLogSubscriptionsResponse pResponseStatus_ =
  ListLogSubscriptionsResponse'
    { _llsrsNextToken = Nothing
    , _llsrsLogSubscriptions = Nothing
    , _llsrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of items to return.
llsrsNextToken :: Lens' ListLogSubscriptionsResponse (Maybe Text)
llsrsNextToken = lens _llsrsNextToken (\ s a -> s{_llsrsNextToken = a})

-- | A list of active 'LogSubscription' objects for calling the AWS account.
llsrsLogSubscriptions :: Lens' ListLogSubscriptionsResponse [LogSubscription]
llsrsLogSubscriptions = lens _llsrsLogSubscriptions (\ s a -> s{_llsrsLogSubscriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
llsrsResponseStatus :: Lens' ListLogSubscriptionsResponse Int
llsrsResponseStatus = lens _llsrsResponseStatus (\ s a -> s{_llsrsResponseStatus = a})

instance NFData ListLogSubscriptionsResponse where
