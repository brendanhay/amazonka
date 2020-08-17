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
-- Module      : Network.AWS.CloudWatchEvents.ListEventSources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this to see all the partner event sources that have been shared with your AWS account. For more information about partner event sources, see 'CreateEventBus' .
--
--
module Network.AWS.CloudWatchEvents.ListEventSources
    (
    -- * Creating a Request
      listEventSources
    , ListEventSources
    -- * Request Lenses
    , lesNextToken
    , lesNamePrefix
    , lesLimit

    -- * Destructuring the Response
    , listEventSourcesResponse
    , ListEventSourcesResponse
    -- * Response Lenses
    , lesrsNextToken
    , lesrsEventSources
    , lesrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEventSources' smart constructor.
data ListEventSources = ListEventSources'
  { _lesNextToken  :: !(Maybe Text)
  , _lesNamePrefix :: !(Maybe Text)
  , _lesLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEventSources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'lesNamePrefix' - Specifying this limits the results to only those partner event sources with names that start with the specified prefix.
--
-- * 'lesLimit' - Specifying this limits the number of results returned by this operation. The operation also returns a @NextToken@ that you can use in a subsequent operation to retrieve the next set of results.
listEventSources
    :: ListEventSources
listEventSources =
  ListEventSources'
    {_lesNextToken = Nothing, _lesNamePrefix = Nothing, _lesLimit = Nothing}


-- | The token returned by a previous call to retrieve the next set of results.
lesNextToken :: Lens' ListEventSources (Maybe Text)
lesNextToken = lens _lesNextToken (\ s a -> s{_lesNextToken = a})

-- | Specifying this limits the results to only those partner event sources with names that start with the specified prefix.
lesNamePrefix :: Lens' ListEventSources (Maybe Text)
lesNamePrefix = lens _lesNamePrefix (\ s a -> s{_lesNamePrefix = a})

-- | Specifying this limits the number of results returned by this operation. The operation also returns a @NextToken@ that you can use in a subsequent operation to retrieve the next set of results.
lesLimit :: Lens' ListEventSources (Maybe Natural)
lesLimit = lens _lesLimit (\ s a -> s{_lesLimit = a}) . mapping _Nat

instance AWSRequest ListEventSources where
        type Rs ListEventSources = ListEventSourcesResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 ListEventSourcesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "EventSources" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListEventSources where

instance NFData ListEventSources where

instance ToHeaders ListEventSources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.ListEventSources" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListEventSources where
        toJSON ListEventSources'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lesNextToken,
                  ("NamePrefix" .=) <$> _lesNamePrefix,
                  ("Limit" .=) <$> _lesLimit])

instance ToPath ListEventSources where
        toPath = const "/"

instance ToQuery ListEventSources where
        toQuery = const mempty

-- | /See:/ 'listEventSourcesResponse' smart constructor.
data ListEventSourcesResponse = ListEventSourcesResponse'
  { _lesrsNextToken      :: !(Maybe Text)
  , _lesrsEventSources   :: !(Maybe [EventSource])
  , _lesrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEventSourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesrsNextToken' - A token you can use in a subsequent operation to retrieve the next set of results.
--
-- * 'lesrsEventSources' - The list of event sources.
--
-- * 'lesrsResponseStatus' - -- | The response status code.
listEventSourcesResponse
    :: Int -- ^ 'lesrsResponseStatus'
    -> ListEventSourcesResponse
listEventSourcesResponse pResponseStatus_ =
  ListEventSourcesResponse'
    { _lesrsNextToken = Nothing
    , _lesrsEventSources = Nothing
    , _lesrsResponseStatus = pResponseStatus_
    }


-- | A token you can use in a subsequent operation to retrieve the next set of results.
lesrsNextToken :: Lens' ListEventSourcesResponse (Maybe Text)
lesrsNextToken = lens _lesrsNextToken (\ s a -> s{_lesrsNextToken = a})

-- | The list of event sources.
lesrsEventSources :: Lens' ListEventSourcesResponse [EventSource]
lesrsEventSources = lens _lesrsEventSources (\ s a -> s{_lesrsEventSources = a}) . _Default . _Coerce

-- | -- | The response status code.
lesrsResponseStatus :: Lens' ListEventSourcesResponse Int
lesrsResponseStatus = lens _lesrsResponseStatus (\ s a -> s{_lesrsResponseStatus = a})

instance NFData ListEventSourcesResponse where
