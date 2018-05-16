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
-- Module      : Network.AWS.IoT.ListThingRegistrationTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List bulk thing provisioning tasks.
--
--
module Network.AWS.IoT.ListThingRegistrationTasks
    (
    -- * Creating a Request
      listThingRegistrationTasks
    , ListThingRegistrationTasks
    -- * Request Lenses
    , ltrtStatus
    , ltrtNextToken
    , ltrtMaxResults

    -- * Destructuring the Response
    , listThingRegistrationTasksResponse
    , ListThingRegistrationTasksResponse
    -- * Response Lenses
    , ltrtrsNextToken
    , ltrtrsTaskIds
    , ltrtrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThingRegistrationTasks' smart constructor.
data ListThingRegistrationTasks = ListThingRegistrationTasks'
  { _ltrtStatus     :: !(Maybe TaskStatus)
  , _ltrtNextToken  :: !(Maybe Text)
  , _ltrtMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingRegistrationTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrtStatus' - The status of the bulk thing provisioning task.
--
-- * 'ltrtNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltrtMaxResults' - The maximum number of results to return at one time.
listThingRegistrationTasks
    :: ListThingRegistrationTasks
listThingRegistrationTasks =
  ListThingRegistrationTasks'
    {_ltrtStatus = Nothing, _ltrtNextToken = Nothing, _ltrtMaxResults = Nothing}


-- | The status of the bulk thing provisioning task.
ltrtStatus :: Lens' ListThingRegistrationTasks (Maybe TaskStatus)
ltrtStatus = lens _ltrtStatus (\ s a -> s{_ltrtStatus = a})

-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltrtNextToken :: Lens' ListThingRegistrationTasks (Maybe Text)
ltrtNextToken = lens _ltrtNextToken (\ s a -> s{_ltrtNextToken = a})

-- | The maximum number of results to return at one time.
ltrtMaxResults :: Lens' ListThingRegistrationTasks (Maybe Natural)
ltrtMaxResults = lens _ltrtMaxResults (\ s a -> s{_ltrtMaxResults = a}) . mapping _Nat

instance AWSRequest ListThingRegistrationTasks where
        type Rs ListThingRegistrationTasks =
             ListThingRegistrationTasksResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListThingRegistrationTasksResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "taskIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListThingRegistrationTasks where

instance NFData ListThingRegistrationTasks where

instance ToHeaders ListThingRegistrationTasks where
        toHeaders = const mempty

instance ToPath ListThingRegistrationTasks where
        toPath = const "/thing-registration-tasks"

instance ToQuery ListThingRegistrationTasks where
        toQuery ListThingRegistrationTasks'{..}
          = mconcat
              ["status" =: _ltrtStatus,
               "nextToken" =: _ltrtNextToken,
               "maxResults" =: _ltrtMaxResults]

-- | /See:/ 'listThingRegistrationTasksResponse' smart constructor.
data ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse'
  { _ltrtrsNextToken      :: !(Maybe Text)
  , _ltrtrsTaskIds        :: !(Maybe [Text])
  , _ltrtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingRegistrationTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrtrsNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltrtrsTaskIds' - A list of bulk thing provisioning task IDs.
--
-- * 'ltrtrsResponseStatus' - -- | The response status code.
listThingRegistrationTasksResponse
    :: Int -- ^ 'ltrtrsResponseStatus'
    -> ListThingRegistrationTasksResponse
listThingRegistrationTasksResponse pResponseStatus_ =
  ListThingRegistrationTasksResponse'
    { _ltrtrsNextToken = Nothing
    , _ltrtrsTaskIds = Nothing
    , _ltrtrsResponseStatus = pResponseStatus_
    }


-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltrtrsNextToken :: Lens' ListThingRegistrationTasksResponse (Maybe Text)
ltrtrsNextToken = lens _ltrtrsNextToken (\ s a -> s{_ltrtrsNextToken = a})

-- | A list of bulk thing provisioning task IDs.
ltrtrsTaskIds :: Lens' ListThingRegistrationTasksResponse [Text]
ltrtrsTaskIds = lens _ltrtrsTaskIds (\ s a -> s{_ltrtrsTaskIds = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrtrsResponseStatus :: Lens' ListThingRegistrationTasksResponse Int
ltrtrsResponseStatus = lens _ltrtrsResponseStatus (\ s a -> s{_ltrtrsResponseStatus = a})

instance NFData ListThingRegistrationTasksResponse
         where
