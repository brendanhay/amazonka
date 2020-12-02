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
-- Module      : Network.AWS.IoT.ListThingGroupsForThing
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups to which the specified thing belongs.
--
--
module Network.AWS.IoT.ListThingGroupsForThing
    (
    -- * Creating a Request
      listThingGroupsForThing
    , ListThingGroupsForThing
    -- * Request Lenses
    , ltgftNextToken
    , ltgftMaxResults
    , ltgftThingName

    -- * Destructuring the Response
    , listThingGroupsForThingResponse
    , ListThingGroupsForThingResponse
    -- * Response Lenses
    , ltgftrsThingGroups
    , ltgftrsNextToken
    , ltgftrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThingGroupsForThing' smart constructor.
data ListThingGroupsForThing = ListThingGroupsForThing'
  { _ltgftNextToken  :: !(Maybe Text)
  , _ltgftMaxResults :: !(Maybe Nat)
  , _ltgftThingName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingGroupsForThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgftNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltgftMaxResults' - The maximum number of results to return at one time.
--
-- * 'ltgftThingName' - The thing name.
listThingGroupsForThing
    :: Text -- ^ 'ltgftThingName'
    -> ListThingGroupsForThing
listThingGroupsForThing pThingName_ =
  ListThingGroupsForThing'
    { _ltgftNextToken = Nothing
    , _ltgftMaxResults = Nothing
    , _ltgftThingName = pThingName_
    }


-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltgftNextToken :: Lens' ListThingGroupsForThing (Maybe Text)
ltgftNextToken = lens _ltgftNextToken (\ s a -> s{_ltgftNextToken = a})

-- | The maximum number of results to return at one time.
ltgftMaxResults :: Lens' ListThingGroupsForThing (Maybe Natural)
ltgftMaxResults = lens _ltgftMaxResults (\ s a -> s{_ltgftMaxResults = a}) . mapping _Nat

-- | The thing name.
ltgftThingName :: Lens' ListThingGroupsForThing Text
ltgftThingName = lens _ltgftThingName (\ s a -> s{_ltgftThingName = a})

instance AWSRequest ListThingGroupsForThing where
        type Rs ListThingGroupsForThing =
             ListThingGroupsForThingResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListThingGroupsForThingResponse' <$>
                   (x .?> "thingGroups" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListThingGroupsForThing where

instance NFData ListThingGroupsForThing where

instance ToHeaders ListThingGroupsForThing where
        toHeaders = const mempty

instance ToPath ListThingGroupsForThing where
        toPath ListThingGroupsForThing'{..}
          = mconcat
              ["/things/", toBS _ltgftThingName, "/thing-groups"]

instance ToQuery ListThingGroupsForThing where
        toQuery ListThingGroupsForThing'{..}
          = mconcat
              ["nextToken" =: _ltgftNextToken,
               "maxResults" =: _ltgftMaxResults]

-- | /See:/ 'listThingGroupsForThingResponse' smart constructor.
data ListThingGroupsForThingResponse = ListThingGroupsForThingResponse'
  { _ltgftrsThingGroups    :: !(Maybe [GroupNameAndARN])
  , _ltgftrsNextToken      :: !(Maybe Text)
  , _ltgftrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingGroupsForThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgftrsThingGroups' - The thing groups.
--
-- * 'ltgftrsNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltgftrsResponseStatus' - -- | The response status code.
listThingGroupsForThingResponse
    :: Int -- ^ 'ltgftrsResponseStatus'
    -> ListThingGroupsForThingResponse
listThingGroupsForThingResponse pResponseStatus_ =
  ListThingGroupsForThingResponse'
    { _ltgftrsThingGroups = Nothing
    , _ltgftrsNextToken = Nothing
    , _ltgftrsResponseStatus = pResponseStatus_
    }


-- | The thing groups.
ltgftrsThingGroups :: Lens' ListThingGroupsForThingResponse [GroupNameAndARN]
ltgftrsThingGroups = lens _ltgftrsThingGroups (\ s a -> s{_ltgftrsThingGroups = a}) . _Default . _Coerce

-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltgftrsNextToken :: Lens' ListThingGroupsForThingResponse (Maybe Text)
ltgftrsNextToken = lens _ltgftrsNextToken (\ s a -> s{_ltgftrsNextToken = a})

-- | -- | The response status code.
ltgftrsResponseStatus :: Lens' ListThingGroupsForThingResponse Int
ltgftrsResponseStatus = lens _ltgftrsResponseStatus (\ s a -> s{_ltgftrsResponseStatus = a})

instance NFData ListThingGroupsForThingResponse where
