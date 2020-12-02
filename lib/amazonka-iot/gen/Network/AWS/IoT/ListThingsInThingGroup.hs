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
-- Module      : Network.AWS.IoT.ListThingsInThingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things in the specified group.
--
--
module Network.AWS.IoT.ListThingsInThingGroup
    (
    -- * Creating a Request
      listThingsInThingGroup
    , ListThingsInThingGroup
    -- * Request Lenses
    , ltitgNextToken
    , ltitgRecursive
    , ltitgMaxResults
    , ltitgThingGroupName

    -- * Destructuring the Response
    , listThingsInThingGroupResponse
    , ListThingsInThingGroupResponse
    -- * Response Lenses
    , ltitgrsNextToken
    , ltitgrsThings
    , ltitgrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThingsInThingGroup' smart constructor.
data ListThingsInThingGroup = ListThingsInThingGroup'
  { _ltitgNextToken      :: !(Maybe Text)
  , _ltitgRecursive      :: !(Maybe Bool)
  , _ltitgMaxResults     :: !(Maybe Nat)
  , _ltitgThingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingsInThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltitgNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltitgRecursive' - When true, list things in this thing group and in all child groups as well.
--
-- * 'ltitgMaxResults' - The maximum number of results to return at one time.
--
-- * 'ltitgThingGroupName' - The thing group name.
listThingsInThingGroup
    :: Text -- ^ 'ltitgThingGroupName'
    -> ListThingsInThingGroup
listThingsInThingGroup pThingGroupName_ =
  ListThingsInThingGroup'
    { _ltitgNextToken = Nothing
    , _ltitgRecursive = Nothing
    , _ltitgMaxResults = Nothing
    , _ltitgThingGroupName = pThingGroupName_
    }


-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltitgNextToken :: Lens' ListThingsInThingGroup (Maybe Text)
ltitgNextToken = lens _ltitgNextToken (\ s a -> s{_ltitgNextToken = a})

-- | When true, list things in this thing group and in all child groups as well.
ltitgRecursive :: Lens' ListThingsInThingGroup (Maybe Bool)
ltitgRecursive = lens _ltitgRecursive (\ s a -> s{_ltitgRecursive = a})

-- | The maximum number of results to return at one time.
ltitgMaxResults :: Lens' ListThingsInThingGroup (Maybe Natural)
ltitgMaxResults = lens _ltitgMaxResults (\ s a -> s{_ltitgMaxResults = a}) . mapping _Nat

-- | The thing group name.
ltitgThingGroupName :: Lens' ListThingsInThingGroup Text
ltitgThingGroupName = lens _ltitgThingGroupName (\ s a -> s{_ltitgThingGroupName = a})

instance AWSRequest ListThingsInThingGroup where
        type Rs ListThingsInThingGroup =
             ListThingsInThingGroupResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListThingsInThingGroupResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "things" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListThingsInThingGroup where

instance NFData ListThingsInThingGroup where

instance ToHeaders ListThingsInThingGroup where
        toHeaders = const mempty

instance ToPath ListThingsInThingGroup where
        toPath ListThingsInThingGroup'{..}
          = mconcat
              ["/thing-groups/", toBS _ltitgThingGroupName,
               "/things"]

instance ToQuery ListThingsInThingGroup where
        toQuery ListThingsInThingGroup'{..}
          = mconcat
              ["nextToken" =: _ltitgNextToken,
               "recursive" =: _ltitgRecursive,
               "maxResults" =: _ltitgMaxResults]

-- | /See:/ 'listThingsInThingGroupResponse' smart constructor.
data ListThingsInThingGroupResponse = ListThingsInThingGroupResponse'
  { _ltitgrsNextToken      :: !(Maybe Text)
  , _ltitgrsThings         :: !(Maybe [Text])
  , _ltitgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThingsInThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltitgrsNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltitgrsThings' - The things in the specified thing group.
--
-- * 'ltitgrsResponseStatus' - -- | The response status code.
listThingsInThingGroupResponse
    :: Int -- ^ 'ltitgrsResponseStatus'
    -> ListThingsInThingGroupResponse
listThingsInThingGroupResponse pResponseStatus_ =
  ListThingsInThingGroupResponse'
    { _ltitgrsNextToken = Nothing
    , _ltitgrsThings = Nothing
    , _ltitgrsResponseStatus = pResponseStatus_
    }


-- | The token used to get the next set of results, or __null__ if there are no additional results.
ltitgrsNextToken :: Lens' ListThingsInThingGroupResponse (Maybe Text)
ltitgrsNextToken = lens _ltitgrsNextToken (\ s a -> s{_ltitgrsNextToken = a})

-- | The things in the specified thing group.
ltitgrsThings :: Lens' ListThingsInThingGroupResponse [Text]
ltitgrsThings = lens _ltitgrsThings (\ s a -> s{_ltitgrsThings = a}) . _Default . _Coerce

-- | -- | The response status code.
ltitgrsResponseStatus :: Lens' ListThingsInThingGroupResponse Int
ltitgrsResponseStatus = lens _ltitgrsResponseStatus (\ s a -> s{_ltitgrsResponseStatus = a})

instance NFData ListThingsInThingGroupResponse where
