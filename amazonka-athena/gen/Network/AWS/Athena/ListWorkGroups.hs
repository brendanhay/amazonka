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
-- Module      : Network.AWS.Athena.ListWorkGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available workgroups for the account.
--
--
module Network.AWS.Athena.ListWorkGroups
    (
    -- * Creating a Request
      listWorkGroups
    , ListWorkGroups
    -- * Request Lenses
    , lwgNextToken
    , lwgMaxResults

    -- * Destructuring the Response
    , listWorkGroupsResponse
    , ListWorkGroupsResponse
    -- * Response Lenses
    , lwgrsNextToken
    , lwgrsWorkGroups
    , lwgrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listWorkGroups' smart constructor.
data ListWorkGroups = ListWorkGroups'
  { _lwgNextToken  :: !(Maybe Text)
  , _lwgMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwgNextToken' - A token to be used by the next request if this request is truncated.
--
-- * 'lwgMaxResults' - The maximum number of workgroups to return in this request.
listWorkGroups
    :: ListWorkGroups
listWorkGroups =
  ListWorkGroups' {_lwgNextToken = Nothing, _lwgMaxResults = Nothing}


-- | A token to be used by the next request if this request is truncated.
lwgNextToken :: Lens' ListWorkGroups (Maybe Text)
lwgNextToken = lens _lwgNextToken (\ s a -> s{_lwgNextToken = a})

-- | The maximum number of workgroups to return in this request.
lwgMaxResults :: Lens' ListWorkGroups (Maybe Natural)
lwgMaxResults = lens _lwgMaxResults (\ s a -> s{_lwgMaxResults = a}) . mapping _Nat

instance AWSRequest ListWorkGroups where
        type Rs ListWorkGroups = ListWorkGroupsResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 ListWorkGroupsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "WorkGroups" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListWorkGroups where

instance NFData ListWorkGroups where

instance ToHeaders ListWorkGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.ListWorkGroups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListWorkGroups where
        toJSON ListWorkGroups'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lwgNextToken,
                  ("MaxResults" .=) <$> _lwgMaxResults])

instance ToPath ListWorkGroups where
        toPath = const "/"

instance ToQuery ListWorkGroups where
        toQuery = const mempty

-- | /See:/ 'listWorkGroupsResponse' smart constructor.
data ListWorkGroupsResponse = ListWorkGroupsResponse'
  { _lwgrsNextToken      :: !(Maybe Text)
  , _lwgrsWorkGroups     :: !(Maybe [WorkGroupSummary])
  , _lwgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwgrsNextToken' - A token to be used by the next request if this request is truncated.
--
-- * 'lwgrsWorkGroups' - The list of workgroups, including their names, descriptions, creation times, and states.
--
-- * 'lwgrsResponseStatus' - -- | The response status code.
listWorkGroupsResponse
    :: Int -- ^ 'lwgrsResponseStatus'
    -> ListWorkGroupsResponse
listWorkGroupsResponse pResponseStatus_ =
  ListWorkGroupsResponse'
    { _lwgrsNextToken = Nothing
    , _lwgrsWorkGroups = Nothing
    , _lwgrsResponseStatus = pResponseStatus_
    }


-- | A token to be used by the next request if this request is truncated.
lwgrsNextToken :: Lens' ListWorkGroupsResponse (Maybe Text)
lwgrsNextToken = lens _lwgrsNextToken (\ s a -> s{_lwgrsNextToken = a})

-- | The list of workgroups, including their names, descriptions, creation times, and states.
lwgrsWorkGroups :: Lens' ListWorkGroupsResponse [WorkGroupSummary]
lwgrsWorkGroups = lens _lwgrsWorkGroups (\ s a -> s{_lwgrsWorkGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
lwgrsResponseStatus :: Lens' ListWorkGroupsResponse Int
lwgrsResponseStatus = lens _lwgrsResponseStatus (\ s a -> s{_lwgrsResponseStatus = a})

instance NFData ListWorkGroupsResponse where
