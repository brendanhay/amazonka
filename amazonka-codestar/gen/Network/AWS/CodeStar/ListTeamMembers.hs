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
-- Module      : Network.AWS.CodeStar.ListTeamMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all team members associated with a project.
--
--
module Network.AWS.CodeStar.ListTeamMembers
    (
    -- * Creating a Request
      listTeamMembers
    , ListTeamMembers
    -- * Request Lenses
    , ltmNextToken
    , ltmMaxResults
    , ltmProjectId

    -- * Destructuring the Response
    , listTeamMembersResponse
    , ListTeamMembersResponse
    -- * Response Lenses
    , ltmrsNextToken
    , ltmrsResponseStatus
    , ltmrsTeamMembers
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTeamMembers' smart constructor.
data ListTeamMembers = ListTeamMembers'
  { _ltmNextToken  :: !(Maybe Text)
  , _ltmMaxResults :: !(Maybe Nat)
  , _ltmProjectId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTeamMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltmNextToken' - The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- * 'ltmMaxResults' - The maximum number of team members you want returned in a response.
--
-- * 'ltmProjectId' - The ID of the project for which you want to list team members.
listTeamMembers
    :: Text -- ^ 'ltmProjectId'
    -> ListTeamMembers
listTeamMembers pProjectId_ =
  ListTeamMembers'
    { _ltmNextToken = Nothing
    , _ltmMaxResults = Nothing
    , _ltmProjectId = pProjectId_
    }


-- | The continuation token for the next set of results, if the results cannot be returned in one response.
ltmNextToken :: Lens' ListTeamMembers (Maybe Text)
ltmNextToken = lens _ltmNextToken (\ s a -> s{_ltmNextToken = a})

-- | The maximum number of team members you want returned in a response.
ltmMaxResults :: Lens' ListTeamMembers (Maybe Natural)
ltmMaxResults = lens _ltmMaxResults (\ s a -> s{_ltmMaxResults = a}) . mapping _Nat

-- | The ID of the project for which you want to list team members.
ltmProjectId :: Lens' ListTeamMembers Text
ltmProjectId = lens _ltmProjectId (\ s a -> s{_ltmProjectId = a})

instance AWSRequest ListTeamMembers where
        type Rs ListTeamMembers = ListTeamMembersResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 ListTeamMembersResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "teamMembers" .!@ mempty))

instance Hashable ListTeamMembers where

instance NFData ListTeamMembers where

instance ToHeaders ListTeamMembers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.ListTeamMembers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTeamMembers where
        toJSON ListTeamMembers'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ltmNextToken,
                  ("maxResults" .=) <$> _ltmMaxResults,
                  Just ("projectId" .= _ltmProjectId)])

instance ToPath ListTeamMembers where
        toPath = const "/"

instance ToQuery ListTeamMembers where
        toQuery = const mempty

-- | /See:/ 'listTeamMembersResponse' smart constructor.
data ListTeamMembersResponse = ListTeamMembersResponse'
  { _ltmrsNextToken      :: !(Maybe Text)
  , _ltmrsResponseStatus :: !Int
  , _ltmrsTeamMembers    :: ![TeamMember]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTeamMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltmrsNextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- * 'ltmrsResponseStatus' - -- | The response status code.
--
-- * 'ltmrsTeamMembers' - A list of team member objects for the project.
listTeamMembersResponse
    :: Int -- ^ 'ltmrsResponseStatus'
    -> ListTeamMembersResponse
listTeamMembersResponse pResponseStatus_ =
  ListTeamMembersResponse'
    { _ltmrsNextToken = Nothing
    , _ltmrsResponseStatus = pResponseStatus_
    , _ltmrsTeamMembers = mempty
    }


-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
ltmrsNextToken :: Lens' ListTeamMembersResponse (Maybe Text)
ltmrsNextToken = lens _ltmrsNextToken (\ s a -> s{_ltmrsNextToken = a})

-- | -- | The response status code.
ltmrsResponseStatus :: Lens' ListTeamMembersResponse Int
ltmrsResponseStatus = lens _ltmrsResponseStatus (\ s a -> s{_ltmrsResponseStatus = a})

-- | A list of team member objects for the project.
ltmrsTeamMembers :: Lens' ListTeamMembersResponse [TeamMember]
ltmrsTeamMembers = lens _ltmrsTeamMembers (\ s a -> s{_ltmrsTeamMembers = a}) . _Coerce

instance NFData ListTeamMembersResponse where
