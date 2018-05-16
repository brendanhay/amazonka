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
-- Module      : Network.AWS.CodeStar.ListProjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all projects in AWS CodeStar associated with your AWS account.
--
--
module Network.AWS.CodeStar.ListProjects
    (
    -- * Creating a Request
      listProjects
    , ListProjects
    -- * Request Lenses
    , lpNextToken
    , lpMaxResults

    -- * Destructuring the Response
    , listProjectsResponse
    , ListProjectsResponse
    -- * Response Lenses
    , lprsNextToken
    , lprsResponseStatus
    , lprsProjects
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProjects' smart constructor.
data ListProjects = ListProjects'
  { _lpNextToken  :: !(Maybe Text)
  , _lpMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - The continuation token to be used to return the next set of results, if the results cannot be returned in one response.
--
-- * 'lpMaxResults' - The maximum amount of data that can be contained in a single set of results.
listProjects
    :: ListProjects
listProjects = ListProjects' {_lpNextToken = Nothing, _lpMaxResults = Nothing}


-- | The continuation token to be used to return the next set of results, if the results cannot be returned in one response.
lpNextToken :: Lens' ListProjects (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | The maximum amount of data that can be contained in a single set of results.
lpMaxResults :: Lens' ListProjects (Maybe Natural)
lpMaxResults = lens _lpMaxResults (\ s a -> s{_lpMaxResults = a}) . mapping _Nat

instance AWSRequest ListProjects where
        type Rs ListProjects = ListProjectsResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 ListProjectsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "projects" .!@ mempty))

instance Hashable ListProjects where

instance NFData ListProjects where

instance ToHeaders ListProjects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.ListProjects" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListProjects where
        toJSON ListProjects'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lpNextToken,
                  ("maxResults" .=) <$> _lpMaxResults])

instance ToPath ListProjects where
        toPath = const "/"

instance ToQuery ListProjects where
        toQuery = const mempty

-- | /See:/ 'listProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { _lprsNextToken      :: !(Maybe Text)
  , _lprsResponseStatus :: !Int
  , _lprsProjects       :: ![ProjectSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken' - The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- * 'lprsResponseStatus' - -- | The response status code.
--
-- * 'lprsProjects' - A list of projects.
listProjectsResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListProjectsResponse
listProjectsResponse pResponseStatus_ =
  ListProjectsResponse'
    { _lprsNextToken = Nothing
    , _lprsResponseStatus = pResponseStatus_
    , _lprsProjects = mempty
    }


-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
lprsNextToken :: Lens' ListProjectsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListProjectsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

-- | A list of projects.
lprsProjects :: Lens' ListProjectsResponse [ProjectSummary]
lprsProjects = lens _lprsProjects (\ s a -> s{_lprsProjects = a}) . _Coerce

instance NFData ListProjectsResponse where
