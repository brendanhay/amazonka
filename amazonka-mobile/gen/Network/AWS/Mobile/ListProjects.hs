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
-- Module      : Network.AWS.Mobile.ListProjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists projects in AWS Mobile Hub.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Mobile.ListProjects
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
    , lprsProjects
    , lprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used to request projects list in AWS Mobile Hub.
--
--
--
-- /See:/ 'listProjects' smart constructor.
data ListProjects = ListProjects'
  { _lpNextToken  :: !(Maybe Text)
  , _lpMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - Pagination token. Set to null to start listing projects from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more projects.
--
-- * 'lpMaxResults' - Maximum number of records to list in a single response.
listProjects
    :: ListProjects
listProjects = ListProjects' {_lpNextToken = Nothing, _lpMaxResults = Nothing}


-- | Pagination token. Set to null to start listing projects from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more projects.
lpNextToken :: Lens' ListProjects (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | Maximum number of records to list in a single response.
lpMaxResults :: Lens' ListProjects (Maybe Int)
lpMaxResults = lens _lpMaxResults (\ s a -> s{_lpMaxResults = a})

instance AWSPager ListProjects where
        page rq rs
          | stop (rs ^. lprsNextToken) = Nothing
          | stop (rs ^. lprsProjects) = Nothing
          | otherwise =
            Just $ rq & lpNextToken .~ rs ^. lprsNextToken

instance AWSRequest ListProjects where
        type Rs ListProjects = ListProjectsResponse
        request = get mobile
        response
          = receiveJSON
              (\ s h x ->
                 ListProjectsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "projects" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListProjects where

instance NFData ListProjects where

instance ToHeaders ListProjects where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListProjects where
        toPath = const "/projects"

instance ToQuery ListProjects where
        toQuery ListProjects'{..}
          = mconcat
              ["nextToken" =: _lpNextToken,
               "maxResults" =: _lpMaxResults]

-- | Result structure used for requests to list projects in AWS Mobile Hub.
--
--
--
-- /See:/ 'listProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { _lprsNextToken      :: !(Maybe Text)
  , _lprsProjects       :: !(Maybe [ProjectSummary])
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken' - Undocumented member.
--
-- * 'lprsProjects' - Undocumented member.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listProjectsResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListProjectsResponse
listProjectsResponse pResponseStatus_ =
  ListProjectsResponse'
    { _lprsNextToken = Nothing
    , _lprsProjects = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lprsNextToken :: Lens' ListProjectsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | Undocumented member.
lprsProjects :: Lens' ListProjectsResponse [ProjectSummary]
lprsProjects = lens _lprsProjects (\ s a -> s{_lprsProjects = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListProjectsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListProjectsResponse where
