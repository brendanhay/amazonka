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
-- Module      : Network.AWS.CodeBuild.ListProjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build project names, with each build project name representing a single build project.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListProjects
    (
    -- * Creating a Request
      listProjects
    , ListProjects
    -- * Request Lenses
    , lpSortOrder
    , lpNextToken
    , lpSortBy

    -- * Destructuring the Response
    , listProjectsResponse
    , ListProjectsResponse
    -- * Response Lenses
    , lprsNextToken
    , lprsProjects
    , lprsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProjects' smart constructor.
data ListProjects = ListProjects'
  { _lpSortOrder :: !(Maybe SortOrderType)
  , _lpNextToken :: !(Maybe Text)
  , _lpSortBy    :: !(Maybe ProjectSortByType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpSortOrder' - The order in which to list build projects. Valid values include:     * @ASCENDING@ : List the build project names in ascending order.     * @DESCENDING@ : List the build project names in descending order. Use @sortBy@ to specify the criterion to be used to list build project names.
--
-- * 'lpNextToken' - During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lpSortBy' - The criterion to be used to list build project names. Valid values include:     * @CREATED_TIME@ : List the build project names based on when each build project was created.     * @LAST_MODIFIED_TIME@ : List the build project names based on when information about each build project was last changed.     * @NAME@ : List the build project names based on each build project's name. Use @sortOrder@ to specify in what order to list the build project names based on the preceding criteria.
listProjects
    :: ListProjects
listProjects =
  ListProjects'
    {_lpSortOrder = Nothing, _lpNextToken = Nothing, _lpSortBy = Nothing}


-- | The order in which to list build projects. Valid values include:     * @ASCENDING@ : List the build project names in ascending order.     * @DESCENDING@ : List the build project names in descending order. Use @sortBy@ to specify the criterion to be used to list build project names.
lpSortOrder :: Lens' ListProjects (Maybe SortOrderType)
lpSortOrder = lens _lpSortOrder (\ s a -> s{_lpSortOrder = a})

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lpNextToken :: Lens' ListProjects (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | The criterion to be used to list build project names. Valid values include:     * @CREATED_TIME@ : List the build project names based on when each build project was created.     * @LAST_MODIFIED_TIME@ : List the build project names based on when information about each build project was last changed.     * @NAME@ : List the build project names based on each build project's name. Use @sortOrder@ to specify in what order to list the build project names based on the preceding criteria.
lpSortBy :: Lens' ListProjects (Maybe ProjectSortByType)
lpSortBy = lens _lpSortBy (\ s a -> s{_lpSortBy = a})

instance AWSPager ListProjects where
        page rq rs
          | stop (rs ^. lprsNextToken) = Nothing
          | stop (rs ^. lprsProjects) = Nothing
          | otherwise =
            Just $ rq & lpNextToken .~ rs ^. lprsNextToken

instance AWSRequest ListProjects where
        type Rs ListProjects = ListProjectsResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 ListProjectsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "projects") <*>
                     (pure (fromEnum s)))

instance Hashable ListProjects where

instance NFData ListProjects where

instance ToHeaders ListProjects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.ListProjects" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListProjects where
        toJSON ListProjects'{..}
          = object
              (catMaybes
                 [("sortOrder" .=) <$> _lpSortOrder,
                  ("nextToken" .=) <$> _lpNextToken,
                  ("sortBy" .=) <$> _lpSortBy])

instance ToPath ListProjects where
        toPath = const "/"

instance ToQuery ListProjects where
        toQuery = const mempty

-- | /See:/ 'listProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { _lprsNextToken      :: !(Maybe Text)
  , _lprsProjects       :: !(Maybe (List1 Text))
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken' - If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- * 'lprsProjects' - The list of build project names, with each build project name representing a single build project.
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


-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
lprsNextToken :: Lens' ListProjectsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | The list of build project names, with each build project name representing a single build project.
lprsProjects :: Lens' ListProjectsResponse (Maybe (NonEmpty Text))
lprsProjects = lens _lprsProjects (\ s a -> s{_lprsProjects = a}) . mapping _List1

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListProjectsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListProjectsResponse where
