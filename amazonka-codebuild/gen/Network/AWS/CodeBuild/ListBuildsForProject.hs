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
-- Module      : Network.AWS.CodeBuild.ListBuildsForProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build IDs for the specified build project, with each build ID representing a single build.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildsForProject
    (
    -- * Creating a Request
      listBuildsForProject
    , ListBuildsForProject
    -- * Request Lenses
    , lbfpSortOrder
    , lbfpNextToken
    , lbfpProjectName

    -- * Destructuring the Response
    , listBuildsForProjectResponse
    , ListBuildsForProjectResponse
    -- * Response Lenses
    , lbfprsIds
    , lbfprsNextToken
    , lbfprsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBuildsForProject' smart constructor.
data ListBuildsForProject = ListBuildsForProject'
  { _lbfpSortOrder   :: !(Maybe SortOrderType)
  , _lbfpNextToken   :: !(Maybe Text)
  , _lbfpProjectName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBuildsForProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbfpSortOrder' - The order to list build IDs. Valid values include:     * @ASCENDING@ : List the build IDs in ascending order by build ID.     * @DESCENDING@ : List the build IDs in descending order by build ID.
--
-- * 'lbfpNextToken' - During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lbfpProjectName' - The name of the AWS CodeBuild project.
listBuildsForProject
    :: Text -- ^ 'lbfpProjectName'
    -> ListBuildsForProject
listBuildsForProject pProjectName_ =
  ListBuildsForProject'
    { _lbfpSortOrder = Nothing
    , _lbfpNextToken = Nothing
    , _lbfpProjectName = pProjectName_
    }


-- | The order to list build IDs. Valid values include:     * @ASCENDING@ : List the build IDs in ascending order by build ID.     * @DESCENDING@ : List the build IDs in descending order by build ID.
lbfpSortOrder :: Lens' ListBuildsForProject (Maybe SortOrderType)
lbfpSortOrder = lens _lbfpSortOrder (\ s a -> s{_lbfpSortOrder = a})

-- | During a previous call, if there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lbfpNextToken :: Lens' ListBuildsForProject (Maybe Text)
lbfpNextToken = lens _lbfpNextToken (\ s a -> s{_lbfpNextToken = a})

-- | The name of the AWS CodeBuild project.
lbfpProjectName :: Lens' ListBuildsForProject Text
lbfpProjectName = lens _lbfpProjectName (\ s a -> s{_lbfpProjectName = a})

instance AWSPager ListBuildsForProject where
        page rq rs
          | stop (rs ^. lbfprsNextToken) = Nothing
          | stop (rs ^. lbfprsIds) = Nothing
          | otherwise =
            Just $ rq & lbfpNextToken .~ rs ^. lbfprsNextToken

instance AWSRequest ListBuildsForProject where
        type Rs ListBuildsForProject =
             ListBuildsForProjectResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 ListBuildsForProjectResponse' <$>
                   (x .?> "ids") <*> (x .?> "nextToken") <*>
                     (pure (fromEnum s)))

instance Hashable ListBuildsForProject where

instance NFData ListBuildsForProject where

instance ToHeaders ListBuildsForProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.ListBuildsForProject" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListBuildsForProject where
        toJSON ListBuildsForProject'{..}
          = object
              (catMaybes
                 [("sortOrder" .=) <$> _lbfpSortOrder,
                  ("nextToken" .=) <$> _lbfpNextToken,
                  Just ("projectName" .= _lbfpProjectName)])

instance ToPath ListBuildsForProject where
        toPath = const "/"

instance ToQuery ListBuildsForProject where
        toQuery = const mempty

-- | /See:/ 'listBuildsForProjectResponse' smart constructor.
data ListBuildsForProjectResponse = ListBuildsForProjectResponse'
  { _lbfprsIds            :: !(Maybe (List1 Text))
  , _lbfprsNextToken      :: !(Maybe Text)
  , _lbfprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBuildsForProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbfprsIds' - A list of build IDs for the specified build project, with each build ID representing a single build.
--
-- * 'lbfprsNextToken' - If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- * 'lbfprsResponseStatus' - -- | The response status code.
listBuildsForProjectResponse
    :: Int -- ^ 'lbfprsResponseStatus'
    -> ListBuildsForProjectResponse
listBuildsForProjectResponse pResponseStatus_ =
  ListBuildsForProjectResponse'
    { _lbfprsIds = Nothing
    , _lbfprsNextToken = Nothing
    , _lbfprsResponseStatus = pResponseStatus_
    }


-- | A list of build IDs for the specified build project, with each build ID representing a single build.
lbfprsIds :: Lens' ListBuildsForProjectResponse (Maybe (NonEmpty Text))
lbfprsIds = lens _lbfprsIds (\ s a -> s{_lbfprsIds = a}) . mapping _List1

-- | If there are more than 100 items in the list, only the first 100 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
lbfprsNextToken :: Lens' ListBuildsForProjectResponse (Maybe Text)
lbfprsNextToken = lens _lbfprsNextToken (\ s a -> s{_lbfprsNextToken = a})

-- | -- | The response status code.
lbfprsResponseStatus :: Lens' ListBuildsForProjectResponse Int
lbfprsResponseStatus = lens _lbfprsResponseStatus (\ s a -> s{_lbfprsResponseStatus = a})

instance NFData ListBuildsForProjectResponse where
