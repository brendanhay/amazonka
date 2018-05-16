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
-- Module      : Network.AWS.GameLift.ListBuilds
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves build records for all builds associated with the AWS account in use. You can limit results to builds that are in a specific status by using the @Status@ parameter. Use the pagination parameters to retrieve results in a set of sequential pages.
--
--
-- Build-related operations include:
--
--     * 'CreateBuild'
--
--     * 'ListBuilds'
--
--     * 'DescribeBuild'
--
--     * 'UpdateBuild'
--
--     * 'DeleteBuild'
--
--
--
module Network.AWS.GameLift.ListBuilds
    (
    -- * Creating a Request
      listBuilds
    , ListBuilds
    -- * Request Lenses
    , lbStatus
    , lbNextToken
    , lbLimit

    -- * Destructuring the Response
    , listBuildsResponse
    , ListBuildsResponse
    -- * Response Lenses
    , lbrsBuilds
    , lbrsNextToken
    , lbrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'listBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { _lbStatus    :: !(Maybe BuildStatus)
  , _lbNextToken :: !(Maybe Text)
  , _lbLimit     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBuilds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbStatus' - Build status to filter results by. To retrieve all builds, leave this parameter empty. Possible build statuses include the following:     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.      * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
--
-- * 'lbNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'lbLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
listBuilds
    :: ListBuilds
listBuilds =
  ListBuilds' {_lbStatus = Nothing, _lbNextToken = Nothing, _lbLimit = Nothing}


-- | Build status to filter results by. To retrieve all builds, leave this parameter empty. Possible build statuses include the following:     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.      * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
lbStatus :: Lens' ListBuilds (Maybe BuildStatus)
lbStatus = lens _lbStatus (\ s a -> s{_lbStatus = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
lbNextToken :: Lens' ListBuilds (Maybe Text)
lbNextToken = lens _lbNextToken (\ s a -> s{_lbNextToken = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
lbLimit :: Lens' ListBuilds (Maybe Natural)
lbLimit = lens _lbLimit (\ s a -> s{_lbLimit = a}) . mapping _Nat

instance AWSRequest ListBuilds where
        type Rs ListBuilds = ListBuildsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 ListBuildsResponse' <$>
                   (x .?> "Builds" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBuilds where

instance NFData ListBuilds where

instance ToHeaders ListBuilds where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.ListBuilds" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListBuilds where
        toJSON ListBuilds'{..}
          = object
              (catMaybes
                 [("Status" .=) <$> _lbStatus,
                  ("NextToken" .=) <$> _lbNextToken,
                  ("Limit" .=) <$> _lbLimit])

instance ToPath ListBuilds where
        toPath = const "/"

instance ToQuery ListBuilds where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'listBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { _lbrsBuilds         :: !(Maybe [Build])
  , _lbrsNextToken      :: !(Maybe Text)
  , _lbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBuildsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsBuilds' - Collection of build records that match the request.
--
-- * 'lbrsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'lbrsResponseStatus' - -- | The response status code.
listBuildsResponse
    :: Int -- ^ 'lbrsResponseStatus'
    -> ListBuildsResponse
listBuildsResponse pResponseStatus_ =
  ListBuildsResponse'
    { _lbrsBuilds = Nothing
    , _lbrsNextToken = Nothing
    , _lbrsResponseStatus = pResponseStatus_
    }


-- | Collection of build records that match the request.
lbrsBuilds :: Lens' ListBuildsResponse [Build]
lbrsBuilds = lens _lbrsBuilds (\ s a -> s{_lbrsBuilds = a}) . _Default . _Coerce

-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
lbrsNextToken :: Lens' ListBuildsResponse (Maybe Text)
lbrsNextToken = lens _lbrsNextToken (\ s a -> s{_lbrsNextToken = a})

-- | -- | The response status code.
lbrsResponseStatus :: Lens' ListBuildsResponse Int
lbrsResponseStatus = lens _lbrsResponseStatus (\ s a -> s{_lbrsResponseStatus = a})

instance NFData ListBuildsResponse where
