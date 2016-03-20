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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves build records for all builds associated with an AWS account.
-- You can filter the result set by build status. Use the pagination
-- parameters to retrieve results in a set of sequential pages.
--
-- Build records are not listed in any particular order.
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

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'listBuilds' smart constructor.
data ListBuilds = ListBuilds'
    { _lbStatus    :: !(Maybe BuildStatus)
    , _lbNextToken :: !(Maybe Text)
    , _lbLimit     :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListBuilds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbStatus'
--
-- * 'lbNextToken'
--
-- * 'lbLimit'
listBuilds
    :: ListBuilds
listBuilds =
    ListBuilds'
    { _lbStatus = Nothing
    , _lbNextToken = Nothing
    , _lbLimit = Nothing
    }

-- | Build state to filter results on. Use this parameter to retrieve builds
-- in a certain state. To retrieve all builds, leave this parameter empty.
-- Possible build states include:
--
-- -   INITIALIZED: A new build has been defined, but no files have been
--     uploaded. You cannot create fleets for builds that are in this
--     state. When a build is successfully created, the build state is set
--     to this value.
-- -   READY: The game build has been successfully uploaded. You can now
--     create new fleets for this build.
-- -   FAILED: The game build upload failed. You cannot create new fleets
--     for this build.
lbStatus :: Lens' ListBuilds (Maybe BuildStatus)
lbStatus = lens _lbStatus (\ s a -> s{_lbStatus = a});

-- | Token indicating the start of the next sequential page of results. Use
-- the token that is returned with a previous call to this action. To
-- specify the start of the result set, do not specify a value.
lbNextToken :: Lens' ListBuilds (Maybe Text)
lbNextToken = lens _lbNextToken (\ s a -> s{_lbNextToken = a});

-- | Maximum number of results to return. You can use this parameter with
-- /NextToken/ to get results as a set of sequential pages.
lbLimit :: Lens' ListBuilds (Maybe Natural)
lbLimit = lens _lbLimit (\ s a -> s{_lbLimit = a}) . mapping _Nat;

instance AWSRequest ListBuilds where
        type Rs ListBuilds = ListBuildsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 ListBuildsResponse' <$>
                   (x .?> "Builds" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListBuilds

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
-- /See:/ 'listBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
    { _lbrsBuilds         :: !(Maybe [Build])
    , _lbrsNextToken      :: !(Maybe Text)
    , _lbrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListBuildsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsBuilds'
--
-- * 'lbrsNextToken'
--
-- * 'lbrsResponseStatus'
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
lbrsBuilds = lens _lbrsBuilds (\ s a -> s{_lbrsBuilds = a}) . _Default . _Coerce;

-- | Token indicating where to resume retrieving results on the next call to
-- this action. If no token is returned, these results represent the end of
-- the list.
--
-- If a request has a limit that exactly matches the number of remaining
-- results, a token is returned even though there are no more results to
-- retrieve.
lbrsNextToken :: Lens' ListBuildsResponse (Maybe Text)
lbrsNextToken = lens _lbrsNextToken (\ s a -> s{_lbrsNextToken = a});

-- | The response status code.
lbrsResponseStatus :: Lens' ListBuildsResponse Int
lbrsResponseStatus = lens _lbrsResponseStatus (\ s a -> s{_lbrsResponseStatus = a});
