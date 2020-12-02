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
-- Module      : Network.AWS.WorkSpaces.DescribeIPGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your IP access control groups.
--
--
module Network.AWS.WorkSpaces.DescribeIPGroups
    (
    -- * Creating a Request
      describeIPGroups
    , DescribeIPGroups
    -- * Request Lenses
    , dipgGroupIds
    , dipgNextToken
    , dipgMaxResults

    -- * Destructuring the Response
    , describeIPGroupsResponse
    , DescribeIPGroupsResponse
    -- * Response Lenses
    , digsrsResult
    , digsrsNextToken
    , digsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'describeIPGroups' smart constructor.
data DescribeIPGroups = DescribeIPGroups'
  { _dipgGroupIds   :: !(Maybe [Text])
  , _dipgNextToken  :: !(Maybe Text)
  , _dipgMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIPGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipgGroupIds' - The IDs of one or more IP access control groups.
--
-- * 'dipgNextToken' - The token for the next set of results. (You received this token from a previous call.)
--
-- * 'dipgMaxResults' - The maximum number of items to return.
describeIPGroups
    :: DescribeIPGroups
describeIPGroups =
  DescribeIPGroups'
    { _dipgGroupIds = Nothing
    , _dipgNextToken = Nothing
    , _dipgMaxResults = Nothing
    }


-- | The IDs of one or more IP access control groups.
dipgGroupIds :: Lens' DescribeIPGroups [Text]
dipgGroupIds = lens _dipgGroupIds (\ s a -> s{_dipgGroupIds = a}) . _Default . _Coerce

-- | The token for the next set of results. (You received this token from a previous call.)
dipgNextToken :: Lens' DescribeIPGroups (Maybe Text)
dipgNextToken = lens _dipgNextToken (\ s a -> s{_dipgNextToken = a})

-- | The maximum number of items to return.
dipgMaxResults :: Lens' DescribeIPGroups (Maybe Natural)
dipgMaxResults = lens _dipgMaxResults (\ s a -> s{_dipgMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeIPGroups where
        type Rs DescribeIPGroups = DescribeIPGroupsResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 DescribeIPGroupsResponse' <$>
                   (x .?> "Result" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeIPGroups where

instance NFData DescribeIPGroups where

instance ToHeaders DescribeIPGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DescribeIpGroups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeIPGroups where
        toJSON DescribeIPGroups'{..}
          = object
              (catMaybes
                 [("GroupIds" .=) <$> _dipgGroupIds,
                  ("NextToken" .=) <$> _dipgNextToken,
                  ("MaxResults" .=) <$> _dipgMaxResults])

instance ToPath DescribeIPGroups where
        toPath = const "/"

instance ToQuery DescribeIPGroups where
        toQuery = const mempty

-- | /See:/ 'describeIPGroupsResponse' smart constructor.
data DescribeIPGroupsResponse = DescribeIPGroupsResponse'
  { _digsrsResult         :: !(Maybe [WorkspacesIPGroup])
  , _digsrsNextToken      :: !(Maybe Text)
  , _digsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIPGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digsrsResult' - Information about the IP access control groups.
--
-- * 'digsrsNextToken' - The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
--
-- * 'digsrsResponseStatus' - -- | The response status code.
describeIPGroupsResponse
    :: Int -- ^ 'digsrsResponseStatus'
    -> DescribeIPGroupsResponse
describeIPGroupsResponse pResponseStatus_ =
  DescribeIPGroupsResponse'
    { _digsrsResult = Nothing
    , _digsrsNextToken = Nothing
    , _digsrsResponseStatus = pResponseStatus_
    }


-- | Information about the IP access control groups.
digsrsResult :: Lens' DescribeIPGroupsResponse [WorkspacesIPGroup]
digsrsResult = lens _digsrsResult (\ s a -> s{_digsrsResult = a}) . _Default . _Coerce

-- | The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
digsrsNextToken :: Lens' DescribeIPGroupsResponse (Maybe Text)
digsrsNextToken = lens _digsrsNextToken (\ s a -> s{_digsrsNextToken = a})

-- | -- | The response status code.
digsrsResponseStatus :: Lens' DescribeIPGroupsResponse Int
digsrsResponseStatus = lens _digsrsResponseStatus (\ s a -> s{_digsrsResponseStatus = a})

instance NFData DescribeIPGroupsResponse where
