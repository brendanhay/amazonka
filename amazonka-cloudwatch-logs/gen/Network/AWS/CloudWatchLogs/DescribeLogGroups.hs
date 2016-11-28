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
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogGroups
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the log groups that are associated with the AWS account making the request. The list returned in the response is ASCII-sorted by log group name.
--
--
-- By default, this operation returns up to 50 log groups. If there are more log groups to list, the response would contain a @nextToken@ value in the response body. You can also limit the number of log groups returned in the response by specifying the @limit@ parameter in the request.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogGroups
    (
    -- * Creating a Request
      describeLogGroups
    , DescribeLogGroups
    -- * Request Lenses
    , dlgLogGroupNamePrefix
    , dlgNextToken
    , dlgLimit

    -- * Destructuring the Response
    , describeLogGroupsResponse
    , DescribeLogGroupsResponse
    -- * Response Lenses
    , dlgrsLogGroups
    , dlgrsNextToken
    , dlgrsResponseStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLogGroups' smart constructor.
data DescribeLogGroups = DescribeLogGroups'
    { _dlgLogGroupNamePrefix :: !(Maybe Text)
    , _dlgNextToken          :: !(Maybe Text)
    , _dlgLimit              :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLogGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgLogGroupNamePrefix' - Will only return log groups that match the provided logGroupNamePrefix. If you don't specify a value, no prefix filter is applied.
--
-- * 'dlgNextToken' - A string token used for pagination that points to the next page of results. It must be a value obtained from the response of the previous @DescribeLogGroups@ request.
--
-- * 'dlgLimit' - The maximum number of items returned in the response. If you don't specify a value, the request would return up to 50 items.
describeLogGroups
    :: DescribeLogGroups
describeLogGroups =
    DescribeLogGroups'
    { _dlgLogGroupNamePrefix = Nothing
    , _dlgNextToken = Nothing
    , _dlgLimit = Nothing
    }

-- | Will only return log groups that match the provided logGroupNamePrefix. If you don't specify a value, no prefix filter is applied.
dlgLogGroupNamePrefix :: Lens' DescribeLogGroups (Maybe Text)
dlgLogGroupNamePrefix = lens _dlgLogGroupNamePrefix (\ s a -> s{_dlgLogGroupNamePrefix = a});

-- | A string token used for pagination that points to the next page of results. It must be a value obtained from the response of the previous @DescribeLogGroups@ request.
dlgNextToken :: Lens' DescribeLogGroups (Maybe Text)
dlgNextToken = lens _dlgNextToken (\ s a -> s{_dlgNextToken = a});

-- | The maximum number of items returned in the response. If you don't specify a value, the request would return up to 50 items.
dlgLimit :: Lens' DescribeLogGroups (Maybe Natural)
dlgLimit = lens _dlgLimit (\ s a -> s{_dlgLimit = a}) . mapping _Nat;

instance AWSPager DescribeLogGroups where
        page rq rs
          | stop (rs ^. dlgrsNextToken) = Nothing
          | stop (rs ^. dlgrsLogGroups) = Nothing
          | otherwise =
            Just $ rq & dlgNextToken .~ rs ^. dlgrsNextToken

instance AWSRequest DescribeLogGroups where
        type Rs DescribeLogGroups = DescribeLogGroupsResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLogGroupsResponse' <$>
                   (x .?> "logGroups" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLogGroups

instance NFData DescribeLogGroups

instance ToHeaders DescribeLogGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeLogGroups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLogGroups where
        toJSON DescribeLogGroups'{..}
          = object
              (catMaybes
                 [("logGroupNamePrefix" .=) <$>
                    _dlgLogGroupNamePrefix,
                  ("nextToken" .=) <$> _dlgNextToken,
                  ("limit" .=) <$> _dlgLimit])

instance ToPath DescribeLogGroups where
        toPath = const "/"

instance ToQuery DescribeLogGroups where
        toQuery = const mempty

-- | /See:/ 'describeLogGroupsResponse' smart constructor.
data DescribeLogGroupsResponse = DescribeLogGroupsResponse'
    { _dlgrsLogGroups      :: !(Maybe [LogGroup])
    , _dlgrsNextToken      :: !(Maybe Text)
    , _dlgrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLogGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrsLogGroups' - Undocumented member.
--
-- * 'dlgrsNextToken' - Undocumented member.
--
-- * 'dlgrsResponseStatus' - -- | The response status code.
describeLogGroupsResponse
    :: Int -- ^ 'dlgrsResponseStatus'
    -> DescribeLogGroupsResponse
describeLogGroupsResponse pResponseStatus_ =
    DescribeLogGroupsResponse'
    { _dlgrsLogGroups = Nothing
    , _dlgrsNextToken = Nothing
    , _dlgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dlgrsLogGroups :: Lens' DescribeLogGroupsResponse [LogGroup]
dlgrsLogGroups = lens _dlgrsLogGroups (\ s a -> s{_dlgrsLogGroups = a}) . _Default . _Coerce;

-- | Undocumented member.
dlgrsNextToken :: Lens' DescribeLogGroupsResponse (Maybe Text)
dlgrsNextToken = lens _dlgrsNextToken (\ s a -> s{_dlgrsNextToken = a});

-- | -- | The response status code.
dlgrsResponseStatus :: Lens' DescribeLogGroupsResponse Int
dlgrsResponseStatus = lens _dlgrsResponseStatus (\ s a -> s{_dlgrsResponseStatus = a});

instance NFData DescribeLogGroupsResponse
