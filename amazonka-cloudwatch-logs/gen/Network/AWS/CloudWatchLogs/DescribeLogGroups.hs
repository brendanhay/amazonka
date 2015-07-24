{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns all the log groups that are associated with the AWS account
-- making the request. The list returned in the response is ASCII-sorted by
-- log group name.
--
-- By default, this operation returns up to 50 log groups. If there are
-- more log groups to list, the response would contain a @nextToken@ value
-- in the response body. You can also limit the number of log groups
-- returned in the response by specifying the @limit@ parameter in the
-- request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html>
module Network.AWS.CloudWatchLogs.DescribeLogGroups
    (
    -- * Request
      DescribeLogGroups
    -- ** Request constructor
    , describeLogGroups
    -- ** Request lenses
    , dlgNextToken
    , dlgLogGroupNamePrefix
    , dlgLimit

    -- * Response
    , DescribeLogGroupsResponse
    -- ** Response constructor
    , describeLogGroupsResponse
    -- ** Response lenses
    , dlgrsLogGroups
    , dlgrsNextToken
    , dlgrsStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLogGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlgNextToken'
--
-- * 'dlgLogGroupNamePrefix'
--
-- * 'dlgLimit'
data DescribeLogGroups = DescribeLogGroups'
    { _dlgNextToken          :: !(Maybe Text)
    , _dlgLogGroupNamePrefix :: !(Maybe Text)
    , _dlgLimit              :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLogGroups' smart constructor.
describeLogGroups :: DescribeLogGroups
describeLogGroups =
    DescribeLogGroups'
    { _dlgNextToken = Nothing
    , _dlgLogGroupNamePrefix = Nothing
    , _dlgLimit = Nothing
    }

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- @DescribeLogGroups@ request.
dlgNextToken :: Lens' DescribeLogGroups (Maybe Text)
dlgNextToken = lens _dlgNextToken (\ s a -> s{_dlgNextToken = a});

-- | Will only return log groups that match the provided logGroupNamePrefix.
-- If you don\'t specify a value, no prefix filter is applied.
dlgLogGroupNamePrefix :: Lens' DescribeLogGroups (Maybe Text)
dlgLogGroupNamePrefix = lens _dlgLogGroupNamePrefix (\ s a -> s{_dlgLogGroupNamePrefix = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
dlgLimit :: Lens' DescribeLogGroups (Maybe Natural)
dlgLimit = lens _dlgLimit (\ s a -> s{_dlgLimit = a}) . mapping _Nat;

instance AWSRequest DescribeLogGroups where
        type Sv DescribeLogGroups = CloudWatchLogs
        type Rs DescribeLogGroups = DescribeLogGroupsResponse
        request = postJSON "DescribeLogGroups"
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLogGroupsResponse' <$>
                   (x .?> "logGroups" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

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
              ["nextToken" .= _dlgNextToken,
               "logGroupNamePrefix" .= _dlgLogGroupNamePrefix,
               "limit" .= _dlgLimit]

instance ToPath DescribeLogGroups where
        toPath = const "/"

instance ToQuery DescribeLogGroups where
        toQuery = const mempty

-- | /See:/ 'describeLogGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlgrsLogGroups'
--
-- * 'dlgrsNextToken'
--
-- * 'dlgrsStatus'
data DescribeLogGroupsResponse = DescribeLogGroupsResponse'
    { _dlgrsLogGroups :: !(Maybe [LogGroup])
    , _dlgrsNextToken :: !(Maybe Text)
    , _dlgrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLogGroupsResponse' smart constructor.
describeLogGroupsResponse :: Int -> DescribeLogGroupsResponse
describeLogGroupsResponse pStatus_ =
    DescribeLogGroupsResponse'
    { _dlgrsLogGroups = Nothing
    , _dlgrsNextToken = Nothing
    , _dlgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dlgrsLogGroups :: Lens' DescribeLogGroupsResponse [LogGroup]
dlgrsLogGroups = lens _dlgrsLogGroups (\ s a -> s{_dlgrsLogGroups = a}) . _Default;

-- | FIXME: Undocumented member.
dlgrsNextToken :: Lens' DescribeLogGroupsResponse (Maybe Text)
dlgrsNextToken = lens _dlgrsNextToken (\ s a -> s{_dlgrsNextToken = a});

-- | FIXME: Undocumented member.
dlgrsStatus :: Lens' DescribeLogGroupsResponse Int
dlgrsStatus = lens _dlgrsStatus (\ s a -> s{_dlgrsStatus = a});
