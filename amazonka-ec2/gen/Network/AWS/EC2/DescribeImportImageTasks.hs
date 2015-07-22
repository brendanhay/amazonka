{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImportImageTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an import virtual machine or import snapshot
-- tasks that are already created.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportImageTasks.html>
module Network.AWS.EC2.DescribeImportImageTasks
    (
    -- * Request
      DescribeImportImageTasks
    -- ** Request constructor
    , describeImportImageTasks
    -- ** Request lenses
    , diitrqFilters
    , diitrqImportTaskIds
    , diitrqNextToken
    , diitrqDryRun
    , diitrqMaxResults

    -- * Response
    , DescribeImportImageTasksResponse
    -- ** Response constructor
    , describeImportImageTasksResponse
    -- ** Response lenses
    , diitrsImportImageTasks
    , diitrsNextToken
    , diitrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImportImageTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diitrqFilters'
--
-- * 'diitrqImportTaskIds'
--
-- * 'diitrqNextToken'
--
-- * 'diitrqDryRun'
--
-- * 'diitrqMaxResults'
data DescribeImportImageTasks = DescribeImportImageTasks'
    { _diitrqFilters       :: !(Maybe [Filter])
    , _diitrqImportTaskIds :: !(Maybe [Text])
    , _diitrqNextToken     :: !(Maybe Text)
    , _diitrqDryRun        :: !(Maybe Bool)
    , _diitrqMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImportImageTasks' smart constructor.
describeImportImageTasks :: DescribeImportImageTasks
describeImportImageTasks =
    DescribeImportImageTasks'
    { _diitrqFilters = Nothing
    , _diitrqImportTaskIds = Nothing
    , _diitrqNextToken = Nothing
    , _diitrqDryRun = Nothing
    , _diitrqMaxResults = Nothing
    }

-- | One or more filters.
diitrqFilters :: Lens' DescribeImportImageTasks [Filter]
diitrqFilters = lens _diitrqFilters (\ s a -> s{_diitrqFilters = a}) . _Default;

-- | A list of import image task IDs.
diitrqImportTaskIds :: Lens' DescribeImportImageTasks [Text]
diitrqImportTaskIds = lens _diitrqImportTaskIds (\ s a -> s{_diitrqImportTaskIds = a}) . _Default;

-- | A token that indicates the next page of results.
diitrqNextToken :: Lens' DescribeImportImageTasks (Maybe Text)
diitrqNextToken = lens _diitrqNextToken (\ s a -> s{_diitrqNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
diitrqDryRun :: Lens' DescribeImportImageTasks (Maybe Bool)
diitrqDryRun = lens _diitrqDryRun (\ s a -> s{_diitrqDryRun = a});

-- | The maximum number of results to return in a single request.
diitrqMaxResults :: Lens' DescribeImportImageTasks (Maybe Int)
diitrqMaxResults = lens _diitrqMaxResults (\ s a -> s{_diitrqMaxResults = a});

instance AWSRequest DescribeImportImageTasks where
        type Sv DescribeImportImageTasks = EC2
        type Rs DescribeImportImageTasks =
             DescribeImportImageTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImportImageTasksResponse' <$>
                   (x .@? "importImageTaskSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeImportImageTasks where
        toHeaders = const mempty

instance ToPath DescribeImportImageTasks where
        toPath = const "/"

instance ToQuery DescribeImportImageTasks where
        toQuery DescribeImportImageTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeImportImageTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _diitrqFilters),
               toQuery
                 (toQueryList "ImportTaskId" <$>
                    _diitrqImportTaskIds),
               "NextToken" =: _diitrqNextToken,
               "DryRun" =: _diitrqDryRun,
               "MaxResults" =: _diitrqMaxResults]

-- | /See:/ 'describeImportImageTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diitrsImportImageTasks'
--
-- * 'diitrsNextToken'
--
-- * 'diitrsStatus'
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'
    { _diitrsImportImageTasks :: !(Maybe [ImportImageTask])
    , _diitrsNextToken        :: !(Maybe Text)
    , _diitrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImportImageTasksResponse' smart constructor.
describeImportImageTasksResponse :: Int -> DescribeImportImageTasksResponse
describeImportImageTasksResponse pStatus_ =
    DescribeImportImageTasksResponse'
    { _diitrsImportImageTasks = Nothing
    , _diitrsNextToken = Nothing
    , _diitrsStatus = pStatus_
    }

-- | A list of zero or more import image tasks that are currently active or
-- were completed or canceled in the previous 7 days.
diitrsImportImageTasks :: Lens' DescribeImportImageTasksResponse [ImportImageTask]
diitrsImportImageTasks = lens _diitrsImportImageTasks (\ s a -> s{_diitrsImportImageTasks = a}) . _Default;

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
diitrsNextToken :: Lens' DescribeImportImageTasksResponse (Maybe Text)
diitrsNextToken = lens _diitrsNextToken (\ s a -> s{_diitrsNextToken = a});

-- | FIXME: Undocumented member.
diitrsStatus :: Lens' DescribeImportImageTasksResponse Int
diitrsStatus = lens _diitrsStatus (\ s a -> s{_diitrsStatus = a});
