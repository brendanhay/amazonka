{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the specified tags.
--
-- You can use filters to limit the results. For example, you can query for
-- the tags for a specific Auto Scaling group. You can specify multiple
-- values for a filter. A tag must match at least one of the specified
-- values for it to be included in the results.
--
-- You can also specify multiple filters. The result includes information
-- for a particular tag only if it matches all the filters. If there\'s no
-- match, no special message is returned.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTags.html>
module Network.AWS.AutoScaling.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtFilters
    , dtNextToken
    , dtMaxRecords

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrNextToken
    , dtrTags
    , dtrStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtFilters'
--
-- * 'dtNextToken'
--
-- * 'dtMaxRecords'
data DescribeTags = DescribeTags'
    { _dtFilters    :: !(Maybe [Filter])
    , _dtNextToken  :: !(Maybe Text)
    , _dtMaxRecords :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'DescribeTags' smart constructor.
describeTags :: DescribeTags
describeTags =
    DescribeTags'
    { _dtFilters = Nothing
    , _dtNextToken = Nothing
    , _dtMaxRecords = Nothing
    }

-- | A filter used to scope the tags to return.
dtFilters :: Lens' DescribeTags [Filter]
dtFilters = lens _dtFilters (\ s a -> s{_dtFilters = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dtNextToken :: Lens' DescribeTags (Maybe Text)
dtNextToken = lens _dtNextToken (\ s a -> s{_dtNextToken = a});

-- | The maximum number of items to return with this call.
dtMaxRecords :: Lens' DescribeTags (Maybe Int)
dtMaxRecords = lens _dtMaxRecords (\ s a -> s{_dtMaxRecords = a});

instance AWSPager DescribeTags where
        page rq rs
          | stop (rs ^. dtrNextToken) = Nothing
          | stop (rs ^. dtrTags) = Nothing
          | otherwise =
            Just $ rq & dtNextToken .~ rs ^. dtrNextToken

instance AWSRequest DescribeTags where
        type Sv DescribeTags = AutoScaling
        type Rs DescribeTags = DescribeTagsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeTagsResult"
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Tags" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure s))

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath = const "/"

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["Action" =: ("DescribeTags" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "member" <$> _dtFilters),
               "NextToken" =: _dtNextToken,
               "MaxRecords" =: _dtMaxRecords]

-- | /See:/ 'describeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrNextToken'
--
-- * 'dtrTags'
--
-- * 'dtrStatus'
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrNextToken :: !(Maybe Text)
    , _dtrTags      :: !(Maybe [TagDescription])
    , _dtrStatus    :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: Status -> DescribeTagsResponse
describeTagsResponse pStatus =
    DescribeTagsResponse'
    { _dtrNextToken = Nothing
    , _dtrTags = Nothing
    , _dtrStatus = pStatus
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dtrNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrNextToken = lens _dtrNextToken (\ s a -> s{_dtrNextToken = a});

-- | The tags.
dtrTags :: Lens' DescribeTagsResponse [TagDescription]
dtrTags = lens _dtrTags (\ s a -> s{_dtrTags = a}) . _Default;

-- | FIXME: Undocumented member.
dtrStatus :: Lens' DescribeTagsResponse Status
dtrStatus = lens _dtrStatus (\ s a -> s{_dtrStatus = a});
