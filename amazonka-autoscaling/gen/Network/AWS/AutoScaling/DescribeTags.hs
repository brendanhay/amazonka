{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags.
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
    , dtrqFilters
    , dtrqNextToken
    , dtrqMaxRecords

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrsNextToken
    , dtrsTags
    , dtrsStatus
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
-- * 'dtrqFilters'
--
-- * 'dtrqNextToken'
--
-- * 'dtrqMaxRecords'
data DescribeTags = DescribeTags'
    { _dtrqFilters    :: !(Maybe [Filter])
    , _dtrqNextToken  :: !(Maybe Text)
    , _dtrqMaxRecords :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTags' smart constructor.
describeTags :: DescribeTags
describeTags =
    DescribeTags'
    { _dtrqFilters = Nothing
    , _dtrqNextToken = Nothing
    , _dtrqMaxRecords = Nothing
    }

-- | A filter used to scope the tags to return.
dtrqFilters :: Lens' DescribeTags [Filter]
dtrqFilters = lens _dtrqFilters (\ s a -> s{_dtrqFilters = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dtrqNextToken :: Lens' DescribeTags (Maybe Text)
dtrqNextToken = lens _dtrqNextToken (\ s a -> s{_dtrqNextToken = a});

-- | The maximum number of items to return with this call.
dtrqMaxRecords :: Lens' DescribeTags (Maybe Int)
dtrqMaxRecords = lens _dtrqMaxRecords (\ s a -> s{_dtrqMaxRecords = a});

instance AWSPager DescribeTags where
        page rq rs
          | stop (rs ^. dtrsNextToken) = Nothing
          | stop (rs ^. dtrsTags) = Nothing
          | otherwise =
            Just $ rq & dtrqNextToken .~ rs ^. dtrsNextToken

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
                     <*> (pure (fromEnum s)))

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
                 toQuery (toQueryList "member" <$> _dtrqFilters),
               "NextToken" =: _dtrqNextToken,
               "MaxRecords" =: _dtrqMaxRecords]

-- | /See:/ 'describeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrsNextToken'
--
-- * 'dtrsTags'
--
-- * 'dtrsStatus'
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrsNextToken :: !(Maybe Text)
    , _dtrsTags      :: !(Maybe [TagDescription])
    , _dtrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: Int -> DescribeTagsResponse
describeTagsResponse pStatus_ =
    DescribeTagsResponse'
    { _dtrsNextToken = Nothing
    , _dtrsTags = Nothing
    , _dtrsStatus = pStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dtrsNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextToken = lens _dtrsNextToken (\ s a -> s{_dtrsNextToken = a});

-- | The tags.
dtrsTags :: Lens' DescribeTagsResponse [TagDescription]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Default;

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DescribeTagsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
