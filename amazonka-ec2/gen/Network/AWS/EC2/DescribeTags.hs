{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the tags for your EC2 resources.
--
-- For more information about tags, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html>
module Network.AWS.EC2.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtrqFilters
    , dtrqNextToken
    , dtrqDryRun
    , dtrqMaxResults

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrsNextToken
    , dtrsTags
    , dtrsStatus
    ) where

import           Network.AWS.EC2.Types
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
-- * 'dtrqDryRun'
--
-- * 'dtrqMaxResults'
data DescribeTags = DescribeTags'
    { _dtrqFilters    :: !(Maybe [Filter])
    , _dtrqNextToken  :: !(Maybe Text)
    , _dtrqDryRun     :: !(Maybe Bool)
    , _dtrqMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTags' smart constructor.
describeTags :: DescribeTags
describeTags =
    DescribeTags'
    { _dtrqFilters = Nothing
    , _dtrqNextToken = Nothing
    , _dtrqDryRun = Nothing
    , _dtrqMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   @key@ - The tag key.
--
-- -   @resource-id@ - The resource ID.
--
-- -   @resource-type@ - The resource type (@customer-gateway@ |
--     @dhcp-options@ | @image@ | @instance@ | @internet-gateway@ |
--     @network-acl@ | @network-interface@ | @reserved-instances@ |
--     @route-table@ | @security-group@ | @snapshot@ |
--     @spot-instances-request@ | @subnet@ | @volume@ | @vpc@ |
--     @vpn-connection@ | @vpn-gateway@).
--
-- -   @value@ - The tag value.
--
dtrqFilters :: Lens' DescribeTags [Filter]
dtrqFilters = lens _dtrqFilters (\ s a -> s{_dtrqFilters = a}) . _Default;

-- | The token to retrieve the next page of results.
dtrqNextToken :: Lens' DescribeTags (Maybe Text)
dtrqNextToken = lens _dtrqNextToken (\ s a -> s{_dtrqNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dtrqDryRun :: Lens' DescribeTags (Maybe Bool)
dtrqDryRun = lens _dtrqDryRun (\ s a -> s{_dtrqDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value larger than
-- 1000, only 1000 results are returned.
dtrqMaxResults :: Lens' DescribeTags (Maybe Int)
dtrqMaxResults = lens _dtrqMaxResults (\ s a -> s{_dtrqMaxResults = a});

instance AWSPager DescribeTags where
        page rq rs
          | stop (rs ^. dtrsNextToken) = Nothing
          | stop (rs ^. dtrsTags) = Nothing
          | otherwise =
            Just $ rq & dtrqNextToken .~ rs ^. dtrsNextToken

instance AWSRequest DescribeTags where
        type Sv DescribeTags = EC2
        type Rs DescribeTags = DescribeTagsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "tagSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath = const "/"

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["Action" =: ("DescribeTags" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dtrqFilters),
               "NextToken" =: _dtrqNextToken,
               "DryRun" =: _dtrqDryRun,
               "MaxResults" =: _dtrqMaxResults]

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
describeTagsResponse pStatus =
    DescribeTagsResponse'
    { _dtrsNextToken = Nothing
    , _dtrsTags = Nothing
    , _dtrsStatus = pStatus
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return..
dtrsNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextToken = lens _dtrsNextToken (\ s a -> s{_dtrsNextToken = a});

-- | A list of tags.
dtrsTags :: Lens' DescribeTagsResponse [TagDescription]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Default;

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DescribeTagsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
