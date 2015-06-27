{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeTags
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

-- | Describes one or more of the tags for your EC2 resources.
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
    , dtFilters
    , dtNextToken
    , dtDryRun
    , dtMaxResults

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrNextToken
    , dtrTags
    , dtrStatus
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
-- * 'dtFilters'
--
-- * 'dtNextToken'
--
-- * 'dtDryRun'
--
-- * 'dtMaxResults'
data DescribeTags = DescribeTags'
    { _dtFilters    :: Maybe [Filter]
    , _dtNextToken  :: Maybe Text
    , _dtDryRun     :: Maybe Bool
    , _dtMaxResults :: Maybe Int
    } deriving (Eq,Read,Show)

-- | 'DescribeTags' smart constructor.
describeTags :: DescribeTags
describeTags =
    DescribeTags'
    { _dtFilters = Nothing
    , _dtNextToken = Nothing
    , _dtDryRun = Nothing
    , _dtMaxResults = Nothing
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
dtFilters :: Lens' DescribeTags [Filter]
dtFilters = lens _dtFilters (\ s a -> s{_dtFilters = a}) . _Default;

-- | The token to retrieve the next page of results.
dtNextToken :: Lens' DescribeTags (Maybe Text)
dtNextToken = lens _dtNextToken (\ s a -> s{_dtNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dtDryRun :: Lens' DescribeTags (Maybe Bool)
dtDryRun = lens _dtDryRun (\ s a -> s{_dtDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value larger than
-- 1000, only 1000 results are returned.
dtMaxResults :: Lens' DescribeTags (Maybe Int)
dtMaxResults = lens _dtMaxResults (\ s a -> s{_dtMaxResults = a});

instance AWSPager DescribeTags where
        page rq rs
          | stop (rs ^. dtrNextToken) = Nothing
          | stop (rs ^. dtrTags) = Nothing
          | otherwise =
            Just $ rq & dtNextToken .~ rs ^. dtrNextToken

instance AWSRequest DescribeTags where
        type Sv DescribeTags = EC2
        type Rs DescribeTags = DescribeTagsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "nextToken") <*> (may (parseXMLList "item") x)
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
               toQuery (toQueryList "Filter" <$> _dtFilters),
               "NextToken" =: _dtNextToken, "DryRun" =: _dtDryRun,
               "MaxResults" =: _dtMaxResults]

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
    { _dtrNextToken :: Maybe Text
    , _dtrTags      :: Maybe [TagDescription]
    , _dtrStatus    :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: Int -> DescribeTagsResponse
describeTagsResponse pStatus =
    DescribeTagsResponse'
    { _dtrNextToken = Nothing
    , _dtrTags = Nothing
    , _dtrStatus = pStatus
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return..
dtrNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrNextToken = lens _dtrNextToken (\ s a -> s{_dtrNextToken = a});

-- | A list of tags.
dtrTags :: Lens' DescribeTagsResponse [TagDescription]
dtrTags = lens _dtrTags (\ s a -> s{_dtrTags = a}) . _Default;

-- | FIXME: Undocumented member.
dtrStatus :: Lens' DescribeTagsResponse Int
dtrStatus = lens _dtrStatus (\ s a -> s{_dtrStatus = a});
