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
-- Module      : Network.AWS.EC2.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the tags for your EC2 resources.
--
-- For more information about tags, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html AWS API Reference> for DescribeTags.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTags
    (
    -- * Creating a Request
      describeTags
    , DescribeTags
    -- * Request Lenses
    , dtFilters
    , dtNextToken
    , dtDryRun
    , dtMaxResults

    -- * Destructuring the Response
    , describeTagsResponse
    , DescribeTagsResponse
    -- * Response Lenses
    , dtrsNextToken
    , dtrsTags
    , dtrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
data DescribeTags = DescribeTags'
    { _dtFilters    :: !(Maybe [Filter])
    , _dtNextToken  :: !(Maybe Text)
    , _dtDryRun     :: !(Maybe Bool)
    , _dtMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtFilters'
--
-- * 'dtNextToken'
--
-- * 'dtDryRun'
--
-- * 'dtMaxResults'
describeTags
    :: DescribeTags
describeTags =
    DescribeTags'
    { _dtFilters = Nothing
    , _dtNextToken = Nothing
    , _dtDryRun = Nothing
    , _dtMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   'key' - The tag key.
--
-- -   'resource-id' - The resource ID.
--
-- -   'resource-type' - The resource type ('customer-gateway' |
--     'dhcp-options' | 'image' | 'instance' | 'internet-gateway' |
--     'network-acl' | 'network-interface' | 'reserved-instances' |
--     'route-table' | 'security-group' | 'snapshot' |
--     'spot-instances-request' | 'subnet' | 'volume' | 'vpc' |
--     'vpn-connection' | 'vpn-gateway').
--
-- -   'value' - The tag value.
--
dtFilters :: Lens' DescribeTags [Filter]
dtFilters = lens _dtFilters (\ s a -> s{_dtFilters = a}) . _Default . _Coerce;

-- | The token to retrieve the next page of results.
dtNextToken :: Lens' DescribeTags (Maybe Text)
dtNextToken = lens _dtNextToken (\ s a -> s{_dtNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dtDryRun :: Lens' DescribeTags (Maybe Bool)
dtDryRun = lens _dtDryRun (\ s a -> s{_dtDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned 'NextToken' value. This value
-- can be between 5 and 1000; if 'MaxResults' is given a value larger than
-- 1000, only 1000 results are returned.
dtMaxResults :: Lens' DescribeTags (Maybe Int)
dtMaxResults = lens _dtMaxResults (\ s a -> s{_dtMaxResults = a});

instance AWSPager DescribeTags where
        page rq rs
          | stop (rs ^. dtrsNextToken) = Nothing
          | stop (rs ^. dtrsTags) = Nothing
          | otherwise =
            Just $ rq & dtNextToken .~ rs ^. dtrsNextToken

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = postQuery eC2
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
               toQuery (toQueryList "Filter" <$> _dtFilters),
               "NextToken" =: _dtNextToken, "DryRun" =: _dtDryRun,
               "MaxResults" =: _dtMaxResults]

-- | /See:/ 'describeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrsNextToken :: !(Maybe Text)
    , _dtrsTags      :: !(Maybe [TagDescription])
    , _dtrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsNextToken'
--
-- * 'dtrsTags'
--
-- * 'dtrsStatus'
describeTagsResponse
    :: Int -- ^ 'dtrsStatus'
    -> DescribeTagsResponse
describeTagsResponse pStatus_ =
    DescribeTagsResponse'
    { _dtrsNextToken = Nothing
    , _dtrsTags = Nothing
    , _dtrsStatus = pStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- 'null' when there are no more results to return..
dtrsNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextToken = lens _dtrsNextToken (\ s a -> s{_dtrsNextToken = a});

-- | A list of tags.
dtrsTags :: Lens' DescribeTagsResponse [TagDescription]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Default . _Coerce;

-- | The response status code.
dtrsStatus :: Lens' DescribeTagsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
