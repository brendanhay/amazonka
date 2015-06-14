{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.DescribeTags
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

-- | Describes the tags associated with the specified load balancers.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeTags.html>
module Network.AWS.ELB.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtLoadBalancerNames

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrTagDescriptions
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ELB.Types

-- | /See:/ 'describeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtLoadBalancerNames'
newtype DescribeTags = DescribeTags'{_dtLoadBalancerNames :: List1 Text} deriving (Eq, Read, Show)

-- | 'DescribeTags' smart constructor.
describeTags :: NonEmpty Text -> DescribeTags
describeTags pLoadBalancerNames = DescribeTags'{_dtLoadBalancerNames = _List1 # pLoadBalancerNames};

-- | The names of the load balancers.
dtLoadBalancerNames :: Lens' DescribeTags (NonEmpty Text)
dtLoadBalancerNames = lens _dtLoadBalancerNames (\ s a -> s{_dtLoadBalancerNames = a}) . _List1;

instance AWSRequest DescribeTags where
        type Sv DescribeTags = ELB
        type Rs DescribeTags = DescribeTagsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeTagsResult"
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "TagDescriptions" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath = const "/"

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["Action" =: ("DescribeTags" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerNames" =:
                 "member" =: _dtLoadBalancerNames]

-- | /See:/ 'describeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTagDescriptions'
newtype DescribeTagsResponse = DescribeTagsResponse'{_dtrTagDescriptions :: [TagDescription]} deriving (Eq, Read, Show)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: DescribeTagsResponse
describeTagsResponse = DescribeTagsResponse'{_dtrTagDescriptions = mempty};

-- | Information about the tags.
dtrTagDescriptions :: Lens' DescribeTagsResponse [TagDescription]
dtrTagDescriptions = lens _dtrTagDescriptions (\ s a -> s{_dtrTagDescriptions = a});
