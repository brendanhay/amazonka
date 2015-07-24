{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified load balancers.
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
    , dtrsTagDescriptions
    , dtrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtLoadBalancerNames'
newtype DescribeTags = DescribeTags'
    { _dtLoadBalancerNames :: List1 Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTags' smart constructor.
describeTags :: NonEmpty Text -> DescribeTags
describeTags pLoadBalancerNames_ =
    DescribeTags'
    { _dtLoadBalancerNames = _List1 # pLoadBalancerNames_
    }

-- | The names of the load balancers.
dtLoadBalancerNames :: Lens' DescribeTags (NonEmpty Text)
dtLoadBalancerNames = lens _dtLoadBalancerNames (\ s a -> s{_dtLoadBalancerNames = a}) . _List1;

instance AWSRequest DescribeTags where
        type Sv DescribeTags = ELB
        type Rs DescribeTags = DescribeTagsResponse
        request = post "DescribeTags"
        response
          = receiveXMLWrapper "DescribeTagsResult"
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "TagDescriptions" .!@ mempty >>=
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
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerNames" =:
                 toQueryList "member" _dtLoadBalancerNames]

-- | /See:/ 'describeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrsTagDescriptions'
--
-- * 'dtrsStatus'
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrsTagDescriptions :: !(Maybe [TagDescription])
    , _dtrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: Int -> DescribeTagsResponse
describeTagsResponse pStatus_ =
    DescribeTagsResponse'
    { _dtrsTagDescriptions = Nothing
    , _dtrsStatus = pStatus_
    }

-- | Information about the tags.
dtrsTagDescriptions :: Lens' DescribeTagsResponse [TagDescription]
dtrsTagDescriptions = lens _dtrsTagDescriptions (\ s a -> s{_dtrsTagDescriptions = a}) . _Default;

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DescribeTagsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
