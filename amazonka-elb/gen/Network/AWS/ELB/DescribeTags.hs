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
-- Module      : Network.AWS.ELB.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified load balancers.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeTags.html AWS API Reference> for DescribeTags.
module Network.AWS.ELB.DescribeTags
    (
    -- * Creating a Request
      describeTags
    , DescribeTags
    -- * Request Lenses
    , dtLoadBalancerNames

    -- * Destructuring the Response
    , describeTagsResponse
    , DescribeTagsResponse
    -- * Response Lenses
    , dtrsTagDescriptions
    , dtrsResponseStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
newtype DescribeTags = DescribeTags'
    { _dtLoadBalancerNames :: List1 Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtLoadBalancerNames'
describeTags
    :: NonEmpty Text -- ^ 'dtLoadBalancerNames'
    -> DescribeTags
describeTags pLoadBalancerNames_ =
    DescribeTags'
    { _dtLoadBalancerNames = _List1 # pLoadBalancerNames_
    }

-- | The names of the load balancers.
dtLoadBalancerNames :: Lens' DescribeTags (NonEmpty Text)
dtLoadBalancerNames = lens _dtLoadBalancerNames (\ s a -> s{_dtLoadBalancerNames = a}) . _List1;

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = postQuery eLB
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
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrsTagDescriptions :: !(Maybe [TagDescription])
    , _dtrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsTagDescriptions'
--
-- * 'dtrsResponseStatus'
describeTagsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeTagsResponse
describeTagsResponse pResponseStatus_ =
    DescribeTagsResponse'
    { _dtrsTagDescriptions = Nothing
    , _dtrsResponseStatus = pResponseStatus_
    }

-- | Information about the tags.
dtrsTagDescriptions :: Lens' DescribeTagsResponse [TagDescription]
dtrsTagDescriptions = lens _dtrsTagDescriptions (\ s a -> s{_dtrsTagDescriptions = a}) . _Default . _Coerce;

-- | The response status code.
dtrsResponseStatus :: Lens' DescribeTagsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a});
