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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified load balancers.
--
--
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

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeTags.
--
--
--
-- /See:/ 'describeTags' smart constructor.
newtype DescribeTags = DescribeTags'
  { _dtLoadBalancerNames :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtLoadBalancerNames' - The names of the load balancers.
describeTags
    :: NonEmpty Text -- ^ 'dtLoadBalancerNames'
    -> DescribeTags
describeTags pLoadBalancerNames_ =
  DescribeTags' {_dtLoadBalancerNames = _List1 # pLoadBalancerNames_}


-- | The names of the load balancers.
dtLoadBalancerNames :: Lens' DescribeTags (NonEmpty Text)
dtLoadBalancerNames = lens _dtLoadBalancerNames (\ s a -> s{_dtLoadBalancerNames = a}) . _List1

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "DescribeTagsResult"
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .@? "TagDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTags where

instance NFData DescribeTags where

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

-- | Contains the output for DescribeTags.
--
--
--
-- /See:/ 'describeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { _dtrsTagDescriptions :: !(Maybe [TagDescription])
  , _dtrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsTagDescriptions' - Information about the tags.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
describeTagsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeTagsResponse
describeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    {_dtrsTagDescriptions = Nothing, _dtrsResponseStatus = pResponseStatus_}


-- | Information about the tags.
dtrsTagDescriptions :: Lens' DescribeTagsResponse [TagDescription]
dtrsTagDescriptions = lens _dtrsTagDescriptions (\ s a -> s{_dtrsTagDescriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeTagsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DescribeTagsResponse where
