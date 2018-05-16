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
-- Module      : Network.AWS.ELB.DescribeAccountLimits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Elastic Load Balancing resource limits for your AWS account.
--
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html Limits for Your Classic Load Balancer> in the /Classic Load Balancer Guide/ .
--
module Network.AWS.ELB.DescribeAccountLimits
    (
    -- * Creating a Request
      describeAccountLimits
    , DescribeAccountLimits
    -- * Request Lenses
    , dalMarker
    , dalPageSize

    -- * Destructuring the Response
    , describeAccountLimitsResponse
    , DescribeAccountLimitsResponse
    -- * Response Lenses
    , dalrsLimits
    , dalrsNextMarker
    , dalrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  { _dalMarker   :: !(Maybe Text)
  , _dalPageSize :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dalPageSize' - The maximum number of results to return with this call.
describeAccountLimits
    :: DescribeAccountLimits
describeAccountLimits =
  DescribeAccountLimits' {_dalMarker = Nothing, _dalPageSize = Nothing}


-- | The marker for the next set of results. (You received this marker from a previous call.)
dalMarker :: Lens' DescribeAccountLimits (Maybe Text)
dalMarker = lens _dalMarker (\ s a -> s{_dalMarker = a})

-- | The maximum number of results to return with this call.
dalPageSize :: Lens' DescribeAccountLimits (Maybe Natural)
dalPageSize = lens _dalPageSize (\ s a -> s{_dalPageSize = a}) . mapping _Nat

instance AWSRequest DescribeAccountLimits where
        type Rs DescribeAccountLimits =
             DescribeAccountLimitsResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "DescribeAccountLimitsResult"
              (\ s h x ->
                 DescribeAccountLimitsResponse' <$>
                   (x .@? "Limits" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAccountLimits where

instance NFData DescribeAccountLimits where

instance ToHeaders DescribeAccountLimits where
        toHeaders = const mempty

instance ToPath DescribeAccountLimits where
        toPath = const "/"

instance ToQuery DescribeAccountLimits where
        toQuery DescribeAccountLimits'{..}
          = mconcat
              ["Action" =: ("DescribeAccountLimits" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "Marker" =: _dalMarker, "PageSize" =: _dalPageSize]

-- | /See:/ 'describeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { _dalrsLimits         :: !(Maybe [Limit])
  , _dalrsNextMarker     :: !(Maybe Text)
  , _dalrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalrsLimits' - Information about the limits.
--
-- * 'dalrsNextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dalrsResponseStatus' - -- | The response status code.
describeAccountLimitsResponse
    :: Int -- ^ 'dalrsResponseStatus'
    -> DescribeAccountLimitsResponse
describeAccountLimitsResponse pResponseStatus_ =
  DescribeAccountLimitsResponse'
    { _dalrsLimits = Nothing
    , _dalrsNextMarker = Nothing
    , _dalrsResponseStatus = pResponseStatus_
    }


-- | Information about the limits.
dalrsLimits :: Lens' DescribeAccountLimitsResponse [Limit]
dalrsLimits = lens _dalrsLimits (\ s a -> s{_dalrsLimits = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dalrsNextMarker :: Lens' DescribeAccountLimitsResponse (Maybe Text)
dalrsNextMarker = lens _dalrsNextMarker (\ s a -> s{_dalrsNextMarker = a})

-- | -- | The response status code.
dalrsResponseStatus :: Lens' DescribeAccountLimitsResponse Int
dalrsResponseStatus = lens _dalrsResponseStatus (\ s a -> s{_dalrsResponseStatus = a})

instance NFData DescribeAccountLimitsResponse where
