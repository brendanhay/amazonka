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
-- Module      : Network.AWS.ELBv2.DescribeSSLPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies or all policies used for SSL negotiation.
--
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security Policies> in the /Application Load Balancers Guide/ .
--
module Network.AWS.ELBv2.DescribeSSLPolicies
    (
    -- * Creating a Request
      describeSSLPolicies
    , DescribeSSLPolicies
    -- * Request Lenses
    , dspNames
    , dspMarker
    , dspPageSize

    -- * Destructuring the Response
    , describeSSLPoliciesResponse
    , DescribeSSLPoliciesResponse
    -- * Response Lenses
    , dsprsSSLPolicies
    , dsprsNextMarker
    , dsprsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSSLPolicies' smart constructor.
data DescribeSSLPolicies = DescribeSSLPolicies'
  { _dspNames    :: !(Maybe [Text])
  , _dspMarker   :: !(Maybe Text)
  , _dspPageSize :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSSLPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspNames' - The names of the policies.
--
-- * 'dspMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dspPageSize' - The maximum number of results to return with this call.
describeSSLPolicies
    :: DescribeSSLPolicies
describeSSLPolicies =
  DescribeSSLPolicies'
    {_dspNames = Nothing, _dspMarker = Nothing, _dspPageSize = Nothing}


-- | The names of the policies.
dspNames :: Lens' DescribeSSLPolicies [Text]
dspNames = lens _dspNames (\ s a -> s{_dspNames = a}) . _Default . _Coerce

-- | The marker for the next set of results. (You received this marker from a previous call.)
dspMarker :: Lens' DescribeSSLPolicies (Maybe Text)
dspMarker = lens _dspMarker (\ s a -> s{_dspMarker = a})

-- | The maximum number of results to return with this call.
dspPageSize :: Lens' DescribeSSLPolicies (Maybe Natural)
dspPageSize = lens _dspPageSize (\ s a -> s{_dspPageSize = a}) . mapping _Nat

instance AWSRequest DescribeSSLPolicies where
        type Rs DescribeSSLPolicies =
             DescribeSSLPoliciesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DescribeSSLPoliciesResult"
              (\ s h x ->
                 DescribeSSLPoliciesResponse' <$>
                   (x .@? "SslPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSSLPolicies where

instance NFData DescribeSSLPolicies where

instance ToHeaders DescribeSSLPolicies where
        toHeaders = const mempty

instance ToPath DescribeSSLPolicies where
        toPath = const "/"

instance ToQuery DescribeSSLPolicies where
        toQuery DescribeSSLPolicies'{..}
          = mconcat
              ["Action" =: ("DescribeSSLPolicies" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "Names" =:
                 toQuery (toQueryList "member" <$> _dspNames),
               "Marker" =: _dspMarker, "PageSize" =: _dspPageSize]

-- | /See:/ 'describeSSLPoliciesResponse' smart constructor.
data DescribeSSLPoliciesResponse = DescribeSSLPoliciesResponse'
  { _dsprsSSLPolicies    :: !(Maybe [SSLPolicy])
  , _dsprsNextMarker     :: !(Maybe Text)
  , _dsprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSSLPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprsSSLPolicies' - Information about the policies.
--
-- * 'dsprsNextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dsprsResponseStatus' - -- | The response status code.
describeSSLPoliciesResponse
    :: Int -- ^ 'dsprsResponseStatus'
    -> DescribeSSLPoliciesResponse
describeSSLPoliciesResponse pResponseStatus_ =
  DescribeSSLPoliciesResponse'
    { _dsprsSSLPolicies = Nothing
    , _dsprsNextMarker = Nothing
    , _dsprsResponseStatus = pResponseStatus_
    }


-- | Information about the policies.
dsprsSSLPolicies :: Lens' DescribeSSLPoliciesResponse [SSLPolicy]
dsprsSSLPolicies = lens _dsprsSSLPolicies (\ s a -> s{_dsprsSSLPolicies = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dsprsNextMarker :: Lens' DescribeSSLPoliciesResponse (Maybe Text)
dsprsNextMarker = lens _dsprsNextMarker (\ s a -> s{_dsprsNextMarker = a})

-- | -- | The response status code.
dsprsResponseStatus :: Lens' DescribeSSLPoliciesResponse Int
dsprsResponseStatus = lens _dsprsResponseStatus (\ s a -> s{_dsprsResponseStatus = a})

instance NFData DescribeSSLPoliciesResponse where
