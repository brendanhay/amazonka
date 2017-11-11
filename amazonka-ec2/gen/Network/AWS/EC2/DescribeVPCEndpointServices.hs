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
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServices
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all supported AWS services that can be specified when creating a VPC endpoint.
--
--
module Network.AWS.EC2.DescribeVPCEndpointServices
    (
    -- * Creating a Request
      describeVPCEndpointServices
    , DescribeVPCEndpointServices
    -- * Request Lenses
    , dvesNextToken
    , dvesDryRun
    , dvesMaxResults

    -- * Destructuring the Response
    , describeVPCEndpointServicesResponse
    , DescribeVPCEndpointServicesResponse
    -- * Response Lenses
    , dvesrsServiceNames
    , dvesrsNextToken
    , dvesrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeVpcEndpointServices.
--
--
--
-- /See:/ 'describeVPCEndpointServices' smart constructor.
data DescribeVPCEndpointServices = DescribeVPCEndpointServices'
  { _dvesNextToken  :: {-# NOUNPACK #-}!(Maybe Text)
  , _dvesDryRun     :: {-# NOUNPACK #-}!(Maybe Bool)
  , _dvesMaxResults :: {-# NOUNPACK #-}!(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvesNextToken' - The token for the next set of items to return. (You received this token from a prior call.)
--
-- * 'dvesDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvesMaxResults' - The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. Constraint: If the value is greater than 1000, we return only 1000 items.
describeVPCEndpointServices
    :: DescribeVPCEndpointServices
describeVPCEndpointServices =
  DescribeVPCEndpointServices'
  {_dvesNextToken = Nothing, _dvesDryRun = Nothing, _dvesMaxResults = Nothing}


-- | The token for the next set of items to return. (You received this token from a prior call.)
dvesNextToken :: Lens' DescribeVPCEndpointServices (Maybe Text)
dvesNextToken = lens _dvesNextToken (\ s a -> s{_dvesNextToken = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvesDryRun :: Lens' DescribeVPCEndpointServices (Maybe Bool)
dvesDryRun = lens _dvesDryRun (\ s a -> s{_dvesDryRun = a});

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results. Constraint: If the value is greater than 1000, we return only 1000 items.
dvesMaxResults :: Lens' DescribeVPCEndpointServices (Maybe Int)
dvesMaxResults = lens _dvesMaxResults (\ s a -> s{_dvesMaxResults = a});

instance AWSRequest DescribeVPCEndpointServices where
        type Rs DescribeVPCEndpointServices =
             DescribeVPCEndpointServicesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointServicesResponse' <$>
                   (x .@? "serviceNameSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVPCEndpointServices where

instance NFData DescribeVPCEndpointServices where

instance ToHeaders DescribeVPCEndpointServices where
        toHeaders = const mempty

instance ToPath DescribeVPCEndpointServices where
        toPath = const "/"

instance ToQuery DescribeVPCEndpointServices where
        toQuery DescribeVPCEndpointServices'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcEndpointServices" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NextToken" =: _dvesNextToken,
               "DryRun" =: _dvesDryRun,
               "MaxResults" =: _dvesMaxResults]

-- | Contains the output of DescribeVpcEndpointServices.
--
--
--
-- /See:/ 'describeVPCEndpointServicesResponse' smart constructor.
data DescribeVPCEndpointServicesResponse = DescribeVPCEndpointServicesResponse'
  { _dvesrsServiceNames   :: {-# NOUNPACK #-}!(Maybe [Text])
  , _dvesrsNextToken      :: {-# NOUNPACK #-}!(Maybe Text)
  , _dvesrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvesrsServiceNames' - A list of supported AWS services.
--
-- * 'dvesrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dvesrsResponseStatus' - -- | The response status code.
describeVPCEndpointServicesResponse
    :: Int -- ^ 'dvesrsResponseStatus'
    -> DescribeVPCEndpointServicesResponse
describeVPCEndpointServicesResponse pResponseStatus_ =
  DescribeVPCEndpointServicesResponse'
  { _dvesrsServiceNames = Nothing
  , _dvesrsNextToken = Nothing
  , _dvesrsResponseStatus = pResponseStatus_
  }


-- | A list of supported AWS services.
dvesrsServiceNames :: Lens' DescribeVPCEndpointServicesResponse [Text]
dvesrsServiceNames = lens _dvesrsServiceNames (\ s a -> s{_dvesrsServiceNames = a}) . _Default . _Coerce;

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dvesrsNextToken :: Lens' DescribeVPCEndpointServicesResponse (Maybe Text)
dvesrsNextToken = lens _dvesrsNextToken (\ s a -> s{_dvesrsNextToken = a});

-- | -- | The response status code.
dvesrsResponseStatus :: Lens' DescribeVPCEndpointServicesResponse Int
dvesrsResponseStatus = lens _dvesrsResponseStatus (\ s a -> s{_dvesrsResponseStatus = a});

instance NFData DescribeVPCEndpointServicesResponse
         where
