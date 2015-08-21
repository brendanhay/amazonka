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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all supported AWS services that can be specified when creating
-- a VPC endpoint.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCEndpointServices.html AWS API Reference> for DescribeVPCEndpointServices.
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
    , dvesrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCEndpointServices' smart constructor.
data DescribeVPCEndpointServices = DescribeVPCEndpointServices'
    { _dvesNextToken  :: !(Maybe Text)
    , _dvesDryRun     :: !(Maybe Bool)
    , _dvesMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeVPCEndpointServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvesNextToken'
--
-- * 'dvesDryRun'
--
-- * 'dvesMaxResults'
describeVPCEndpointServices
    :: DescribeVPCEndpointServices
describeVPCEndpointServices =
    DescribeVPCEndpointServices'
    { _dvesNextToken = Nothing
    , _dvesDryRun = Nothing
    , _dvesMaxResults = Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
dvesNextToken :: Lens' DescribeVPCEndpointServices (Maybe Text)
dvesNextToken = lens _dvesNextToken (\ s a -> s{_dvesNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dvesDryRun :: Lens' DescribeVPCEndpointServices (Maybe Bool)
dvesDryRun = lens _dvesDryRun (\ s a -> s{_dvesDryRun = a});

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
dvesMaxResults :: Lens' DescribeVPCEndpointServices (Maybe Int)
dvesMaxResults = lens _dvesMaxResults (\ s a -> s{_dvesMaxResults = a});

instance AWSRequest DescribeVPCEndpointServices where
        type Sv DescribeVPCEndpointServices = EC2
        type Rs DescribeVPCEndpointServices =
             DescribeVPCEndpointServicesResponse
        request = postQuery
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointServicesResponse' <$>
                   (x .@? "serviceNameSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVPCEndpointServices where
        toHeaders = const mempty

instance ToPath DescribeVPCEndpointServices where
        toPath = const "/"

instance ToQuery DescribeVPCEndpointServices where
        toQuery DescribeVPCEndpointServices'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcEndpointServices" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NextToken" =: _dvesNextToken,
               "DryRun" =: _dvesDryRun,
               "MaxResults" =: _dvesMaxResults]

-- | /See:/ 'describeVPCEndpointServicesResponse' smart constructor.
data DescribeVPCEndpointServicesResponse = DescribeVPCEndpointServicesResponse'
    { _dvesrsServiceNames :: !(Maybe [Text])
    , _dvesrsNextToken    :: !(Maybe Text)
    , _dvesrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeVPCEndpointServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvesrsServiceNames'
--
-- * 'dvesrsNextToken'
--
-- * 'dvesrsStatus'
describeVPCEndpointServicesResponse
    :: Int -- ^ 'dvesrsStatus'
    -> DescribeVPCEndpointServicesResponse
describeVPCEndpointServicesResponse pStatus_ =
    DescribeVPCEndpointServicesResponse'
    { _dvesrsServiceNames = Nothing
    , _dvesrsNextToken = Nothing
    , _dvesrsStatus = pStatus_
    }

-- | A list of supported AWS services.
dvesrsServiceNames :: Lens' DescribeVPCEndpointServicesResponse [Text]
dvesrsServiceNames = lens _dvesrsServiceNames (\ s a -> s{_dvesrsServiceNames = a}) . _Default . _Coerce;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dvesrsNextToken :: Lens' DescribeVPCEndpointServicesResponse (Maybe Text)
dvesrsNextToken = lens _dvesrsNextToken (\ s a -> s{_dvesrsNextToken = a});

-- | The response status code.
dvesrsStatus :: Lens' DescribeVPCEndpointServicesResponse Int
dvesrsStatus = lens _dvesrsStatus (\ s a -> s{_dvesrsStatus = a});
