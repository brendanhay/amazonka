{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpointServices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Describes all supported AWS services that can be specified when creating
-- a VPC endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCEndpointServices.html>
module Network.AWS.EC2.DescribeVPCEndpointServices
    (
    -- * Request
      DescribeVPCEndpointServices
    -- ** Request constructor
    , describeVPCEndpointServices
    -- ** Request lenses
    , dvesNextToken
    , dvesDryRun
    , dvesMaxResults

    -- * Response
    , DescribeVPCEndpointServicesResponse
    -- ** Response constructor
    , describeVPCEndpointServicesResponse
    -- ** Response lenses
    , dvesrServiceNames
    , dvesrNextToken
    , dvesrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCEndpointServices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvesNextToken'
--
-- * 'dvesDryRun'
--
-- * 'dvesMaxResults'
data DescribeVPCEndpointServices = DescribeVPCEndpointServices'
    { _dvesNextToken  :: !(Maybe Text)
    , _dvesDryRun     :: !(Maybe Bool)
    , _dvesMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCEndpointServices' smart constructor.
describeVPCEndpointServices :: DescribeVPCEndpointServices
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
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
        request = post
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
                 ("DescribeVPCEndpointServices" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NextToken" =: _dvesNextToken,
               "DryRun" =: _dvesDryRun,
               "MaxResults" =: _dvesMaxResults]

-- | /See:/ 'describeVPCEndpointServicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvesrServiceNames'
--
-- * 'dvesrNextToken'
--
-- * 'dvesrStatus'
data DescribeVPCEndpointServicesResponse = DescribeVPCEndpointServicesResponse'
    { _dvesrServiceNames :: !(Maybe [Text])
    , _dvesrNextToken    :: !(Maybe Text)
    , _dvesrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCEndpointServicesResponse' smart constructor.
describeVPCEndpointServicesResponse :: Int -> DescribeVPCEndpointServicesResponse
describeVPCEndpointServicesResponse pStatus =
    DescribeVPCEndpointServicesResponse'
    { _dvesrServiceNames = Nothing
    , _dvesrNextToken = Nothing
    , _dvesrStatus = pStatus
    }

-- | A list of supported AWS services.
dvesrServiceNames :: Lens' DescribeVPCEndpointServicesResponse [Text]
dvesrServiceNames = lens _dvesrServiceNames (\ s a -> s{_dvesrServiceNames = a}) . _Default;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dvesrNextToken :: Lens' DescribeVPCEndpointServicesResponse (Maybe Text)
dvesrNextToken = lens _dvesrNextToken (\ s a -> s{_dvesrNextToken = a});

-- | FIXME: Undocumented member.
dvesrStatus :: Lens' DescribeVPCEndpointServicesResponse Int
dvesrStatus = lens _dvesrStatus (\ s a -> s{_dvesrStatus = a});
