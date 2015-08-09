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
-- Module      : Network.AWS.EC2.DescribeSpotFleetInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified Spot fleet.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetInstances.html AWS API Reference> for DescribeSpotFleetInstances.
module Network.AWS.EC2.DescribeSpotFleetInstances
    (
    -- * Creating a Request
      DescribeSpotFleetInstances
    , describeSpotFleetInstances
    -- * Request Lenses
    , dsfiNextToken
    , dsfiDryRun
    , dsfiMaxResults
    , dsfiSpotFleetRequestId

    -- * Destructuring the Response
    , DescribeSpotFleetInstancesResponse
    , describeSpotFleetInstancesResponse
    -- * Response Lenses
    , dsfirsNextToken
    , dsfirsStatus
    , dsfirsSpotFleetRequestId
    , dsfirsActiveInstances
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotFleetInstances.
--
-- /See:/ 'describeSpotFleetInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfiNextToken'
--
-- * 'dsfiDryRun'
--
-- * 'dsfiMaxResults'
--
-- * 'dsfiSpotFleetRequestId'
data DescribeSpotFleetInstances = DescribeSpotFleetInstances'
    { _dsfiNextToken          :: !(Maybe Text)
    , _dsfiDryRun             :: !(Maybe Bool)
    , _dsfiMaxResults         :: !(Maybe Int)
    , _dsfiSpotFleetRequestId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotFleetInstances' smart constructor.
describeSpotFleetInstances :: Text -> DescribeSpotFleetInstances
describeSpotFleetInstances pSpotFleetRequestId_ =
    DescribeSpotFleetInstances'
    { _dsfiNextToken = Nothing
    , _dsfiDryRun = Nothing
    , _dsfiMaxResults = Nothing
    , _dsfiSpotFleetRequestId = pSpotFleetRequestId_
    }

-- | The token for the next set of results.
dsfiNextToken :: Lens' DescribeSpotFleetInstances (Maybe Text)
dsfiNextToken = lens _dsfiNextToken (\ s a -> s{_dsfiNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsfiDryRun :: Lens' DescribeSpotFleetInstances (Maybe Bool)
dsfiDryRun = lens _dsfiDryRun (\ s a -> s{_dsfiDryRun = a});

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
dsfiMaxResults :: Lens' DescribeSpotFleetInstances (Maybe Int)
dsfiMaxResults = lens _dsfiMaxResults (\ s a -> s{_dsfiMaxResults = a});

-- | The ID of the Spot fleet request.
dsfiSpotFleetRequestId :: Lens' DescribeSpotFleetInstances Text
dsfiSpotFleetRequestId = lens _dsfiSpotFleetRequestId (\ s a -> s{_dsfiSpotFleetRequestId = a});

instance AWSRequest DescribeSpotFleetInstances where
        type Sv DescribeSpotFleetInstances = EC2
        type Rs DescribeSpotFleetInstances =
             DescribeSpotFleetInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotFleetInstancesResponse' <$>
                   (x .@? "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .@ "spotFleetRequestId")
                     <*>
                     (x .@? "activeInstanceSet" .!@ mempty >>=
                        parseXMLList "item"))

instance ToHeaders DescribeSpotFleetInstances where
        toHeaders = const mempty

instance ToPath DescribeSpotFleetInstances where
        toPath = const "/"

instance ToQuery DescribeSpotFleetInstances where
        toQuery DescribeSpotFleetInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotFleetInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NextToken" =: _dsfiNextToken,
               "DryRun" =: _dsfiDryRun,
               "MaxResults" =: _dsfiMaxResults,
               "SpotFleetRequestId" =: _dsfiSpotFleetRequestId]

-- | Contains the output of DescribeSpotFleetInstances.
--
-- /See:/ 'describeSpotFleetInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfirsNextToken'
--
-- * 'dsfirsStatus'
--
-- * 'dsfirsSpotFleetRequestId'
--
-- * 'dsfirsActiveInstances'
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
    { _dsfirsNextToken          :: !(Maybe Text)
    , _dsfirsStatus             :: !Int
    , _dsfirsSpotFleetRequestId :: !Text
    , _dsfirsActiveInstances    :: ![ActiveInstance]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotFleetInstancesResponse' smart constructor.
describeSpotFleetInstancesResponse :: Int -> Text -> DescribeSpotFleetInstancesResponse
describeSpotFleetInstancesResponse pStatus_ pSpotFleetRequestId_ =
    DescribeSpotFleetInstancesResponse'
    { _dsfirsNextToken = Nothing
    , _dsfirsStatus = pStatus_
    , _dsfirsSpotFleetRequestId = pSpotFleetRequestId_
    , _dsfirsActiveInstances = mempty
    }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
dsfirsNextToken :: Lens' DescribeSpotFleetInstancesResponse (Maybe Text)
dsfirsNextToken = lens _dsfirsNextToken (\ s a -> s{_dsfirsNextToken = a});

-- | Undocumented member.
dsfirsStatus :: Lens' DescribeSpotFleetInstancesResponse Int
dsfirsStatus = lens _dsfirsStatus (\ s a -> s{_dsfirsStatus = a});

-- | The ID of the Spot fleet request.
dsfirsSpotFleetRequestId :: Lens' DescribeSpotFleetInstancesResponse Text
dsfirsSpotFleetRequestId = lens _dsfirsSpotFleetRequestId (\ s a -> s{_dsfirsSpotFleetRequestId = a});

-- | The running instances. Note that this list is refreshed periodically and
-- might be out of date.
dsfirsActiveInstances :: Lens' DescribeSpotFleetInstancesResponse [ActiveInstance]
dsfirsActiveInstances = lens _dsfirsActiveInstances (\ s a -> s{_dsfirsActiveInstances = a}) . _Coerce;
