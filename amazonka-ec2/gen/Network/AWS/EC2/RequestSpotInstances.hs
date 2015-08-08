{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Instance request. Spot Instances are instances that
-- Amazon EC2 launches when the bid price that you specify exceeds the
-- current Spot Price. Amazon EC2 periodically sets the Spot Price based on
-- available Spot Instance capacity and current Spot Instance requests. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance Requests>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotInstances.html AWS API Reference> for RequestSpotInstances.
module Network.AWS.EC2.RequestSpotInstances
    (
    -- * Creating a Request
      RequestSpotInstances
    , requestSpotInstances
    -- * Request Lenses
    , rsiInstanceCount
    , rsiClientToken
    , rsiAvailabilityZoneGroup
    , rsiLaunchSpecification
    , rsiValidUntil
    , rsiLaunchGroup
    , rsiType
    , rsiValidFrom
    , rsiDryRun
    , rsiSpotPrice

    -- * Destructuring the Response
    , RequestSpotInstancesResponse
    , requestSpotInstancesResponse
    -- * Response Lenses
    , rsirsSpotInstanceRequests
    , rsirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RequestSpotInstances.
--
-- /See:/ 'requestSpotInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsiInstanceCount'
--
-- * 'rsiClientToken'
--
-- * 'rsiAvailabilityZoneGroup'
--
-- * 'rsiLaunchSpecification'
--
-- * 'rsiValidUntil'
--
-- * 'rsiLaunchGroup'
--
-- * 'rsiType'
--
-- * 'rsiValidFrom'
--
-- * 'rsiDryRun'
--
-- * 'rsiSpotPrice'
data RequestSpotInstances = RequestSpotInstances'
    { _rsiInstanceCount         :: !(Maybe Int)
    , _rsiClientToken           :: !(Maybe Text)
    , _rsiAvailabilityZoneGroup :: !(Maybe Text)
    , _rsiLaunchSpecification   :: !(Maybe RequestSpotLaunchSpecification)
    , _rsiValidUntil            :: !(Maybe ISO8601)
    , _rsiLaunchGroup           :: !(Maybe Text)
    , _rsiType                  :: !(Maybe SpotInstanceType)
    , _rsiValidFrom             :: !(Maybe ISO8601)
    , _rsiDryRun                :: !(Maybe Bool)
    , _rsiSpotPrice             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestSpotInstances' smart constructor.
requestSpotInstances :: Text -> RequestSpotInstances
requestSpotInstances pSpotPrice_ =
    RequestSpotInstances'
    { _rsiInstanceCount = Nothing
    , _rsiClientToken = Nothing
    , _rsiAvailabilityZoneGroup = Nothing
    , _rsiLaunchSpecification = Nothing
    , _rsiValidUntil = Nothing
    , _rsiLaunchGroup = Nothing
    , _rsiType = Nothing
    , _rsiValidFrom = Nothing
    , _rsiDryRun = Nothing
    , _rsiSpotPrice = pSpotPrice_
    }

-- | The maximum number of Spot Instances to launch.
--
-- Default: 1
rsiInstanceCount :: Lens' RequestSpotInstances (Maybe Int)
rsiInstanceCount = lens _rsiInstanceCount (\ s a -> s{_rsiInstanceCount = a});

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon Elastic Compute Cloud User Guide/.
rsiClientToken :: Lens' RequestSpotInstances (Maybe Text)
rsiClientToken = lens _rsiClientToken (\ s a -> s{_rsiClientToken = a});

-- | The user-specified name for a logical grouping of bids.
--
-- When you specify an Availability Zone group in a Spot Instance request,
-- all Spot Instances in the request are launched in the same Availability
-- Zone. Instance proximity is maintained with this parameter, but the
-- choice of Availability Zone is not. The group applies only to bids for
-- Spot Instances of the same instance type. Any additional Spot Instance
-- requests that are specified with the same Availability Zone group name
-- are launched in that same Availability Zone, as long as at least one
-- instance from the group is still active.
--
-- If there is no active instance running in the Availability Zone group
-- that you specify for a new Spot Instance request (all instances are
-- terminated, the bid is expired, or the bid falls below current market),
-- then Amazon EC2 launches the instance in any Availability Zone where the
-- constraint can be met. Consequently, the subsequent set of Spot
-- Instances could be placed in a different zone from the original request,
-- even if you specified the same Availability Zone group.
--
-- Default: Instances are launched in any available Availability Zone.
rsiAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiAvailabilityZoneGroup = lens _rsiAvailabilityZoneGroup (\ s a -> s{_rsiAvailabilityZoneGroup = a});

-- | Undocumented member.
rsiLaunchSpecification :: Lens' RequestSpotInstances (Maybe RequestSpotLaunchSpecification)
rsiLaunchSpecification = lens _rsiLaunchSpecification (\ s a -> s{_rsiLaunchSpecification = a});

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or
-- this date is reached. If the request is persistent, it remains active
-- until it is canceled or this date and time is reached.
--
-- Default: The request is effective indefinitely.
rsiValidUntil :: Lens' RequestSpotInstances (Maybe UTCTime)
rsiValidUntil = lens _rsiValidUntil (\ s a -> s{_rsiValidUntil = a}) . mapping _Time;

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
rsiLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiLaunchGroup = lens _rsiLaunchGroup (\ s a -> s{_rsiLaunchGroup = a});

-- | The Spot Instance request type.
--
-- Default: @one-time@
rsiType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rsiType = lens _rsiType (\ s a -> s{_rsiType = a});

-- | The start date of the request. If this is a one-time request, the
-- request becomes active at this date and time and remains active until
-- all instances launch, the request expires, or the request is canceled.
-- If the request is persistent, the request becomes active at this date
-- and time and remains active until it expires or is canceled.
--
-- Default: The request is effective indefinitely.
rsiValidFrom :: Lens' RequestSpotInstances (Maybe UTCTime)
rsiValidFrom = lens _rsiValidFrom (\ s a -> s{_rsiValidFrom = a}) . mapping _Time;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsiDryRun :: Lens' RequestSpotInstances (Maybe Bool)
rsiDryRun = lens _rsiDryRun (\ s a -> s{_rsiDryRun = a});

-- | The maximum hourly price (bid) for any Spot Instance launched to fulfill
-- the request.
rsiSpotPrice :: Lens' RequestSpotInstances Text
rsiSpotPrice = lens _rsiSpotPrice (\ s a -> s{_rsiSpotPrice = a});

instance AWSRequest RequestSpotInstances where
        type Sv RequestSpotInstances = EC2
        type Rs RequestSpotInstances =
             RequestSpotInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 RequestSpotInstancesResponse' <$>
                   (x .@? "spotInstanceRequestSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders RequestSpotInstances where
        toHeaders = const mempty

instance ToPath RequestSpotInstances where
        toPath = const "/"

instance ToQuery RequestSpotInstances where
        toQuery RequestSpotInstances'{..}
          = mconcat
              ["Action" =: ("RequestSpotInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceCount" =: _rsiInstanceCount,
               "ClientToken" =: _rsiClientToken,
               "AvailabilityZoneGroup" =: _rsiAvailabilityZoneGroup,
               "LaunchSpecification" =: _rsiLaunchSpecification,
               "ValidUntil" =: _rsiValidUntil,
               "LaunchGroup" =: _rsiLaunchGroup, "Type" =: _rsiType,
               "ValidFrom" =: _rsiValidFrom, "DryRun" =: _rsiDryRun,
               "SpotPrice" =: _rsiSpotPrice]

-- | Contains the output of RequestSpotInstances.
--
-- /See:/ 'requestSpotInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsirsSpotInstanceRequests'
--
-- * 'rsirsStatus'
data RequestSpotInstancesResponse = RequestSpotInstancesResponse'
    { _rsirsSpotInstanceRequests :: !(Maybe [SpotInstanceRequest])
    , _rsirsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestSpotInstancesResponse' smart constructor.
requestSpotInstancesResponse :: Int -> RequestSpotInstancesResponse
requestSpotInstancesResponse pStatus_ =
    RequestSpotInstancesResponse'
    { _rsirsSpotInstanceRequests = Nothing
    , _rsirsStatus = pStatus_
    }

-- | One or more Spot Instance requests.
rsirsSpotInstanceRequests :: Lens' RequestSpotInstancesResponse [SpotInstanceRequest]
rsirsSpotInstanceRequests = lens _rsirsSpotInstanceRequests (\ s a -> s{_rsirsSpotInstanceRequests = a}) . _Default . _Coerce;

-- | Undocumented member.
rsirsStatus :: Lens' RequestSpotInstancesResponse Int
rsirsStatus = lens _rsirsStatus (\ s a -> s{_rsirsStatus = a});
