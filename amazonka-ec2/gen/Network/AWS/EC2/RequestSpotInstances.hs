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
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotInstances.html>
module Network.AWS.EC2.RequestSpotInstances
    (
    -- * Request
      RequestSpotInstances
    -- ** Request constructor
    , requestSpotInstances
    -- ** Request lenses
    , rsirqInstanceCount
    , rsirqClientToken
    , rsirqAvailabilityZoneGroup
    , rsirqLaunchSpecification
    , rsirqValidUntil
    , rsirqLaunchGroup
    , rsirqType
    , rsirqValidFrom
    , rsirqDryRun
    , rsirqSpotPrice

    -- * Response
    , RequestSpotInstancesResponse
    -- ** Response constructor
    , requestSpotInstancesResponse
    -- ** Response lenses
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
-- * 'rsirqInstanceCount'
--
-- * 'rsirqClientToken'
--
-- * 'rsirqAvailabilityZoneGroup'
--
-- * 'rsirqLaunchSpecification'
--
-- * 'rsirqValidUntil'
--
-- * 'rsirqLaunchGroup'
--
-- * 'rsirqType'
--
-- * 'rsirqValidFrom'
--
-- * 'rsirqDryRun'
--
-- * 'rsirqSpotPrice'
data RequestSpotInstances = RequestSpotInstances'
    { _rsirqInstanceCount         :: !(Maybe Int)
    , _rsirqClientToken           :: !(Maybe Text)
    , _rsirqAvailabilityZoneGroup :: !(Maybe Text)
    , _rsirqLaunchSpecification   :: !(Maybe RequestSpotLaunchSpecification)
    , _rsirqValidUntil            :: !(Maybe ISO8601)
    , _rsirqLaunchGroup           :: !(Maybe Text)
    , _rsirqType                  :: !(Maybe SpotInstanceType)
    , _rsirqValidFrom             :: !(Maybe ISO8601)
    , _rsirqDryRun                :: !(Maybe Bool)
    , _rsirqSpotPrice             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestSpotInstances' smart constructor.
requestSpotInstances :: Text -> RequestSpotInstances
requestSpotInstances pSpotPrice_ =
    RequestSpotInstances'
    { _rsirqInstanceCount = Nothing
    , _rsirqClientToken = Nothing
    , _rsirqAvailabilityZoneGroup = Nothing
    , _rsirqLaunchSpecification = Nothing
    , _rsirqValidUntil = Nothing
    , _rsirqLaunchGroup = Nothing
    , _rsirqType = Nothing
    , _rsirqValidFrom = Nothing
    , _rsirqDryRun = Nothing
    , _rsirqSpotPrice = pSpotPrice_
    }

-- | The maximum number of Spot Instances to launch.
--
-- Default: 1
rsirqInstanceCount :: Lens' RequestSpotInstances (Maybe Int)
rsirqInstanceCount = lens _rsirqInstanceCount (\ s a -> s{_rsirqInstanceCount = a});

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon Elastic Compute Cloud User Guide/.
rsirqClientToken :: Lens' RequestSpotInstances (Maybe Text)
rsirqClientToken = lens _rsirqClientToken (\ s a -> s{_rsirqClientToken = a});

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
rsirqAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rsirqAvailabilityZoneGroup = lens _rsirqAvailabilityZoneGroup (\ s a -> s{_rsirqAvailabilityZoneGroup = a});

-- | FIXME: Undocumented member.
rsirqLaunchSpecification :: Lens' RequestSpotInstances (Maybe RequestSpotLaunchSpecification)
rsirqLaunchSpecification = lens _rsirqLaunchSpecification (\ s a -> s{_rsirqLaunchSpecification = a});

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or
-- this date is reached. If the request is persistent, it remains active
-- until it is canceled or this date and time is reached.
--
-- Default: The request is effective indefinitely.
rsirqValidUntil :: Lens' RequestSpotInstances (Maybe UTCTime)
rsirqValidUntil = lens _rsirqValidUntil (\ s a -> s{_rsirqValidUntil = a}) . mapping _Time;

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
rsirqLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rsirqLaunchGroup = lens _rsirqLaunchGroup (\ s a -> s{_rsirqLaunchGroup = a});

-- | The Spot Instance request type.
--
-- Default: @one-time@
rsirqType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rsirqType = lens _rsirqType (\ s a -> s{_rsirqType = a});

-- | The start date of the request. If this is a one-time request, the
-- request becomes active at this date and time and remains active until
-- all instances launch, the request expires, or the request is canceled.
-- If the request is persistent, the request becomes active at this date
-- and time and remains active until it expires or is canceled.
--
-- Default: The request is effective indefinitely.
rsirqValidFrom :: Lens' RequestSpotInstances (Maybe UTCTime)
rsirqValidFrom = lens _rsirqValidFrom (\ s a -> s{_rsirqValidFrom = a}) . mapping _Time;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsirqDryRun :: Lens' RequestSpotInstances (Maybe Bool)
rsirqDryRun = lens _rsirqDryRun (\ s a -> s{_rsirqDryRun = a});

-- | The maximum hourly price (bid) for any Spot Instance launched to fulfill
-- the request.
rsirqSpotPrice :: Lens' RequestSpotInstances Text
rsirqSpotPrice = lens _rsirqSpotPrice (\ s a -> s{_rsirqSpotPrice = a});

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
               "InstanceCount" =: _rsirqInstanceCount,
               "ClientToken" =: _rsirqClientToken,
               "AvailabilityZoneGroup" =:
                 _rsirqAvailabilityZoneGroup,
               "LaunchSpecification" =: _rsirqLaunchSpecification,
               "ValidUntil" =: _rsirqValidUntil,
               "LaunchGroup" =: _rsirqLaunchGroup,
               "Type" =: _rsirqType, "ValidFrom" =: _rsirqValidFrom,
               "DryRun" =: _rsirqDryRun,
               "SpotPrice" =: _rsirqSpotPrice]

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
rsirsSpotInstanceRequests = lens _rsirsSpotInstanceRequests (\ s a -> s{_rsirsSpotInstanceRequests = a}) . _Default;

-- | FIXME: Undocumented member.
rsirsStatus :: Lens' RequestSpotInstancesResponse Int
rsirsStatus = lens _rsirsStatus (\ s a -> s{_rsirsStatus = a});
