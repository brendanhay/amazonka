{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a Spot Instance request. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current Spot Instance requests. For
-- more information about Spot Instances, see Spot Instances in the Amazon
-- Elastic Compute Cloud User Guide. Example This example creates a Spot
-- Instance request for two m1.small instances and associates an IAM instance
-- profile called s3access with them.
-- https://ec2.amazonaws.com/?Action=RequestSpotInstances &amp;SpotPrice=0.50
-- &amp;InstanceCount=2 &amp;Type=one-time
-- &amp;AvailabilityZoneGroup=MyAzGroup
-- &amp;LaunchSpecification.ImageId=ami-1a2b3c4d
-- &amp;LaunchSpecification.KeyName=my-key-pair
-- &amp;LaunchSpecification.SecurityGroup.1=websrv
-- &amp;LaunchSpecification.InstanceType=m1.small
-- &amp;LaunchSpecification.IamInstanceProfile.Name=s3access &amp;AUTHPARAMS
-- &lt;RequestSpotInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- sir-1a2b3c4d 0.5 one-time open pending-evaluation YYYY-MM-DDTHH:MM:SS.000Z
-- Your Spot request has been submitted for review, and is pending evaluation.
-- MyAzGroup ami-1a2b3c4d my-key-pair sg-1a2b3c4d websrv m1.small false false
-- YYYY-MM-DDTHH:MM:SS.000Z Linux/UNIX.
module Network.AWS.EC2
    (
    -- * Request
      RequestSpotInstances
    -- ** Request constructor
    , mkRequestSpotInstances
    -- ** Request lenses
    , rsiSpotPrice
    , rsiInstanceCount
    , rsiType
    , rsiValidFrom
    , rsiValidUntil
    , rsiLaunchGroup
    , rsiAvailabilityZoneGroup
    , rsiLaunchSpecification

    -- * Response
    , RequestSpotInstancesResponse
    -- ** Response constructor
    , mkRequestSpotInstancesResponse
    -- ** Response lenses
    , rsirSpotInstanceRequests
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data RequestSpotInstances = RequestSpotInstances
    { _rsiSpotPrice :: !Text
    , _rsiInstanceCount :: !(Maybe Integer)
    , _rsiType :: Maybe SpotInstanceType
    , _rsiValidFrom :: !(Maybe ISO8601)
    , _rsiValidUntil :: !(Maybe ISO8601)
    , _rsiLaunchGroup :: !(Maybe Text)
    , _rsiAvailabilityZoneGroup :: !(Maybe Text)
    , _rsiLaunchSpecification :: Maybe LaunchSpecification
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RequestSpotInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotPrice ::@ @Text@
--
-- * @InstanceCount ::@ @Maybe Integer@
--
-- * @Type ::@ @Maybe SpotInstanceType@
--
-- * @ValidFrom ::@ @Maybe ISO8601@
--
-- * @ValidUntil ::@ @Maybe ISO8601@
--
-- * @LaunchGroup ::@ @Maybe Text@
--
-- * @AvailabilityZoneGroup ::@ @Maybe Text@
--
-- * @LaunchSpecification ::@ @Maybe LaunchSpecification@
--
mkRequestSpotInstances :: Text -- ^ 'rsiSpotPrice'
                       -> RequestSpotInstances
mkRequestSpotInstances p1 = RequestSpotInstances
    { _rsiSpotPrice = p1
    , _rsiInstanceCount = Nothing
    , _rsiType = Nothing
    , _rsiValidFrom = Nothing
    , _rsiValidUntil = Nothing
    , _rsiLaunchGroup = Nothing
    , _rsiAvailabilityZoneGroup = Nothing
    , _rsiLaunchSpecification = Nothing
    }

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
rsiSpotPrice :: Lens' RequestSpotInstances Text
rsiSpotPrice = lens _rsiSpotPrice (\s a -> s { _rsiSpotPrice = a })

-- | The maximum number of Spot Instances to launch. Default: 1.
rsiInstanceCount :: Lens' RequestSpotInstances (Maybe Integer)
rsiInstanceCount =
    lens _rsiInstanceCount (\s a -> s { _rsiInstanceCount = a })

-- | The Spot Instance request type. Default: one-time.
rsiType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)
rsiType = lens _rsiType (\s a -> s { _rsiType = a })

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all instances
-- launch, the request expires, or the request is canceled. If the request is
-- persistent, the request becomes active at this date and time and remains
-- active until it expires or is canceled. Default: The request is effective
-- indefinitely.
rsiValidFrom :: Lens' RequestSpotInstances (Maybe ISO8601)
rsiValidFrom = lens _rsiValidFrom (\s a -> s { _rsiValidFrom = a })

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or this
-- date is reached. If the request is persistent, it remains active until it
-- is canceled or this date and time is reached. Default: The request is
-- effective indefinitely.
rsiValidUntil :: Lens' RequestSpotInstances (Maybe ISO8601)
rsiValidUntil = lens _rsiValidUntil (\s a -> s { _rsiValidUntil = a })

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together. Default: Instances are launched and
-- terminated individually.
rsiLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiLaunchGroup = lens _rsiLaunchGroup (\s a -> s { _rsiLaunchGroup = a })

-- | The user-specified name for a logical grouping of bids. When you specify an
-- Availability Zone group in a Spot Instance request, all Spot Instances in
-- the request are launched in the same Availability Zone. Instance proximity
-- is maintained with this parameter, but the choice of Availability Zone is
-- not. The group applies only to bids for Spot Instances of the same instance
-- type. Any additional Spot Instance requests that are specified with the
-- same Availability Zone group name are launched in that same Availability
-- Zone, as long as at least one instance from the group is still active. If
-- there is no active instance running in the Availability Zone group that you
-- specify for a new Spot Instance request (all instances are terminated, the
-- bid is expired, or the bid falls below current market), then Amazon EC2
-- launches the instance in any Availability Zone where the constraint can be
-- met. Consequently, the subsequent set of Spot Instances could be placed in
-- a different zone from the original request, even if you specified the same
-- Availability Zone group. Default: Instances are launched in any available
-- Availability Zone.
rsiAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)
rsiAvailabilityZoneGroup =
    lens _rsiAvailabilityZoneGroup
         (\s a -> s { _rsiAvailabilityZoneGroup = a })

-- | The launch specification.
rsiLaunchSpecification :: Lens' RequestSpotInstances (Maybe LaunchSpecification)
rsiLaunchSpecification =
    lens _rsiLaunchSpecification (\s a -> s { _rsiLaunchSpecification = a })

instance ToQuery RequestSpotInstances where
    toQuery = genericQuery def

newtype RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { _rsirSpotInstanceRequests :: [SpotInstanceRequest]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RequestSpotInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotInstanceRequests ::@ @[SpotInstanceRequest]@
--
mkRequestSpotInstancesResponse :: RequestSpotInstancesResponse
mkRequestSpotInstancesResponse = RequestSpotInstancesResponse
    { _rsirSpotInstanceRequests = mempty
    }

-- | Information about the Spot Instance request.
rsirSpotInstanceRequests :: Lens' RequestSpotInstancesResponse [SpotInstanceRequest]
rsirSpotInstanceRequests =
    lens _rsirSpotInstanceRequests
         (\s a -> s { _rsirSpotInstanceRequests = a })

instance FromXML RequestSpotInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RequestSpotInstances where
    type Sv RequestSpotInstances = EC2
    type Rs RequestSpotInstances = RequestSpotInstancesResponse

    request = post "RequestSpotInstances"
    response _ = xmlResponse
