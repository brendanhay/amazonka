{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.RequestSpotInstances
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
module Network.AWS.EC2.V2014_06_15.RequestSpotInstances
    (
    -- * Request
      RequestSpotInstances
    -- ** Default constructor
    , requestSpotInstances
    -- ** Accessors and lenses
    , _rsirSpotPrice
    , rsirSpotPrice
    , _rsirValidFrom
    , rsirValidFrom
    , _rsirValidUntil
    , rsirValidUntil
    , _rsirInstanceCount
    , rsirInstanceCount
    , _rsirLaunchSpecification
    , rsirLaunchSpecification
    , _rsirType
    , rsirType
    , _rsirLaunchGroup
    , rsirLaunchGroup
    , _rsirAvailabilityZoneGroup
    , rsirAvailabilityZoneGroup

    -- * Response
    , RequestSpotInstancesResponse
    -- ** Accessors and lenses
    , _rsisSpotInstanceRequests
    , rsisSpotInstanceRequests
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RequestSpotInstances' request.
requestSpotInstances :: Text -- ^ 'rsirSpotPrice'
                     -> RequestSpotInstances
requestSpotInstances p1 = RequestSpotInstances
    { _rsirSpotPrice = p1
    , _rsirValidFrom = Nothing
    , _rsirValidUntil = Nothing
    , _rsirInstanceCount = Nothing
    , _rsirLaunchSpecification = Nothing
    , _rsirType = Nothing
    , _rsirLaunchGroup = Nothing
    , _rsirAvailabilityZoneGroup = Nothing
    }

data RequestSpotInstances = RequestSpotInstances

makeSiglessLenses ''RequestSpotInstances

instance ToQuery RequestSpotInstances where
    toQuery = genericQuery def

data RequestSpotInstancesResponse = RequestSpotInstancesResponse
    { _rsisSpotInstanceRequests :: [SpotInstanceRequest]
      -- ^ Information about the Spot Instance request.
    } deriving (Show, Generic)

makeSiglessLenses ''RequestSpotInstancesResponse

instance FromXML RequestSpotInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RequestSpotInstances where
    type Sv RequestSpotInstances = EC2
    type Rs RequestSpotInstances = RequestSpotInstancesResponse

    request = post "RequestSpotInstances"
    response _ = xmlResponse

-- | The maximum hourly price for any Spot Instance launched to fulfill the
-- request.
rsirSpotPrice :: Lens' RequestSpotInstances (Text)

-- | The start date of the request. If this is a one-time request, the request
-- becomes active at this date and time and remains active until all instances
-- launch, the request expires, or the request is canceled. If the request is
-- persistent, the request becomes active at this date and time and remains
-- active until it expires or is canceled. Default: The request is effective
-- indefinitely.
rsirValidFrom :: Lens' RequestSpotInstances (Maybe ISO8601)

-- | The end date of the request. If this is a one-time request, the request
-- remains active until all instances launch, the request is canceled, or this
-- date is reached. If the request is persistent, it remains active until it
-- is canceled or this date and time is reached. Default: The request is
-- effective indefinitely.
rsirValidUntil :: Lens' RequestSpotInstances (Maybe ISO8601)

-- | The maximum number of Spot Instances to launch. Default: 1.
rsirInstanceCount :: Lens' RequestSpotInstances (Maybe Integer)

-- | The launch specification.
rsirLaunchSpecification :: Lens' RequestSpotInstances (Maybe LaunchSpecification)

-- | The Spot Instance request type. Default: one-time.
rsirType :: Lens' RequestSpotInstances (Maybe SpotInstanceType)

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together. Default: Instances are launched and
-- terminated individually.
rsirLaunchGroup :: Lens' RequestSpotInstances (Maybe Text)

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
rsirAvailabilityZoneGroup :: Lens' RequestSpotInstances (Maybe Text)

-- | Information about the Spot Instance request.
rsisSpotInstanceRequests :: Lens' RequestSpotInstancesResponse ([SpotInstanceRequest])
