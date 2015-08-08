{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Config
--
-- AWS Config provides a way to keep track of the configurations of all the
-- AWS resources associated with your AWS account. You can use AWS Config
-- to get the current and historical configurations of each AWS resource
-- and also to get information about the relationship between the
-- resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2)
-- instance, an Elastic Block Store (EBS) volume, an Elastic network
-- Interface (ENI), or a security group. For a complete list of resources
-- currently supported by AWS Config, see
-- <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resources>.
--
-- You can access and manage AWS Config through the AWS Management Console,
-- the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS
-- SDKs for AWS Config
--
-- This reference guide contains documentation for the AWS Config API and
-- the AWS CLI commands that you can use to manage AWS Config.
--
-- The AWS Config API uses the Signature Version 4 protocol for signing
-- requests. For more information about how to sign a request with this
-- protocol, see
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- For detailed information about AWS Config features and their associated
-- actions or commands, as well as how to work with AWS Management Console,
-- see
-- <http://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html What Is AWS Config?>
-- in the /AWS Config Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/config/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.Config
    (
    -- * Service Description
      Config

    -- * Error Matchers
    -- $errors
    , _ValidationException
    , _InvalidTimeRangeException
    , _InvalidRecordingGroupException
    , _InvalidSNSTopicARNException
    , _InvalidRoleException
    , _LastDeliveryChannelDeleteFailedException
    , _InvalidLimitException
    , _InvalidDeliveryChannelNameException
    , _NoSuchDeliveryChannelException
    , _ResourceNotDiscoveredException
    , _InvalidNextTokenException
    , _NoSuchBucketException
    , _NoAvailableConfigurationRecorderException
    , _NoAvailableDeliveryChannelException
    , _NoRunningConfigurationRecorderException
    , _MaxNumberOfConfigurationRecordersExceededException
    , _InvalidConfigurationRecorderNameException
    , _InsufficientDeliveryPolicyException
    , _MaxNumberOfDeliveryChannelsExceededException
    , _NoSuchConfigurationRecorderException
    , _InvalidS3KeyPrefixException

    -- * Operations
    -- $operations

    -- ** GetResourceConfigHistory
    , module Network.AWS.Config.GetResourceConfigHistory

    -- ** StopConfigurationRecorder
    , module Network.AWS.Config.StopConfigurationRecorder

    -- ** DeliverConfigSnapshot
    , module Network.AWS.Config.DeliverConfigSnapshot

    -- ** DescribeConfigurationRecorders
    , module Network.AWS.Config.DescribeConfigurationRecorders

    -- ** StartConfigurationRecorder
    , module Network.AWS.Config.StartConfigurationRecorder

    -- ** DescribeConfigurationRecorderStatus
    , module Network.AWS.Config.DescribeConfigurationRecorderStatus

    -- ** PutConfigurationRecorder
    , module Network.AWS.Config.PutConfigurationRecorder

    -- ** DeleteDeliveryChannel
    , module Network.AWS.Config.DeleteDeliveryChannel

    -- ** PutDeliveryChannel
    , module Network.AWS.Config.PutDeliveryChannel

    -- ** DescribeDeliveryChannelStatus
    , module Network.AWS.Config.DescribeDeliveryChannelStatus

    -- ** DescribeDeliveryChannels
    , module Network.AWS.Config.DescribeDeliveryChannels

    -- * Types

    -- ** ChronologicalOrder
    , ChronologicalOrder (..)

    -- ** ConfigurationItemStatus
    , ConfigurationItemStatus (..)

    -- ** DeliveryStatus
    , DeliveryStatus (..)

    -- ** RecorderStatus
    , RecorderStatus (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** ConfigExportDeliveryInfo
    , ConfigExportDeliveryInfo
    , configExportDeliveryInfo
    , cediLastErrorCode
    , cediLastAttemptTime
    , cediLastSuccessfulTime
    , cediLastStatus
    , cediLastErrorMessage

    -- ** ConfigStreamDeliveryInfo
    , ConfigStreamDeliveryInfo
    , configStreamDeliveryInfo
    , csdiLastErrorCode
    , csdiLastStatusChangeTime
    , csdiLastStatus
    , csdiLastErrorMessage

    -- ** ConfigurationItem
    , ConfigurationItem
    , configurationItem
    , ciResourceId
    , ciConfigurationStateId
    , ciResourceType
    , ciArn
    , ciResourceCreationTime
    , ciConfigurationItemStatus
    , ciAccountId
    , ciConfigurationItemCaptureTime
    , ciAvailabilityZone
    , ciRelationships
    , ciVersion
    , ciRelatedEvents
    , ciConfiguration
    , ciConfigurationItemMD5Hash
    , ciTags

    -- ** ConfigurationRecorder
    , ConfigurationRecorder
    , configurationRecorder
    , crName
    , crRecordingGroup
    , crRoleARN

    -- ** ConfigurationRecorderStatus
    , ConfigurationRecorderStatus
    , configurationRecorderStatus
    , crsLastErrorCode
    , crsLastStopTime
    , crsLastStatusChangeTime
    , crsRecording
    , crsLastStatus
    , crsLastErrorMessage
    , crsName
    , crsLastStartTime

    -- ** DeliveryChannel
    , DeliveryChannel
    , deliveryChannel
    , dcS3KeyPrefix
    , dcSnsTopicARN
    , dcName
    , dcS3BucketName

    -- ** DeliveryChannelStatus
    , DeliveryChannelStatus
    , deliveryChannelStatus
    , dcsConfigStreamDeliveryInfo
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigHistoryDeliveryInfo
    , dcsName

    -- ** RecordingGroup
    , RecordingGroup
    , recordingGroup
    , rgAllSupported
    , rgResourceTypes

    -- ** Relationship
    , Relationship
    , relationship
    , rResourceId
    , rResourceType
    , rRelationshipName
    ) where

import           Network.AWS.Config.DeleteDeliveryChannel
import           Network.AWS.Config.DeliverConfigSnapshot
import           Network.AWS.Config.DescribeConfigurationRecorders
import           Network.AWS.Config.DescribeConfigurationRecorderStatus
import           Network.AWS.Config.DescribeDeliveryChannels
import           Network.AWS.Config.DescribeDeliveryChannelStatus
import           Network.AWS.Config.GetResourceConfigHistory
import           Network.AWS.Config.PutConfigurationRecorder
import           Network.AWS.Config.PutDeliveryChannel
import           Network.AWS.Config.StartConfigurationRecorder
import           Network.AWS.Config.StopConfigurationRecorder
import           Network.AWS.Config.Types
import           Network.AWS.Config.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Config'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly send a request until some remote success condition
specified by the 'Wait' configuration is fulfilled. The 'Wait' configuration
specifies how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
