{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the Amazon EC2 Simple Systems Manager (SSM) API Reference. SSM enables you to remotely manage the configuration of your Amazon EC2 instances, virtual machines (VMs), or servers in your on-premises environment or in an environment provided by other cloud providers using scripts, commands, or the Amazon EC2 console. SSM includes an on-demand solution called /Amazon EC2 Run Command/ and a lightweight instance configuration solution called /SSM Config/.
--
-- This references is intended to be used with the EC2 Run Command User Guide for <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/execute-remote-commands.html Linux> or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/execute-remote-commands.html Windows>.
--
-- You must register your on-premises servers and VMs through an activation process before you can configure them using Run Command. Registered servers and VMs are called /managed instances/. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/managed-instances.html Setting Up Run Command On Managed Instances (On-Premises Servers and VMs) on Linux> or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/managed-instances.html Setting Up Run Command On Managed Instances (On-Premises Servers and VMs) on Windows>.
--
-- __Run Command__
--
-- Run Command provides an on-demand experience for executing commands. You can use pre-defined SSM documents to perform the actions listed later in this section, or you can create your own documents. With these documents, you can remotely configure your instances by sending commands using the __Commands__ page in the <http://console.aws.amazon.com/ec2/ Amazon EC2 console>, <http://docs.aws.amazon.com/powershell/latest/reference/items/Amazon_Simple_Systems_Management_cmdlets.html AWS Tools for Windows PowerShell>, the <http://docs.aws.amazon.com/cli/latest/reference/ssm/index.html AWS CLI>, or AWS SDKs.
--
-- Run Command reports the status of the command execution for each instance targeted by a command. You can also audit the command execution to understand who executed commands, when, and what changes were made. By switching between different SSM documents, you can quickly configure your instances with different types of commands. To get started with Run Command, verify that your environment meets the prerequisites for remotely running commands on EC2 instances (<http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/remote-commands-prereq.html Linux> or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/remote-commands-prereq.html Windows>).
--
-- __SSM Config__
--
-- SSM Config is a lightweight instance configuration solution. SSM Config is currently only available for Windows instances. With SSM Config, you can specify a setup configuration for your instances. SSM Config is similar to EC2 User Data, which is another way of running one-time scripts or applying settings during instance launch. SSM Config is an extension of this capability. Using SSM documents, you can specify which actions the system should perform on your instances, including which applications to install, which AWS Directory Service directory to join, which Microsoft PowerShell modules to install, etc. If an instance is missing one or more of these configurations, the system makes those changes. By default, the system checks every five minutes to see if there is a new configuration to apply as defined in a new SSM document. If so, the system updates the instances accordingly. In this way, you can remotely maintain a consistent configuration baseline on your instances. SSM Config is available using the AWS CLI or the AWS Tools for Windows PowerShell. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-configuration-manage.html Managing Windows Instance Configuration>.
--
-- SSM Config and Run Command include the following pre-defined documents.
--
-- __Linux__
--
-- -   __AWS-RunShellScript__ to run shell scripts
--
-- -   __AWS-UpdateSSMAgent__ to update the Amazon SSM agent
--
-- __Windows__
--
-- -   __AWS-JoinDirectoryServiceDomain__ to join an AWS Directory
--
-- -   __AWS-RunPowerShellScript__ to run PowerShell commands or scripts
--
-- -   __AWS-UpdateEC2Config__ to update the EC2Config service
--
-- -   __AWS-ConfigureWindowsUpdate__ to configure Windows Update settings
--
-- -   __AWS-InstallApplication__ to install, repair, or uninstall software using an MSI package
--
-- -   __AWS-InstallPowerShellModule__ to install PowerShell modules
--
-- -   __AWS-ConfigureCloudWatch__ to configure Amazon CloudWatch Logs to monitor applications and systems
--
-- -   __AWS-ListWindowsInventory__ to collect information about an EC2 instance running in Windows.
--
-- -   __AWS-FindWindowsUpdates__ to scan an instance and determines which updates are missing.
--
-- -   __AWS-InstallMissingWindowsUpdates__ to install missing updates on your EC2 instance.
--
-- -   __AWS-InstallSpecificWindowsUpdates__ to install one or more specific updates.
--
-- The commands or scripts specified in SSM documents run with administrative privilege on your instances because the Amazon SSM agent runs as root on Linux and the EC2Config service runs in the Local System account on Windows. If a user has permission to execute any of the pre-defined SSM documents (any document that begins with AWS-*) then that user also has administrator access to the instance. Delegate access to Run Command and SSM Config judiciously. This becomes extremely important if you create your own SSM documents. Amazon Web Services does not provide guidance about how to create secure SSM documents. You create SSM documents and delegate access to Run Command at your own risk. As a security best practice, we recommend that you assign access to \"AWS-*\" documents, especially the AWS-RunShellScript document on Linux and the AWS-RunPowerShellScript document on Windows, to trusted administrators only. You can create SSM documents for specific tasks and delegate access to non-administrators.
--
-- For information about creating and sharing SSM documents, see the following topics in the SSM User Guide:
--
-- -   <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/create-ssm-doc.html Creating SSM Documents> and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssm-sharing.html Sharing SSM Documents> (Linux)
--
-- -   <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/create-ssm-doc.html Creating SSM Documents> and <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ssm-sharing.html Sharing SSM Documents> (Windows)
--
module Network.AWS.SSM
    (
    -- * Service Configuration
      ssm

    -- * Errors
    -- $errors

    -- ** UnsupportedPlatformType
    , _UnsupportedPlatformType

    -- ** InvalidPermissionType
    , _InvalidPermissionType

    -- ** AssociatedInstances
    , _AssociatedInstances

    -- ** InvalidInstanceId
    , _InvalidInstanceId

    -- ** StatusUnchanged
    , _StatusUnchanged

    -- ** InvalidNextToken
    , _InvalidNextToken

    -- ** InvalidOutputFolder
    , _InvalidOutputFolder

    -- ** InvalidActivationId
    , _InvalidActivationId

    -- ** InvalidCommandId
    , _InvalidCommandId

    -- ** DuplicateInstanceId
    , _DuplicateInstanceId

    -- ** InvalidResourceType
    , _InvalidResourceType

    -- ** InvalidDocument
    , _InvalidDocument

    -- ** InvalidFilterKey
    , _InvalidFilterKey

    -- ** InvalidInstanceInformationFilterValue
    , _InvalidInstanceInformationFilterValue

    -- ** AssociationAlreadyExists
    , _AssociationAlreadyExists

    -- ** InvalidDocumentContent
    , _InvalidDocumentContent

    -- ** AssociationLimitExceeded
    , _AssociationLimitExceeded

    -- ** AssociationDoesNotExist
    , _AssociationDoesNotExist

    -- ** InternalServerError
    , _InternalServerError

    -- ** InvalidRole
    , _InvalidRole

    -- ** TooManyUpdates
    , _TooManyUpdates

    -- ** InvalidActivation
    , _InvalidActivation

    -- ** MaxDocumentSizeExceeded
    , _MaxDocumentSizeExceeded

    -- ** InvalidDocumentOperation
    , _InvalidDocumentOperation

    -- ** InvalidParameters
    , _InvalidParameters

    -- ** InvalidResourceId
    , _InvalidResourceId

    -- ** InvalidNotificationConfig
    , _InvalidNotificationConfig

    -- ** DocumentPermissionLimit
    , _DocumentPermissionLimit

    -- ** DocumentAlreadyExists
    , _DocumentAlreadyExists

    -- ** DocumentLimitExceeded
    , _DocumentLimitExceeded

    -- ** InvalidFilter
    , _InvalidFilter

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeActivations
    , module Network.AWS.SSM.DescribeActivations

    -- ** ListTagsForResource
    , module Network.AWS.SSM.ListTagsForResource

    -- ** DescribeDocument
    , module Network.AWS.SSM.DescribeDocument

    -- ** CreateAssociation
    , module Network.AWS.SSM.CreateAssociation

    -- ** DeleteActivation
    , module Network.AWS.SSM.DeleteActivation

    -- ** CreateActivation
    , module Network.AWS.SSM.CreateActivation

    -- ** CreateDocument
    , module Network.AWS.SSM.CreateDocument

    -- ** RemoveTagsFromResource
    , module Network.AWS.SSM.RemoveTagsFromResource

    -- ** ListCommandInvocations (Paginated)
    , module Network.AWS.SSM.ListCommandInvocations

    -- ** ListDocuments (Paginated)
    , module Network.AWS.SSM.ListDocuments

    -- ** UpdateManagedInstanceRole
    , module Network.AWS.SSM.UpdateManagedInstanceRole

    -- ** GetDocument
    , module Network.AWS.SSM.GetDocument

    -- ** AddTagsToResource
    , module Network.AWS.SSM.AddTagsToResource

    -- ** CancelCommand
    , module Network.AWS.SSM.CancelCommand

    -- ** DeregisterManagedInstance
    , module Network.AWS.SSM.DeregisterManagedInstance

    -- ** DescribeAssociation
    , module Network.AWS.SSM.DescribeAssociation

    -- ** ModifyDocumentPermission
    , module Network.AWS.SSM.ModifyDocumentPermission

    -- ** UpdateAssociationStatus
    , module Network.AWS.SSM.UpdateAssociationStatus

    -- ** DescribeInstanceInformation
    , module Network.AWS.SSM.DescribeInstanceInformation

    -- ** ListAssociations (Paginated)
    , module Network.AWS.SSM.ListAssociations

    -- ** DeleteAssociation
    , module Network.AWS.SSM.DeleteAssociation

    -- ** SendCommand
    , module Network.AWS.SSM.SendCommand

    -- ** ListCommands (Paginated)
    , module Network.AWS.SSM.ListCommands

    -- ** DeleteDocument
    , module Network.AWS.SSM.DeleteDocument

    -- ** DescribeDocumentPermission
    , module Network.AWS.SSM.DescribeDocumentPermission

    -- ** CreateAssociationBatch
    , module Network.AWS.SSM.CreateAssociationBatch

    -- * Types

    -- ** AssociationFilterKey
    , AssociationFilterKey (..)

    -- ** AssociationStatusName
    , AssociationStatusName (..)

    -- ** CommandFilterKey
    , CommandFilterKey (..)

    -- ** CommandInvocationStatus
    , CommandInvocationStatus (..)

    -- ** CommandPluginStatus
    , CommandPluginStatus (..)

    -- ** CommandStatus
    , CommandStatus (..)

    -- ** DescribeActivationsFilterKeys
    , DescribeActivationsFilterKeys (..)

    -- ** DocumentFilterKey
    , DocumentFilterKey (..)

    -- ** DocumentHashType
    , DocumentHashType (..)

    -- ** DocumentParameterType
    , DocumentParameterType (..)

    -- ** DocumentPermissionType
    , DocumentPermissionType (..)

    -- ** DocumentStatus
    , DocumentStatus (..)

    -- ** Fault
    , Fault (..)

    -- ** InstanceInformationFilterKey
    , InstanceInformationFilterKey (..)

    -- ** NotificationEvent
    , NotificationEvent (..)

    -- ** NotificationType
    , NotificationType (..)

    -- ** PingStatus
    , PingStatus (..)

    -- ** PlatformType
    , PlatformType (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** ResourceTypeForTagging
    , ResourceTypeForTagging (..)

    -- ** Activation
    , Activation
    , activation
    , aExpired
    , aDefaultInstanceName
    , aActivationId
    , aCreatedDate
    , aRegistrationLimit
    , aExpirationDate
    , aDescription
    , aRegistrationsCount
    , aIAMRole

    -- ** Association
    , Association
    , association
    , aInstanceId
    , aName

    -- ** AssociationDescription
    , AssociationDescription
    , associationDescription
    , adInstanceId
    , adStatus
    , adDate
    , adName
    , adParameters

    -- ** AssociationFilter
    , AssociationFilter
    , associationFilter
    , afKey
    , afValue

    -- ** AssociationStatus
    , AssociationStatus
    , associationStatus
    , asAdditionalInfo
    , asDate
    , asName
    , asMessage

    -- ** Command
    , Command
    , command
    , cStatus
    , cExpiresAfter
    , cNotificationConfig
    , cOutputS3KeyPrefix
    , cDocumentName
    , cInstanceIds
    , cCommandId
    , cParameters
    , cComment
    , cOutputS3BucketName
    , cRequestedDateTime
    , cServiceRole

    -- ** CommandFilter
    , CommandFilter
    , commandFilter
    , cfKey
    , cfValue

    -- ** CommandInvocation
    , CommandInvocation
    , commandInvocation
    , ciInstanceId
    , ciStatus
    , ciNotificationConfig
    , ciCommandPlugins
    , ciDocumentName
    , ciCommandId
    , ciComment
    , ciTraceOutput
    , ciRequestedDateTime
    , ciServiceRole

    -- ** CommandPlugin
    , CommandPlugin
    , commandPlugin
    , cpStatus
    , cpResponseStartDateTime
    , cpOutputS3KeyPrefix
    , cpResponseCode
    , cpOutput
    , cpName
    , cpOutputS3BucketName
    , cpResponseFinishDateTime

    -- ** CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreName
    , cabreParameters

    -- ** DescribeActivationsFilter
    , DescribeActivationsFilter
    , describeActivationsFilter
    , dafFilterKey
    , dafFilterValues

    -- ** DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dHash
    , dSha1
    , dOwner
    , dPlatformTypes
    , dCreatedDate
    , dName
    , dHashType
    , dParameters
    , dDescription

    -- ** DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- ** DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diOwner
    , diPlatformTypes
    , diName

    -- ** DocumentParameter
    , DocumentParameter
    , documentParameter
    , dpName
    , dpDefaultValue
    , dpType
    , dpDescription

    -- ** FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage

    -- ** InstanceInformation
    , InstanceInformation
    , instanceInformation
    , iiInstanceId
    , iiPingStatus
    , iiIPAddress
    , iiResourceType
    , iiRegistrationDate
    , iiPlatformVersion
    , iiIsLatestVersion
    , iiAgentVersion
    , iiLastPingDateTime
    , iiActivationId
    , iiName
    , iiPlatformType
    , iiPlatformName
    , iiComputerName
    , iiIAMRole

    -- ** InstanceInformationFilter
    , InstanceInformationFilter
    , instanceInformationFilter
    , iifKey
    , iifValueSet

    -- ** NotificationConfig
    , NotificationConfig
    , notificationConfig
    , ncNotificationEvents
    , ncNotificationType
    , ncNotificationARN

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import           Network.AWS.SSM.AddTagsToResource
import           Network.AWS.SSM.CancelCommand
import           Network.AWS.SSM.CreateActivation
import           Network.AWS.SSM.CreateAssociation
import           Network.AWS.SSM.CreateAssociationBatch
import           Network.AWS.SSM.CreateDocument
import           Network.AWS.SSM.DeleteActivation
import           Network.AWS.SSM.DeleteAssociation
import           Network.AWS.SSM.DeleteDocument
import           Network.AWS.SSM.DeregisterManagedInstance
import           Network.AWS.SSM.DescribeActivations
import           Network.AWS.SSM.DescribeAssociation
import           Network.AWS.SSM.DescribeDocument
import           Network.AWS.SSM.DescribeDocumentPermission
import           Network.AWS.SSM.DescribeInstanceInformation
import           Network.AWS.SSM.GetDocument
import           Network.AWS.SSM.ListAssociations
import           Network.AWS.SSM.ListCommandInvocations
import           Network.AWS.SSM.ListCommands
import           Network.AWS.SSM.ListDocuments
import           Network.AWS.SSM.ListTagsForResource
import           Network.AWS.SSM.ModifyDocumentPermission
import           Network.AWS.SSM.RemoveTagsFromResource
import           Network.AWS.SSM.SendCommand
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.UpdateAssociationStatus
import           Network.AWS.SSM.UpdateManagedInstanceRole
import           Network.AWS.SSM.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SSM'.
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
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
