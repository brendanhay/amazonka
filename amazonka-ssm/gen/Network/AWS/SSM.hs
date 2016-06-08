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
-- Simple Systems Manager (SSM) enables you to remotely manage the configuration of your Amazon EC2 instance. Using SSM, you can run scripts or commands using either EC2 Run Command or SSM Config. (SSM Config is currently available only for Windows instances.)
--
-- __Run Command__
--
-- Run Command provides an on-demand experience for executing commands. You can use pre-defined Amazon SSM documents to perform the actions listed later in this section, or you can create your own documents. With these documents, you can remotely configure your instances by sending commands using the __Commands__ page in the <http://console.aws.amazon.com/ec2/ Amazon EC2 console>, <http://docs.aws.amazon.com/powershell/latest/reference/items/Amazon_Simple_Systems_Management_cmdlets.html AWS Tools for Windows PowerShell>, or the <http://docs.aws.amazon.com/cli/latest/reference/ssm/index.html AWS CLI>.
--
-- Run Command reports the status of the command execution for each instance targeted by a command. You can also audit the command execution to understand who executed commands, when, and what changes were made. By switching between different SSM documents, you can quickly configure your instances with different types of commands. To get started with Run Command, verify that your environment meets the prerequisites for remotely running commands on EC2 instances (<http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/remote-commands-prereq.html Linux> or <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/remote-commands-prereq.html Windows>).
--
-- __SSM Config__
--
-- SSM Config is a lightweight instance configuration solution. SSM Config is currently only available for Windows instances. With SSM Config, you can specify a setup configuration for your instances. SSM Config is similar to EC2 User Data, which is another way of running one-time scripts or applying settings during instance launch. SSM Config is an extension of this capability. Using SSM documents, you can specify which actions the system should perform on your instances, including which applications to install, which AWS Directory Service directory to join, which Microsoft PowerShell modules to install, etc. If an instance is missing one or more of these configurations, the system makes those changes. By default, the system checks every five minutes to see if there is a new configuration to apply as defined in a new SSM document. If so, the system updates the instances accordingly. In this way, you can remotely maintain a consistent configuration baseline on your instances. SSM Config is available using the AWS CLI or the AWS Tools for Windows PowerShell. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-configuration-manage.html Managing Windows Instance Configuration>.
--
-- SSM Config and Run Command include the following pre-defined documents.
--
-- Amazon Pre-defined SSM Documents
--
-- Name Description Platform
--
-- AWS-RunShellScript
--
-- Run shell scripts
--
-- Linux
--
-- AWS-UpdateSSMAgent
--
-- Update the Amazon SSM agent
--
-- Linux
--
-- AWS-JoinDirectoryServiceDomain
--
-- Join an AWS Directory
--
-- Windows
--
-- AWS-RunPowerShellScript
--
-- Run PowerShell commands or scripts
--
-- Windows
--
-- AWS-UpdateEC2Config
--
-- Update the EC2Config service
--
-- Windows
--
-- AWS-ConfigureWindowsUpdate
--
-- Configure Windows Update settings
--
-- Windows
--
-- AWS-InstallApplication
--
-- Install, repair, or uninstall software using an MSI package
--
-- Windows
--
-- AWS-InstallPowerShellModule
--
-- Install PowerShell modules
--
-- Windows
--
-- AWS-ConfigureCloudWatch
--
-- Configure Amazon CloudWatch Logs to monitor applications and systems
--
-- Windows
--
-- The commands or scripts specified in SSM documents run with administrative privilege on your instances because the Amazon SSM agent runs as root on Linux and the EC2Config service runs in the Local System account on Windows. If a user has permission to execute any of the pre-defined SSM documents (any document that begins with AWS-*) then that user also has administrator access to the instance. Delegate access to SSM and Run Command judiciously. This becomes extremely important if you create your own SSM documents. Amazon Web Services does not provide guidance about how to create secure SSM documents. You create SSM documents and delegate access to Run Command at your own risk. As a security best practice, we recommend that you assign access to \"AWS-*\" documents, especially the AWS-RunShellScript document on Linux and the AWS-RunPowerShellScript document on Windows, to trusted administrators only. You can create SSM documents for specific tasks and delegate access to non-administrators.
module Network.AWS.SSM
    (
    -- * Service Configuration
      ssm

    -- * Errors
    -- $errors

    -- ** UnsupportedPlatformType
    , _UnsupportedPlatformType

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

    -- ** InvalidCommandId
    , _InvalidCommandId

    -- ** DuplicateInstanceId
    , _DuplicateInstanceId

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

    -- ** TooManyUpdates
    , _TooManyUpdates

    -- ** MaxDocumentSizeExceeded
    , _MaxDocumentSizeExceeded

    -- ** InvalidParameters
    , _InvalidParameters

    -- ** DocumentAlreadyExists
    , _DocumentAlreadyExists

    -- ** DocumentLimitExceeded
    , _DocumentLimitExceeded

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeDocument
    , module Network.AWS.SSM.DescribeDocument

    -- ** CreateAssociation
    , module Network.AWS.SSM.CreateAssociation

    -- ** CreateDocument
    , module Network.AWS.SSM.CreateDocument

    -- ** ListCommandInvocations (Paginated)
    , module Network.AWS.SSM.ListCommandInvocations

    -- ** ListDocuments (Paginated)
    , module Network.AWS.SSM.ListDocuments

    -- ** GetDocument
    , module Network.AWS.SSM.GetDocument

    -- ** CancelCommand
    , module Network.AWS.SSM.CancelCommand

    -- ** DescribeAssociation
    , module Network.AWS.SSM.DescribeAssociation

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

    -- ** DocumentFilterKey
    , DocumentFilterKey (..)

    -- ** DocumentParameterType
    , DocumentParameterType (..)

    -- ** DocumentStatus
    , DocumentStatus (..)

    -- ** Fault
    , Fault (..)

    -- ** InstanceInformationFilterKey
    , InstanceInformationFilterKey (..)

    -- ** PingStatus
    , PingStatus (..)

    -- ** PlatformType
    , PlatformType (..)

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
    , cOutputS3KeyPrefix
    , cDocumentName
    , cInstanceIds
    , cCommandId
    , cParameters
    , cComment
    , cOutputS3BucketName
    , cRequestedDateTime

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
    , ciCommandPlugins
    , ciDocumentName
    , ciCommandId
    , ciComment
    , ciTraceOutput
    , ciRequestedDateTime

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

    -- ** DocumentDescription
    , DocumentDescription
    , documentDescription
    , dStatus
    , dSha1
    , dPlatformTypes
    , dCreatedDate
    , dName
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
    , iiPlatformVersion
    , iiIsLatestVersion
    , iiAgentVersion
    , iiLastPingDateTime
    , iiPlatformType
    , iiPlatformName

    -- ** InstanceInformationFilter
    , InstanceInformationFilter
    , instanceInformationFilter
    , iifKey
    , iifValueSet
    ) where

import           Network.AWS.SSM.CancelCommand
import           Network.AWS.SSM.CreateAssociation
import           Network.AWS.SSM.CreateAssociationBatch
import           Network.AWS.SSM.CreateDocument
import           Network.AWS.SSM.DeleteAssociation
import           Network.AWS.SSM.DeleteDocument
import           Network.AWS.SSM.DescribeAssociation
import           Network.AWS.SSM.DescribeDocument
import           Network.AWS.SSM.DescribeInstanceInformation
import           Network.AWS.SSM.GetDocument
import           Network.AWS.SSM.ListAssociations
import           Network.AWS.SSM.ListCommandInvocations
import           Network.AWS.SSM.ListCommands
import           Network.AWS.SSM.ListDocuments
import           Network.AWS.SSM.SendCommand
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.UpdateAssociationStatus
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
