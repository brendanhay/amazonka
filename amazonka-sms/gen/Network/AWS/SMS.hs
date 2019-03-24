{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AAWS Sever Migration Service__
--
-- This is the /AWS Sever Migration Service API Reference/ . It provides descriptions, syntax, and usage examples for each of the actions and data types for the AWS Sever Migration Service (AWS SMS). The topic for each action shows the Query API request parameters and the XML response. You can also view the XML request elements in the WSDL.
--
-- Alternatively, you can use one of the AWS SDKs to access an API that's tailored to the programming language or platform that you're using. For more information, see <http://aws.amazon.com/tools/#SDKs AWS SDKs> .
--
-- To learn more about the Server Migration Service, see the following resources:
--
--     * <https://aws.amazon.com/server-migration-service/ AWS Sever Migration Service product page>
--
--     * <https://docs.aws.amazon.com/server-migration-service/latest/userguide/server-migration.html AWS Sever Migration Service User Guide>
--
--
--
module Network.AWS.SMS
    (
    -- * Service Configuration
      sms

    -- * Errors
    -- $errors

    -- ** ReplicationRunLimitExceededException
    , _ReplicationRunLimitExceededException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** NoConnectorsAvailableException
    , _NoConnectorsAvailableException

    -- ** ReplicationJobNotFoundException
    , _ReplicationJobNotFoundException

    -- ** ServerCannotBeReplicatedException
    , _ServerCannotBeReplicatedException

    -- ** InternalError
    , _InternalError

    -- ** ReplicationJobAlreadyExistsException
    , _ReplicationJobAlreadyExistsException

    -- ** OperationNotPermittedException
    , _OperationNotPermittedException

    -- ** TemporarilyUnavailableException
    , _TemporarilyUnavailableException

    -- ** MissingRequiredParameterException
    , _MissingRequiredParameterException

    -- ** UnauthorizedOperationException
    , _UnauthorizedOperationException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteAppReplicationConfiguration
    , module Network.AWS.SMS.DeleteAppReplicationConfiguration

    -- ** PutAppReplicationConfiguration
    , module Network.AWS.SMS.PutAppReplicationConfiguration

    -- ** DeleteServerCatalog
    , module Network.AWS.SMS.DeleteServerCatalog

    -- ** GetAppLaunchConfiguration
    , module Network.AWS.SMS.GetAppLaunchConfiguration

    -- ** DeleteAppLaunchConfiguration
    , module Network.AWS.SMS.DeleteAppLaunchConfiguration

    -- ** StartAppReplication
    , module Network.AWS.SMS.StartAppReplication

    -- ** PutAppLaunchConfiguration
    , module Network.AWS.SMS.PutAppLaunchConfiguration

    -- ** GetReplicationRuns (Paginated)
    , module Network.AWS.SMS.GetReplicationRuns

    -- ** TerminateApp
    , module Network.AWS.SMS.TerminateApp

    -- ** ListApps (Paginated)
    , module Network.AWS.SMS.ListApps

    -- ** GetServers (Paginated)
    , module Network.AWS.SMS.GetServers

    -- ** DeleteApp
    , module Network.AWS.SMS.DeleteApp

    -- ** UpdateApp
    , module Network.AWS.SMS.UpdateApp

    -- ** ImportServerCatalog
    , module Network.AWS.SMS.ImportServerCatalog

    -- ** GenerateTemplate
    , module Network.AWS.SMS.GenerateTemplate

    -- ** GetConnectors (Paginated)
    , module Network.AWS.SMS.GetConnectors

    -- ** GetReplicationJobs (Paginated)
    , module Network.AWS.SMS.GetReplicationJobs

    -- ** DisassociateConnector
    , module Network.AWS.SMS.DisassociateConnector

    -- ** LaunchApp
    , module Network.AWS.SMS.LaunchApp

    -- ** CreateReplicationJob
    , module Network.AWS.SMS.CreateReplicationJob

    -- ** GenerateChangeSet
    , module Network.AWS.SMS.GenerateChangeSet

    -- ** GetApp
    , module Network.AWS.SMS.GetApp

    -- ** UpdateReplicationJob
    , module Network.AWS.SMS.UpdateReplicationJob

    -- ** DeleteReplicationJob
    , module Network.AWS.SMS.DeleteReplicationJob

    -- ** CreateApp
    , module Network.AWS.SMS.CreateApp

    -- ** StopAppReplication
    , module Network.AWS.SMS.StopAppReplication

    -- ** GetAppReplicationConfiguration
    , module Network.AWS.SMS.GetAppReplicationConfiguration

    -- ** StartOnDemandReplicationRun
    , module Network.AWS.SMS.StartOnDemandReplicationRun

    -- * Types

    -- ** AppLaunchStatus
    , AppLaunchStatus (..)

    -- ** AppReplicationStatus
    , AppReplicationStatus (..)

    -- ** AppStatus
    , AppStatus (..)

    -- ** ConnectorCapability
    , ConnectorCapability (..)

    -- ** ConnectorStatus
    , ConnectorStatus (..)

    -- ** LicenseType
    , LicenseType (..)

    -- ** OutputFormat
    , OutputFormat (..)

    -- ** ReplicationJobState
    , ReplicationJobState (..)

    -- ** ReplicationRunState
    , ReplicationRunState (..)

    -- ** ReplicationRunType
    , ReplicationRunType (..)

    -- ** ServerCatalogStatus
    , ServerCatalogStatus (..)

    -- ** ServerType
    , ServerType (..)

    -- ** VMManagerType
    , VMManagerType (..)

    -- ** AppSummary
    , AppSummary
    , appSummary
    , asCreationTime
    , asTotalServers
    , asStatus
    , asLaunchDetails
    , asLaunchStatusMessage
    , asReplicationStatusMessage
    , asTotalServerGroups
    , asRoleName
    , asLaunchStatus
    , asAppId
    , asName
    , asStatusMessage
    , asLatestReplicationTime
    , asReplicationStatus
    , asLastModified
    , asDescription

    -- ** Connector
    , Connector
    , connector
    , cStatus
    , cVmManagerName
    , cIpAddress
    , cVmManagerId
    , cVmManagerType
    , cConnectorId
    , cAssociatedOn
    , cMacAddress
    , cVersion
    , cCapabilityList

    -- ** LaunchDetails
    , LaunchDetails
    , launchDetails
    , ldStackId
    , ldLatestLaunchTime
    , ldStackName

    -- ** ReplicationJob
    , ReplicationJob
    , replicationJob
    , rjFrequency
    , rjNumberOfRecentAMIsToKeep
    , rjState
    , rjServerType
    , rjServerId
    , rjLicenseType
    , rjRoleName
    , rjVmServer
    , rjEncrypted
    , rjReplicationJobId
    , rjReplicationRunList
    , rjNextReplicationRunStartTime
    , rjStatusMessage
    , rjKmsKeyId
    , rjLatestAMIId
    , rjSeedReplicationTime
    , rjRunOnce
    , rjDescription

    -- ** ReplicationRun
    , ReplicationRun
    , replicationRun
    , rrState
    , rrReplicationRunId
    , rrEncrypted
    , rrStageDetails
    , rrScheduledStartTime
    , rrStatusMessage
    , rrKmsKeyId
    , rrCompletedTime
    , rrAmiId
    , rrType
    , rrDescription

    -- ** ReplicationRunStageDetails
    , ReplicationRunStageDetails
    , replicationRunStageDetails
    , rrsdStage
    , rrsdStageProgress

    -- ** S3Location
    , S3Location
    , s3Location
    , slBucket
    , slKey

    -- ** Server
    , Server
    , server
    , sServerType
    , sServerId
    , sReplicationJobTerminated
    , sVmServer
    , sReplicationJobId

    -- ** ServerGroup
    , ServerGroup
    , serverGroup
    , sgServerList
    , sgName
    , sgServerGroupId

    -- ** ServerGroupLaunchConfiguration
    , ServerGroupLaunchConfiguration
    , serverGroupLaunchConfiguration
    , sglcServerGroupId
    , sglcLaunchOrder
    , sglcServerLaunchConfigurations

    -- ** ServerGroupReplicationConfiguration
    , ServerGroupReplicationConfiguration
    , serverGroupReplicationConfiguration
    , sgrcServerGroupId
    , sgrcServerReplicationConfigurations

    -- ** ServerLaunchConfiguration
    , ServerLaunchConfiguration
    , serverLaunchConfiguration
    , slcEc2KeyName
    , slcAssociatePublicIPAddress
    , slcSubnet
    , slcLogicalId
    , slcSecurityGroup
    , slcUserData
    , slcInstanceType
    , slcServer
    , slcVpc

    -- ** ServerReplicationConfiguration
    , ServerReplicationConfiguration
    , serverReplicationConfiguration
    , srcServerReplicationParameters
    , srcServer

    -- ** ServerReplicationParameters
    , ServerReplicationParameters
    , serverReplicationParameters
    , srpFrequency
    , srpNumberOfRecentAMIsToKeep
    , srpSeedTime
    , srpLicenseType
    , srpEncrypted
    , srpKmsKeyId
    , srpRunOnce

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** UserData
    , UserData
    , userData
    , udS3Location

    -- ** VMServer
    , VMServer
    , vMServer
    , vmsVmManagerName
    , vmsVmManagerType
    , vmsVmServerAddress
    , vmsVmName
    , vmsVmPath

    -- ** VMServerAddress
    , VMServerAddress
    , vMServerAddress
    , vmsaVmManagerId
    , vmsaVmId
    ) where

import Network.AWS.SMS.CreateApp
import Network.AWS.SMS.CreateReplicationJob
import Network.AWS.SMS.DeleteApp
import Network.AWS.SMS.DeleteAppLaunchConfiguration
import Network.AWS.SMS.DeleteAppReplicationConfiguration
import Network.AWS.SMS.DeleteReplicationJob
import Network.AWS.SMS.DeleteServerCatalog
import Network.AWS.SMS.DisassociateConnector
import Network.AWS.SMS.GenerateChangeSet
import Network.AWS.SMS.GenerateTemplate
import Network.AWS.SMS.GetApp
import Network.AWS.SMS.GetAppLaunchConfiguration
import Network.AWS.SMS.GetAppReplicationConfiguration
import Network.AWS.SMS.GetConnectors
import Network.AWS.SMS.GetReplicationJobs
import Network.AWS.SMS.GetReplicationRuns
import Network.AWS.SMS.GetServers
import Network.AWS.SMS.ImportServerCatalog
import Network.AWS.SMS.LaunchApp
import Network.AWS.SMS.ListApps
import Network.AWS.SMS.PutAppLaunchConfiguration
import Network.AWS.SMS.PutAppReplicationConfiguration
import Network.AWS.SMS.StartAppReplication
import Network.AWS.SMS.StartOnDemandReplicationRun
import Network.AWS.SMS.StopAppReplication
import Network.AWS.SMS.TerminateApp
import Network.AWS.SMS.Types
import Network.AWS.SMS.UpdateApp
import Network.AWS.SMS.UpdateReplicationJob
import Network.AWS.SMS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SMS'.
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
