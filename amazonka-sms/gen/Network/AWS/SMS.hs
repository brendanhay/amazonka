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
-- Amazon Server Migration Service automates the process of migrating servers to EC2.
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

    -- ** MissingRequiredParameterException
    , _MissingRequiredParameterException

    -- ** UnauthorizedOperationException
    , _UnauthorizedOperationException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteServerCatalog
    , module Network.AWS.SMS.DeleteServerCatalog

    -- ** GetReplicationRuns (Paginated)
    , module Network.AWS.SMS.GetReplicationRuns

    -- ** GetServers (Paginated)
    , module Network.AWS.SMS.GetServers

    -- ** ImportServerCatalog
    , module Network.AWS.SMS.ImportServerCatalog

    -- ** GetConnectors (Paginated)
    , module Network.AWS.SMS.GetConnectors

    -- ** GetReplicationJobs (Paginated)
    , module Network.AWS.SMS.GetReplicationJobs

    -- ** DisassociateConnector
    , module Network.AWS.SMS.DisassociateConnector

    -- ** CreateReplicationJob
    , module Network.AWS.SMS.CreateReplicationJob

    -- ** UpdateReplicationJob
    , module Network.AWS.SMS.UpdateReplicationJob

    -- ** DeleteReplicationJob
    , module Network.AWS.SMS.DeleteReplicationJob

    -- ** StartOnDemandReplicationRun
    , module Network.AWS.SMS.StartOnDemandReplicationRun

    -- * Types

    -- ** ConnectorCapability
    , ConnectorCapability (..)

    -- ** ConnectorStatus
    , ConnectorStatus (..)

    -- ** LicenseType
    , LicenseType (..)

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

    -- ** ReplicationJob
    , ReplicationJob
    , replicationJob
    , rjFrequency
    , rjState
    , rjServerType
    , rjServerId
    , rjLicenseType
    , rjRoleName
    , rjVmServer
    , rjReplicationJobId
    , rjReplicationRunList
    , rjNextReplicationRunStartTime
    , rjStatusMessage
    , rjLatestAMIId
    , rjSeedReplicationTime
    , rjDescription

    -- ** ReplicationRun
    , ReplicationRun
    , replicationRun
    , rrState
    , rrReplicationRunId
    , rrScheduledStartTime
    , rrStatusMessage
    , rrCompletedTime
    , rrAmiId
    , rrType
    , rrDescription

    -- ** Server
    , Server
    , server
    , sServerType
    , sServerId
    , sReplicationJobTerminated
    , sVmServer
    , sReplicationJobId

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

import Network.AWS.SMS.CreateReplicationJob
import Network.AWS.SMS.DeleteReplicationJob
import Network.AWS.SMS.DeleteServerCatalog
import Network.AWS.SMS.DisassociateConnector
import Network.AWS.SMS.GetConnectors
import Network.AWS.SMS.GetReplicationJobs
import Network.AWS.SMS.GetReplicationRuns
import Network.AWS.SMS.GetServers
import Network.AWS.SMS.ImportServerCatalog
import Network.AWS.SMS.StartOnDemandReplicationRun
import Network.AWS.SMS.Types
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
