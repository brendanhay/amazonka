{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS OpsWorks CM__
--
-- AWS OpsWorks for configuration management (CM) is a service that runs and manages configuration management servers.
--
-- __Glossary of terms__
--
--     * __Server__ : A configuration management server that can be highly-available. The configuration management server runs on an Amazon Elastic Compute Cloud (EC2) instance, and may use various other AWS services, such as Amazon Relational Database Service (RDS) and Elastic Load Balancing. A server is a generic abstraction over the configuration manager that you want to use, much like Amazon RDS. In AWS OpsWorks CM, you do not start or stop servers. After you create servers, they continue to run until they are deleted.
--
--     * __Engine__ : The engine is the specific configuration manager that you want to use. Valid values in this release include @Chef@ and @Puppet@ .
--
--     * __Backup__ : This is an application-level backup of the data that the configuration manager stores. AWS OpsWorks CM creates an S3 bucket for backups when you launch the first server. A backup maintains a snapshot of a server's configuration-related attributes at the time the backup starts.
--
--     * __Events__ : Events are always related to a server. Events are written during server creation, when health checks run, when backups are created, when system maintenance is performed, etc. When you delete a server, the server's events are also deleted.
--
--     * __Account attributes__ : Every account has attributes that are assigned in the AWS OpsWorks CM database. These attributes store information about configuration limits (servers, backups, etc.) and your customer account.
--
--
--
-- __Endpoints__
--
-- AWS OpsWorks CM supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Your servers can only be accessed or managed within the endpoint in which they are created.
--
--     * opsworks-cm.us-east-1.amazonaws.com
--
--     * opsworks-cm.us-west-2.amazonaws.com
--
--     * opsworks-cm.eu-west-1.amazonaws.com
--
--
--
-- __Throttling limits__
--
-- All API operations allow for five requests per second with a burst of 10 requests per second.
--
module Network.AWS.OpsWorksCM
    (
    -- * Service Configuration
      opsWorksCM

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidStateException
    , _InvalidStateException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- ** NodeAssociated
    , nodeAssociated

    -- * Operations
    -- $operations

    -- ** AssociateNode
    , module Network.AWS.OpsWorksCM.AssociateNode

    -- ** UpdateServer
    , module Network.AWS.OpsWorksCM.UpdateServer

    -- ** DeleteServer
    , module Network.AWS.OpsWorksCM.DeleteServer

    -- ** DeleteBackup
    , module Network.AWS.OpsWorksCM.DeleteBackup

    -- ** DescribeEvents
    , module Network.AWS.OpsWorksCM.DescribeEvents

    -- ** DisassociateNode
    , module Network.AWS.OpsWorksCM.DisassociateNode

    -- ** CreateBackup
    , module Network.AWS.OpsWorksCM.CreateBackup

    -- ** UpdateServerEngineAttributes
    , module Network.AWS.OpsWorksCM.UpdateServerEngineAttributes

    -- ** StartMaintenance
    , module Network.AWS.OpsWorksCM.StartMaintenance

    -- ** DescribeBackups
    , module Network.AWS.OpsWorksCM.DescribeBackups

    -- ** CreateServer
    , module Network.AWS.OpsWorksCM.CreateServer

    -- ** RestoreServer
    , module Network.AWS.OpsWorksCM.RestoreServer

    -- ** DescribeNodeAssociationStatus
    , module Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus

    -- ** DescribeAccountAttributes
    , module Network.AWS.OpsWorksCM.DescribeAccountAttributes

    -- ** DescribeServers
    , module Network.AWS.OpsWorksCM.DescribeServers

    -- * Types

    -- ** BackupStatus
    , BackupStatus (..)

    -- ** BackupType
    , BackupType (..)

    -- ** MaintenanceStatus
    , MaintenanceStatus (..)

    -- ** NodeAssociationStatus
    , NodeAssociationStatus (..)

    -- ** ServerStatus
    , ServerStatus (..)

    -- ** AccountAttribute
    , AccountAttribute
    , accountAttribute
    , aaUsed
    , aaMaximum
    , aaName

    -- ** Backup
    , Backup
    , backup
    , bEngineVersion
    , bServiceRoleARN
    , bStatus
    , bInstanceProfileARN
    , bSecurityGroupIds
    , bStatusDescription
    , bServerName
    , bSubnetIds
    , bKeyPair
    , bCreatedAt
    , bBackupId
    , bEngine
    , bInstanceType
    , bEngineModel
    , bPreferredMaintenanceWindow
    , bUserARN
    , bPreferredBackupWindow
    , bS3LogURL
    , bS3DataSize
    , bBackupARN
    , bS3DataURL
    , bDescription
    , bBackupType
    , bToolsVersion

    -- ** EngineAttribute
    , EngineAttribute
    , engineAttribute
    , eaValue
    , eaName

    -- ** Server
    , Server
    , server
    , sEngineVersion
    , sServiceRoleARN
    , sDisableAutomatedBackup
    , sStatus
    , sInstanceProfileARN
    , sSecurityGroupIds
    , sAssociatePublicIPAddress
    , sServerName
    , sSubnetIds
    , sKeyPair
    , sCreatedAt
    , sServerARN
    , sEngine
    , sMaintenanceStatus
    , sInstanceType
    , sEngineModel
    , sEngineAttributes
    , sPreferredMaintenanceWindow
    , sPreferredBackupWindow
    , sStatusReason
    , sEndpoint
    , sCloudFormationStackARN
    , sBackupRetentionCount

    -- ** ServerEvent
    , ServerEvent
    , serverEvent
    , seLogURL
    , seServerName
    , seCreatedAt
    , seMessage
    ) where

import Network.AWS.OpsWorksCM.AssociateNode
import Network.AWS.OpsWorksCM.CreateBackup
import Network.AWS.OpsWorksCM.CreateServer
import Network.AWS.OpsWorksCM.DeleteBackup
import Network.AWS.OpsWorksCM.DeleteServer
import Network.AWS.OpsWorksCM.DescribeAccountAttributes
import Network.AWS.OpsWorksCM.DescribeBackups
import Network.AWS.OpsWorksCM.DescribeEvents
import Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
import Network.AWS.OpsWorksCM.DescribeServers
import Network.AWS.OpsWorksCM.DisassociateNode
import Network.AWS.OpsWorksCM.RestoreServer
import Network.AWS.OpsWorksCM.StartMaintenance
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.UpdateServer
import Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
import Network.AWS.OpsWorksCM.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'OpsWorksCM'.
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
