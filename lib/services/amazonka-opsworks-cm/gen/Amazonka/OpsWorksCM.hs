{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.OpsWorksCM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS OpsWorks CM
--
-- AWS OpsWorks for configuration management (CM) is a service that runs
-- and manages configuration management servers. You can use AWS OpsWorks
-- CM to create and manage AWS OpsWorks for Chef Automate and AWS OpsWorks
-- for Puppet Enterprise servers, and add or remove nodes for the servers
-- to manage.
--
-- __Glossary of terms__
--
-- -   __Server__: A configuration management server that can be
--     highly-available. The configuration management server runs on an
--     Amazon Elastic Compute Cloud (EC2) instance, and may use various
--     other AWS services, such as Amazon Relational Database Service (RDS)
--     and Elastic Load Balancing. A server is a generic abstraction over
--     the configuration manager that you want to use, much like Amazon
--     RDS. In AWS OpsWorks CM, you do not start or stop servers. After you
--     create servers, they continue to run until they are deleted.
--
-- -   __Engine__: The engine is the specific configuration manager that
--     you want to use. Valid values in this release include @ChefAutomate@
--     and @Puppet@.
--
-- -   __Backup__: This is an application-level backup of the data that the
--     configuration manager stores. AWS OpsWorks CM creates an S3 bucket
--     for backups when you launch the first server. A backup maintains a
--     snapshot of a server\'s configuration-related attributes at the time
--     the backup starts.
--
-- -   __Events__: Events are always related to a server. Events are
--     written during server creation, when health checks run, when backups
--     are created, when system maintenance is performed, etc. When you
--     delete a server, the server\'s events are also deleted.
--
-- -   __Account attributes__: Every account has attributes that are
--     assigned in the AWS OpsWorks CM database. These attributes store
--     information about configuration limits (servers, backups, etc.) and
--     your customer account.
--
-- __Endpoints__
--
-- AWS OpsWorks CM supports the following endpoints, all HTTPS. You must
-- connect to one of the following endpoints. Your servers can only be
-- accessed or managed within the endpoint in which they are created.
--
-- -   opsworks-cm.us-east-1.amazonaws.com
--
-- -   opsworks-cm.us-east-2.amazonaws.com
--
-- -   opsworks-cm.us-west-1.amazonaws.com
--
-- -   opsworks-cm.us-west-2.amazonaws.com
--
-- -   opsworks-cm.ap-northeast-1.amazonaws.com
--
-- -   opsworks-cm.ap-southeast-1.amazonaws.com
--
-- -   opsworks-cm.ap-southeast-2.amazonaws.com
--
-- -   opsworks-cm.eu-central-1.amazonaws.com
--
-- -   opsworks-cm.eu-west-1.amazonaws.com
--
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/opsworks-service.html AWS OpsWorks endpoints and quotas>
-- in the AWS General Reference.
--
-- __Throttling limits__
--
-- All API operations allow for five requests per second with a burst of 10
-- requests per second.
module Amazonka.OpsWorksCM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** NodeAssociated
    newNodeAssociated,

    -- * Operations
    -- $operations

    -- ** AssociateNode
    AssociateNode (AssociateNode'),
    newAssociateNode,
    AssociateNodeResponse (AssociateNodeResponse'),
    newAssociateNodeResponse,

    -- ** CreateBackup
    CreateBackup (CreateBackup'),
    newCreateBackup,
    CreateBackupResponse (CreateBackupResponse'),
    newCreateBackupResponse,

    -- ** CreateServer
    CreateServer (CreateServer'),
    newCreateServer,
    CreateServerResponse (CreateServerResponse'),
    newCreateServerResponse,

    -- ** DeleteBackup
    DeleteBackup (DeleteBackup'),
    newDeleteBackup,
    DeleteBackupResponse (DeleteBackupResponse'),
    newDeleteBackupResponse,

    -- ** DeleteServer
    DeleteServer (DeleteServer'),
    newDeleteServer,
    DeleteServerResponse (DeleteServerResponse'),
    newDeleteServerResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeBackups (Paginated)
    DescribeBackups (DescribeBackups'),
    newDescribeBackups,
    DescribeBackupsResponse (DescribeBackupsResponse'),
    newDescribeBackupsResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeNodeAssociationStatus
    DescribeNodeAssociationStatus (DescribeNodeAssociationStatus'),
    newDescribeNodeAssociationStatus,
    DescribeNodeAssociationStatusResponse (DescribeNodeAssociationStatusResponse'),
    newDescribeNodeAssociationStatusResponse,

    -- ** DescribeServers (Paginated)
    DescribeServers (DescribeServers'),
    newDescribeServers,
    DescribeServersResponse (DescribeServersResponse'),
    newDescribeServersResponse,

    -- ** DisassociateNode
    DisassociateNode (DisassociateNode'),
    newDisassociateNode,
    DisassociateNodeResponse (DisassociateNodeResponse'),
    newDisassociateNodeResponse,

    -- ** ExportServerEngineAttribute
    ExportServerEngineAttribute (ExportServerEngineAttribute'),
    newExportServerEngineAttribute,
    ExportServerEngineAttributeResponse (ExportServerEngineAttributeResponse'),
    newExportServerEngineAttributeResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RestoreServer
    RestoreServer (RestoreServer'),
    newRestoreServer,
    RestoreServerResponse (RestoreServerResponse'),
    newRestoreServerResponse,

    -- ** StartMaintenance
    StartMaintenance (StartMaintenance'),
    newStartMaintenance,
    StartMaintenanceResponse (StartMaintenanceResponse'),
    newStartMaintenanceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateServer
    UpdateServer (UpdateServer'),
    newUpdateServer,
    UpdateServerResponse (UpdateServerResponse'),
    newUpdateServerResponse,

    -- ** UpdateServerEngineAttributes
    UpdateServerEngineAttributes (UpdateServerEngineAttributes'),
    newUpdateServerEngineAttributes,
    UpdateServerEngineAttributesResponse (UpdateServerEngineAttributesResponse'),
    newUpdateServerEngineAttributesResponse,

    -- * Types

    -- ** BackupStatus
    BackupStatus (..),

    -- ** BackupType
    BackupType (..),

    -- ** MaintenanceStatus
    MaintenanceStatus (..),

    -- ** NodeAssociationStatus
    NodeAssociationStatus (..),

    -- ** ServerStatus
    ServerStatus (..),

    -- ** AccountAttribute
    AccountAttribute (AccountAttribute'),
    newAccountAttribute,

    -- ** Backup
    Backup (Backup'),
    newBackup,

    -- ** EngineAttribute
    EngineAttribute (EngineAttribute'),
    newEngineAttribute,

    -- ** Server
    Server (Server'),
    newServer,

    -- ** ServerEvent
    ServerEvent (ServerEvent'),
    newServerEvent,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.OpsWorksCM.AssociateNode
import Amazonka.OpsWorksCM.CreateBackup
import Amazonka.OpsWorksCM.CreateServer
import Amazonka.OpsWorksCM.DeleteBackup
import Amazonka.OpsWorksCM.DeleteServer
import Amazonka.OpsWorksCM.DescribeAccountAttributes
import Amazonka.OpsWorksCM.DescribeBackups
import Amazonka.OpsWorksCM.DescribeEvents
import Amazonka.OpsWorksCM.DescribeNodeAssociationStatus
import Amazonka.OpsWorksCM.DescribeServers
import Amazonka.OpsWorksCM.DisassociateNode
import Amazonka.OpsWorksCM.ExportServerEngineAttribute
import Amazonka.OpsWorksCM.Lens
import Amazonka.OpsWorksCM.ListTagsForResource
import Amazonka.OpsWorksCM.RestoreServer
import Amazonka.OpsWorksCM.StartMaintenance
import Amazonka.OpsWorksCM.TagResource
import Amazonka.OpsWorksCM.Types
import Amazonka.OpsWorksCM.UntagResource
import Amazonka.OpsWorksCM.UpdateServer
import Amazonka.OpsWorksCM.UpdateServerEngineAttributes
import Amazonka.OpsWorksCM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'OpsWorksCM'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
