{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.BackupGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Backup gateway
--
-- Backup gateway connects Backup to your hypervisor, so you can create,
-- store, and restore backups of your virtual machines (VMs) anywhere,
-- whether on-premises or in the VMware Cloud (VMC) on Amazon Web Services.
--
-- Add on-premises resources by connecting to a hypervisor through a
-- gateway. Backup will automatically discover the resources in your
-- hypervisor.
--
-- Use Backup to assign virtual or on-premises resources to a backup plan,
-- or run on-demand backups. Once you have backed up your resources, you
-- can view them and restore them like any resource supported by Backup.
--
-- To download the Amazon Web Services software to get started, navigate to
-- the Backup console, choose __Gateways__, then choose __Create gateway__.
module Amazonka.BackupGateway
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateGatewayToServer
    AssociateGatewayToServer (AssociateGatewayToServer'),
    newAssociateGatewayToServer,
    AssociateGatewayToServerResponse (AssociateGatewayToServerResponse'),
    newAssociateGatewayToServerResponse,

    -- ** CreateGateway
    CreateGateway (CreateGateway'),
    newCreateGateway,
    CreateGatewayResponse (CreateGatewayResponse'),
    newCreateGatewayResponse,

    -- ** DeleteGateway
    DeleteGateway (DeleteGateway'),
    newDeleteGateway,
    DeleteGatewayResponse (DeleteGatewayResponse'),
    newDeleteGatewayResponse,

    -- ** DeleteHypervisor
    DeleteHypervisor (DeleteHypervisor'),
    newDeleteHypervisor,
    DeleteHypervisorResponse (DeleteHypervisorResponse'),
    newDeleteHypervisorResponse,

    -- ** DisassociateGatewayFromServer
    DisassociateGatewayFromServer (DisassociateGatewayFromServer'),
    newDisassociateGatewayFromServer,
    DisassociateGatewayFromServerResponse (DisassociateGatewayFromServerResponse'),
    newDisassociateGatewayFromServerResponse,

    -- ** GetGateway
    GetGateway (GetGateway'),
    newGetGateway,
    GetGatewayResponse (GetGatewayResponse'),
    newGetGatewayResponse,

    -- ** GetVirtualMachine
    GetVirtualMachine (GetVirtualMachine'),
    newGetVirtualMachine,
    GetVirtualMachineResponse (GetVirtualMachineResponse'),
    newGetVirtualMachineResponse,

    -- ** ImportHypervisorConfiguration
    ImportHypervisorConfiguration (ImportHypervisorConfiguration'),
    newImportHypervisorConfiguration,
    ImportHypervisorConfigurationResponse (ImportHypervisorConfigurationResponse'),
    newImportHypervisorConfigurationResponse,

    -- ** ListGateways (Paginated)
    ListGateways (ListGateways'),
    newListGateways,
    ListGatewaysResponse (ListGatewaysResponse'),
    newListGatewaysResponse,

    -- ** ListHypervisors (Paginated)
    ListHypervisors (ListHypervisors'),
    newListHypervisors,
    ListHypervisorsResponse (ListHypervisorsResponse'),
    newListHypervisorsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListVirtualMachines (Paginated)
    ListVirtualMachines (ListVirtualMachines'),
    newListVirtualMachines,
    ListVirtualMachinesResponse (ListVirtualMachinesResponse'),
    newListVirtualMachinesResponse,

    -- ** PutMaintenanceStartTime
    PutMaintenanceStartTime (PutMaintenanceStartTime'),
    newPutMaintenanceStartTime,
    PutMaintenanceStartTimeResponse (PutMaintenanceStartTimeResponse'),
    newPutMaintenanceStartTimeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestHypervisorConfiguration
    TestHypervisorConfiguration (TestHypervisorConfiguration'),
    newTestHypervisorConfiguration,
    TestHypervisorConfigurationResponse (TestHypervisorConfigurationResponse'),
    newTestHypervisorConfigurationResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateGatewayInformation
    UpdateGatewayInformation (UpdateGatewayInformation'),
    newUpdateGatewayInformation,
    UpdateGatewayInformationResponse (UpdateGatewayInformationResponse'),
    newUpdateGatewayInformationResponse,

    -- ** UpdateGatewaySoftwareNow
    UpdateGatewaySoftwareNow (UpdateGatewaySoftwareNow'),
    newUpdateGatewaySoftwareNow,
    UpdateGatewaySoftwareNowResponse (UpdateGatewaySoftwareNowResponse'),
    newUpdateGatewaySoftwareNowResponse,

    -- ** UpdateHypervisor
    UpdateHypervisor (UpdateHypervisor'),
    newUpdateHypervisor,
    UpdateHypervisorResponse (UpdateHypervisorResponse'),
    newUpdateHypervisorResponse,

    -- * Types

    -- ** GatewayType
    GatewayType (..),

    -- ** HypervisorState
    HypervisorState (..),

    -- ** Gateway
    Gateway (Gateway'),
    newGateway,

    -- ** GatewayDetails
    GatewayDetails (GatewayDetails'),
    newGatewayDetails,

    -- ** Hypervisor
    Hypervisor (Hypervisor'),
    newHypervisor,

    -- ** MaintenanceStartTime
    MaintenanceStartTime (MaintenanceStartTime'),
    newMaintenanceStartTime,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VirtualMachine
    VirtualMachine (VirtualMachine'),
    newVirtualMachine,

    -- ** VirtualMachineDetails
    VirtualMachineDetails (VirtualMachineDetails'),
    newVirtualMachineDetails,
  )
where

import Amazonka.BackupGateway.AssociateGatewayToServer
import Amazonka.BackupGateway.CreateGateway
import Amazonka.BackupGateway.DeleteGateway
import Amazonka.BackupGateway.DeleteHypervisor
import Amazonka.BackupGateway.DisassociateGatewayFromServer
import Amazonka.BackupGateway.GetGateway
import Amazonka.BackupGateway.GetVirtualMachine
import Amazonka.BackupGateway.ImportHypervisorConfiguration
import Amazonka.BackupGateway.Lens
import Amazonka.BackupGateway.ListGateways
import Amazonka.BackupGateway.ListHypervisors
import Amazonka.BackupGateway.ListTagsForResource
import Amazonka.BackupGateway.ListVirtualMachines
import Amazonka.BackupGateway.PutMaintenanceStartTime
import Amazonka.BackupGateway.TagResource
import Amazonka.BackupGateway.TestHypervisorConfiguration
import Amazonka.BackupGateway.Types
import Amazonka.BackupGateway.UntagResource
import Amazonka.BackupGateway.UpdateGatewayInformation
import Amazonka.BackupGateway.UpdateGatewaySoftwareNow
import Amazonka.BackupGateway.UpdateHypervisor
import Amazonka.BackupGateway.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'BackupGateway'.

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
