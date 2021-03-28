{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon WorkSpaces Service__ 
--
-- Amazon WorkSpaces enables you to provision virtual, cloud-based Microsoft Windows and Amazon Linux desktops for your users.
module Network.AWS.WorkSpaces
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** UnsupportedNetworkConfigurationException
    , _UnsupportedNetworkConfigurationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** ResourceCreationFailedException
    , _ResourceCreationFailedException

    -- ** ResourceUnavailableException
    , _ResourceUnavailableException

    -- ** InvalidParameterValuesException
    , _InvalidParameterValuesException

    -- ** ResourceAssociatedException
    , _ResourceAssociatedException

    -- ** OperationInProgressException
    , _OperationInProgressException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** ResourceLimitExceededException
    , _ResourceLimitExceededException

    -- ** InvalidResourceStateException
    , _InvalidResourceStateException

    -- ** OperationNotSupportedException
    , _OperationNotSupportedException

    -- ** UnsupportedWorkspaceConfigurationException
    , _UnsupportedWorkspaceConfigurationException

    -- ** WorkspacesDefaultRoleNotFoundException
    , _WorkspacesDefaultRoleNotFoundException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateConnectionAlias 
    , module Network.AWS.WorkSpaces.AssociateConnectionAlias

    -- ** DescribeAccount 
    , module Network.AWS.WorkSpaces.DescribeAccount

    -- ** RevokeIpRules 
    , module Network.AWS.WorkSpaces.RevokeIpRules

    -- ** DescribeWorkspaceImages (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspaceImages

    -- ** ModifyWorkspaceProperties 
    , module Network.AWS.WorkSpaces.ModifyWorkspaceProperties

    -- ** DeregisterWorkspaceDirectory 
    , module Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory

    -- ** MigrateWorkspace 
    , module Network.AWS.WorkSpaces.MigrateWorkspace

    -- ** DescribeTags 
    , module Network.AWS.WorkSpaces.DescribeTags

    -- ** DescribeWorkspaceDirectories (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories

    -- ** DisassociateIpGroups 
    , module Network.AWS.WorkSpaces.DisassociateIpGroups

    -- ** DescribeWorkspaceBundles (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspaceBundles

    -- ** AuthorizeIpRules 
    , module Network.AWS.WorkSpaces.AuthorizeIpRules

    -- ** DescribeWorkspaceImagePermissions 
    , module Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions

    -- ** RebuildWorkspaces 
    , module Network.AWS.WorkSpaces.RebuildWorkspaces

    -- ** ImportWorkspaceImage 
    , module Network.AWS.WorkSpaces.ImportWorkspaceImage

    -- ** ModifyWorkspaceState 
    , module Network.AWS.WorkSpaces.ModifyWorkspaceState

    -- ** CreateIpGroup 
    , module Network.AWS.WorkSpaces.CreateIpGroup

    -- ** DisassociateConnectionAlias 
    , module Network.AWS.WorkSpaces.DisassociateConnectionAlias

    -- ** ModifyWorkspaceCreationProperties 
    , module Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties

    -- ** RegisterWorkspaceDirectory 
    , module Network.AWS.WorkSpaces.RegisterWorkspaceDirectory

    -- ** RestoreWorkspace 
    , module Network.AWS.WorkSpaces.RestoreWorkspace

    -- ** DescribeConnectionAliasPermissions 
    , module Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions

    -- ** CreateTags 
    , module Network.AWS.WorkSpaces.CreateTags

    -- ** DeleteTags 
    , module Network.AWS.WorkSpaces.DeleteTags

    -- ** ModifyWorkspaceAccessProperties 
    , module Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties

    -- ** UpdateRulesOfIpGroup 
    , module Network.AWS.WorkSpaces.UpdateRulesOfIpGroup

    -- ** DeleteWorkspaceImage 
    , module Network.AWS.WorkSpaces.DeleteWorkspaceImage

    -- ** StopWorkspaces 
    , module Network.AWS.WorkSpaces.StopWorkspaces

    -- ** AssociateIpGroups 
    , module Network.AWS.WorkSpaces.AssociateIpGroups

    -- ** ModifySelfservicePermissions 
    , module Network.AWS.WorkSpaces.ModifySelfservicePermissions

    -- ** DeleteConnectionAlias 
    , module Network.AWS.WorkSpaces.DeleteConnectionAlias

    -- ** DescribeWorkspacesConnectionStatus (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus

    -- ** CreateConnectionAlias 
    , module Network.AWS.WorkSpaces.CreateConnectionAlias

    -- ** RebootWorkspaces 
    , module Network.AWS.WorkSpaces.RebootWorkspaces

    -- ** DeleteIpGroup 
    , module Network.AWS.WorkSpaces.DeleteIpGroup

    -- ** CopyWorkspaceImage 
    , module Network.AWS.WorkSpaces.CopyWorkspaceImage

    -- ** DescribeWorkspaceSnapshots 
    , module Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots

    -- ** TerminateWorkspaces 
    , module Network.AWS.WorkSpaces.TerminateWorkspaces

    -- ** UpdateConnectionAliasPermission 
    , module Network.AWS.WorkSpaces.UpdateConnectionAliasPermission

    -- ** CreateWorkspaces 
    , module Network.AWS.WorkSpaces.CreateWorkspaces

    -- ** DescribeClientProperties 
    , module Network.AWS.WorkSpaces.DescribeClientProperties

    -- ** ModifyClientProperties 
    , module Network.AWS.WorkSpaces.ModifyClientProperties

    -- ** DescribeIpGroups (Paginated)
    , module Network.AWS.WorkSpaces.DescribeIpGroups

    -- ** ListAvailableManagementCidrRanges (Paginated)
    , module Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges

    -- ** UpdateWorkspaceImagePermission 
    , module Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission

    -- ** DescribeWorkspaces (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspaces

    -- ** DescribeConnectionAliases 
    , module Network.AWS.WorkSpaces.DescribeConnectionAliases

    -- ** StartWorkspaces 
    , module Network.AWS.WorkSpaces.StartWorkspaces

    -- ** DescribeAccountModifications (Paginated)
    , module Network.AWS.WorkSpaces.DescribeAccountModifications

    -- ** ModifyAccount 
    , module Network.AWS.WorkSpaces.ModifyAccount

    -- * Types

    -- ** WorkspaceRequest
    , WorkspaceRequest (..)
    , mkWorkspaceRequest
    , wrDirectoryId
    , wrUserName
    , wrBundleId
    , wrRootVolumeEncryptionEnabled
    , wrTags
    , wrUserVolumeEncryptionEnabled
    , wrVolumeEncryptionKey
    , wrWorkspaceProperties

    -- ** WorkspaceDirectory
    , WorkspaceDirectory (..)
    , mkWorkspaceDirectory
    , wdAlias
    , wdCustomerUserName
    , wdDirectoryId
    , wdDirectoryName
    , wdDirectoryType
    , wdDnsIpAddresses
    , wdIamRoleId
    , wdRegistrationCode
    , wdSelfservicePermissions
    , wdState
    , wdSubnetIds
    , wdTenancy
    , wdWorkspaceAccessProperties
    , wdWorkspaceCreationProperties
    , wdWorkspaceSecurityGroupId
    , wdIpGroupIds

    -- ** RegistrationCode
    , RegistrationCode (..)

    -- ** DedicatedTenancySupportResultEnum
    , DedicatedTenancySupportResultEnum (..)

    -- ** Snapshot
    , Snapshot (..)
    , mkSnapshot
    , sSnapshotTime

    -- ** DirectoryId
    , DirectoryId (..)

    -- ** WorkspaceImageErrorCode
    , WorkspaceImageErrorCode (..)

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** ModificationResourceEnum
    , ModificationResourceEnum (..)

    -- ** WorkspaceImageId
    , WorkspaceImageId (..)

    -- ** IpGroupDesc
    , IpGroupDesc (..)

    -- ** AccountModification
    , AccountModification (..)
    , mkAccountModification
    , amDedicatedTenancyManagementCidrRange
    , amDedicatedTenancySupport
    , amErrorCode
    , amErrorMessage
    , amModificationState
    , amStartTime

    -- ** IpAddress
    , IpAddress (..)

    -- ** ConnectionAliasState
    , ConnectionAliasState (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** Application
    , Application (..)

    -- ** IpRuleItem
    , IpRuleItem (..)
    , mkIpRuleItem
    , iriIpRule
    , iriRuleDesc

    -- ** DedicatedTenancySupportEnum
    , DedicatedTenancySupportEnum (..)

    -- ** ManagementCidrRangeConstraint
    , ManagementCidrRangeConstraint (..)

    -- ** Compute
    , Compute (..)

    -- ** Workspace
    , Workspace (..)
    , mkWorkspace
    , wBundleId
    , wComputerName
    , wDirectoryId
    , wErrorCode
    , wErrorMessage
    , wIpAddress
    , wModificationStates
    , wRootVolumeEncryptionEnabled
    , wState
    , wSubnetId
    , wUserName
    , wUserVolumeEncryptionEnabled
    , wVolumeEncryptionKey
    , wWorkspaceId
    , wWorkspaceProperties

    -- ** WorkspaceImageName
    , WorkspaceImageName (..)

    -- ** OperatingSystem
    , OperatingSystem (..)
    , mkOperatingSystem
    , osType

    -- ** ConnectionAlias
    , ConnectionAlias (..)
    , mkConnectionAlias
    , caAliasId
    , caAssociations
    , caConnectionString
    , caOwnerAccountId
    , caState

    -- ** ClientProperties
    , ClientProperties (..)
    , mkClientProperties
    , cpReconnectEnabled

    -- ** RebuildRequest
    , RebuildRequest (..)
    , mkRebuildRequest
    , rrWorkspaceId

    -- ** FailedCreateWorkspaceRequest
    , FailedCreateWorkspaceRequest (..)
    , mkFailedCreateWorkspaceRequest
    , fcwrErrorCode
    , fcwrErrorMessage
    , fcwrWorkspaceRequest

    -- ** WorkspaceDirectoryState
    , WorkspaceDirectoryState (..)

    -- ** IpGroupId
    , IpGroupId (..)

    -- ** TargetWorkspaceState
    , TargetWorkspaceState (..)

    -- ** DedicatedTenancyModificationStateEnum
    , DedicatedTenancyModificationStateEnum (..)

    -- ** Ec2ImageId
    , Ec2ImageId (..)

    -- ** Alias
    , Alias (..)

    -- ** UserName
    , UserName (..)

    -- ** SubnetId
    , SubnetId (..)

    -- ** FailedWorkspaceChangeRequest
    , FailedWorkspaceChangeRequest (..)
    , mkFailedWorkspaceChangeRequest
    , fwcrErrorCode
    , fwcrErrorMessage
    , fwcrWorkspaceId

    -- ** ModificationState
    , ModificationState (..)
    , mkModificationState
    , msResource
    , msState

    -- ** IpGroupName
    , IpGroupName (..)

    -- ** ClientPropertiesResult
    , ClientPropertiesResult (..)
    , mkClientPropertiesResult
    , cprClientProperties
    , cprResourceId

    -- ** BundleId
    , BundleId (..)

    -- ** DedicatedTenancyManagementCidrRange
    , DedicatedTenancyManagementCidrRange (..)

    -- ** StopRequest
    , StopRequest (..)
    , mkStopRequest
    , srWorkspaceId

    -- ** SecurityGroupId
    , SecurityGroupId (..)

    -- ** WorkspaceImageDescription
    , WorkspaceImageDescription (..)

    -- ** AwsAccount
    , AwsAccount (..)

    -- ** WorkspaceProperties
    , WorkspaceProperties (..)
    , mkWorkspaceProperties
    , wpComputeTypeName
    , wpRootVolumeSizeGib
    , wpRunningMode
    , wpRunningModeAutoStopTimeoutInMinutes
    , wpUserVolumeSizeGib

    -- ** RunningMode
    , RunningMode (..)

    -- ** ImagePermission
    , ImagePermission (..)
    , mkImagePermission
    , ipSharedAccountId

    -- ** WorkspaceDirectoryType
    , WorkspaceDirectoryType (..)

    -- ** NonEmptyString
    , NonEmptyString (..)

    -- ** RootStorage
    , RootStorage (..)
    , mkRootStorage
    , rsCapacity

    -- ** ImageType
    , ImageType (..)

    -- ** IpRule
    , IpRule (..)

    -- ** WorkspaceConnectionStatus
    , WorkspaceConnectionStatus (..)
    , mkWorkspaceConnectionStatus
    , wcsConnectionState
    , wcsConnectionStateCheckTimestamp
    , wcsLastKnownUserConnectionTimestamp
    , wcsWorkspaceId

    -- ** WorkspaceState
    , WorkspaceState (..)

    -- ** Tenancy
    , Tenancy (..)

    -- ** ModificationStateEnum
    , ModificationStateEnum (..)

    -- ** ReconnectEnum
    , ReconnectEnum (..)

    -- ** WorkspaceImage
    , WorkspaceImage (..)
    , mkWorkspaceImage
    , wiCreated
    , wiDescription
    , wiErrorCode
    , wiErrorMessage
    , wiImageId
    , wiName
    , wiOperatingSystem
    , wiOwnerAccountId
    , wiRequiredTenancy
    , wiState

    -- ** BundleOwner
    , BundleOwner (..)

    -- ** AssociationStatus
    , AssociationStatus (..)

    -- ** WorkspaceCreationProperties
    , WorkspaceCreationProperties (..)
    , mkWorkspaceCreationProperties
    , wcpCustomSecurityGroupId
    , wcpDefaultOu
    , wcpEnableInternetAccess
    , wcpEnableMaintenanceMode
    , wcpEnableWorkDocs
    , wcpUserEnabledAsLocalAdministrator

    -- ** ComputeType
    , ComputeType (..)
    , mkComputeType
    , ctName

    -- ** ConnectionIdentifier
    , ConnectionIdentifier (..)

    -- ** ConnectionAliasId
    , ConnectionAliasId (..)

    -- ** WorkspaceImageState
    , WorkspaceImageState (..)

    -- ** VolumeEncryptionKey
    , VolumeEncryptionKey (..)

    -- ** RebootRequest
    , RebootRequest (..)
    , mkRebootRequest
    , rWorkspaceId

    -- ** UserStorage
    , UserStorage (..)
    , mkUserStorage
    , usCapacity

    -- ** AccessPropertyValue
    , AccessPropertyValue (..)

    -- ** ComputerName
    , ComputerName (..)

    -- ** OperatingSystemType
    , OperatingSystemType (..)

    -- ** WorkspacesIpGroup
    , WorkspacesIpGroup (..)
    , mkWorkspacesIpGroup
    , wigGroupDesc
    , wigGroupId
    , wigGroupName
    , wigUserRules

    -- ** WorkspaceAccessProperties
    , WorkspaceAccessProperties (..)
    , mkWorkspaceAccessProperties
    , wapDeviceTypeAndroid
    , wapDeviceTypeChromeOs
    , wapDeviceTypeIos
    , wapDeviceTypeOsx
    , wapDeviceTypeWeb
    , wapDeviceTypeWindows
    , wapDeviceTypeZeroClient

    -- ** TerminateRequest
    , TerminateRequest (..)
    , mkTerminateRequest
    , trWorkspaceId

    -- ** WorkspaceId
    , WorkspaceId (..)

    -- ** WorkspaceImageIngestionProcess
    , WorkspaceImageIngestionProcess (..)

    -- ** ConnectionAliasPermission
    , ConnectionAliasPermission (..)
    , mkConnectionAliasPermission
    , capSharedAccountId
    , capAllowAssociation

    -- ** ConnectionString
    , ConnectionString (..)

    -- ** Description
    , Description (..)

    -- ** DefaultWorkspaceCreationProperties
    , DefaultWorkspaceCreationProperties (..)
    , mkDefaultWorkspaceCreationProperties
    , dwcpCustomSecurityGroupId
    , dwcpDefaultOu
    , dwcpEnableInternetAccess
    , dwcpEnableMaintenanceMode
    , dwcpEnableWorkDocs
    , dwcpUserEnabledAsLocalAdministrator

    -- ** DirectoryName
    , DirectoryName (..)

    -- ** ConnectionAliasAssociation
    , ConnectionAliasAssociation (..)
    , mkConnectionAliasAssociation
    , caaAssociatedAccountId
    , caaAssociationStatus
    , caaConnectionIdentifier
    , caaResourceId

    -- ** DefaultOu
    , DefaultOu (..)

    -- ** ConnectionState
    , ConnectionState (..)

    -- ** SelfservicePermissions
    , SelfservicePermissions (..)
    , mkSelfservicePermissions
    , spChangeComputeType
    , spIncreaseVolumeSize
    , spRebuildWorkspace
    , spRestartWorkspace
    , spSwitchRunningMode

    -- ** WorkspaceBundle
    , WorkspaceBundle (..)
    , mkWorkspaceBundle
    , wbBundleId
    , wbComputeType
    , wbDescription
    , wbImageId
    , wbLastUpdatedTime
    , wbName
    , wbOwner
    , wbRootStorage
    , wbUserStorage

    -- ** WorkspaceImageRequiredTenancy
    , WorkspaceImageRequiredTenancy (..)

    -- ** StartRequest
    , StartRequest (..)
    , mkStartRequest
    , sWorkspaceId

    -- ** CustomerUserName
    , CustomerUserName (..)

    -- ** IamRoleId
    , IamRoleId (..)

    -- ** WorkspaceSecurityGroupId
    , WorkspaceSecurityGroupId (..)

    -- ** AliasId
    , AliasId (..)

    -- ** NextToken
    , NextToken (..)

    -- ** ResourceId
    , ResourceId (..)

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** GroupId
    , GroupId (..)

    -- ** RuleDesc
    , RuleDesc (..)

    -- ** SourceWorkspaceId
    , SourceWorkspaceId (..)

    -- ** TargetWorkspaceId
    , TargetWorkspaceId (..)

    -- ** OwnerAccountId
    , OwnerAccountId (..)

    -- ** SourceRegion
    , SourceRegion (..)

    -- ** SharedAccountId
    , SharedAccountId (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Waiters
import Network.AWS.WorkSpaces.AssociateConnectionAlias
import Network.AWS.WorkSpaces.DescribeAccount
import Network.AWS.WorkSpaces.RevokeIpRules
import Network.AWS.WorkSpaces.DescribeWorkspaceImages
import Network.AWS.WorkSpaces.ModifyWorkspaceProperties
import Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
import Network.AWS.WorkSpaces.MigrateWorkspace
import Network.AWS.WorkSpaces.DescribeTags
import Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
import Network.AWS.WorkSpaces.DisassociateIpGroups
import Network.AWS.WorkSpaces.DescribeWorkspaceBundles
import Network.AWS.WorkSpaces.AuthorizeIpRules
import Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
import Network.AWS.WorkSpaces.RebuildWorkspaces
import Network.AWS.WorkSpaces.ImportWorkspaceImage
import Network.AWS.WorkSpaces.ModifyWorkspaceState
import Network.AWS.WorkSpaces.CreateIpGroup
import Network.AWS.WorkSpaces.DisassociateConnectionAlias
import Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
import Network.AWS.WorkSpaces.RegisterWorkspaceDirectory
import Network.AWS.WorkSpaces.RestoreWorkspace
import Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
import Network.AWS.WorkSpaces.CreateTags
import Network.AWS.WorkSpaces.DeleteTags
import Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
import Network.AWS.WorkSpaces.UpdateRulesOfIpGroup
import Network.AWS.WorkSpaces.DeleteWorkspaceImage
import Network.AWS.WorkSpaces.StopWorkspaces
import Network.AWS.WorkSpaces.AssociateIpGroups
import Network.AWS.WorkSpaces.ModifySelfservicePermissions
import Network.AWS.WorkSpaces.DeleteConnectionAlias
import Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
import Network.AWS.WorkSpaces.CreateConnectionAlias
import Network.AWS.WorkSpaces.RebootWorkspaces
import Network.AWS.WorkSpaces.DeleteIpGroup
import Network.AWS.WorkSpaces.CopyWorkspaceImage
import Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
import Network.AWS.WorkSpaces.TerminateWorkspaces
import Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DescribeClientProperties
import Network.AWS.WorkSpaces.ModifyClientProperties
import Network.AWS.WorkSpaces.DescribeIpGroups
import Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
import Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
import Network.AWS.WorkSpaces.DescribeWorkspaces
import Network.AWS.WorkSpaces.DescribeConnectionAliases
import Network.AWS.WorkSpaces.StartWorkspaces
import Network.AWS.WorkSpaces.DescribeAccountModifications
import Network.AWS.WorkSpaces.ModifyAccount
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'WorkSpaces'.
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
