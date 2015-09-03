{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elastic Compute Cloud
--
-- Amazon Elastic Compute Cloud (Amazon EC2) provides resizable computing
-- capacity in the Amazon Web Services (AWS) cloud. Using Amazon EC2
-- eliminates your need to invest in hardware up front, so you can develop
-- and deploy applications faster.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.EC2
    (
    -- * Service Configuration
      eC2

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** InstanceTerminated
    , instanceTerminated

    -- ** VolumeInUse
    , volumeInUse

    -- ** SubnetAvailable
    , subnetAvailable

    -- ** SystemStatusOK
    , systemStatusOK

    -- ** CustomerGatewayAvailable
    , customerGatewayAvailable

    -- ** ConversionTaskCompleted
    , conversionTaskCompleted

    -- ** InstanceStopped
    , instanceStopped

    -- ** ConversionTaskDeleted
    , conversionTaskDeleted

    -- ** PasswordDataAvailable
    , passwordDataAvailable

    -- ** InstanceRunning
    , instanceRunning

    -- ** SpotInstanceRequestFulfilled
    , spotInstanceRequestFulfilled

    -- ** VPCAvailable
    , vpcAvailable

    -- ** ExportTaskCompleted
    , exportTaskCompleted

    -- ** VPNConnectionAvailable
    , vpnConnectionAvailable

    -- ** ExportTaskCancelled
    , exportTaskCancelled

    -- ** VolumeDeleted
    , volumeDeleted

    -- ** BundleTaskComplete
    , bundleTaskComplete

    -- ** VPNConnectionDeleted
    , vpnConnectionDeleted

    -- ** ConversionTaskCancelled
    , conversionTaskCancelled

    -- ** ImageAvailable
    , imageAvailable

    -- ** SnapshotCompleted
    , snapshotCompleted

    -- ** InstanceExists
    , instanceExists

    -- ** InstanceStatusOK
    , instanceStatusOK

    -- ** VolumeAvailable
    , volumeAvailable

    -- * Operations
    -- $operations

    -- ** ImportInstance
    , module Network.AWS.EC2.ImportInstance

    -- ** RevokeSecurityGroupEgress
    , module Network.AWS.EC2.RevokeSecurityGroupEgress

    -- ** CreateVPNGateway
    , module Network.AWS.EC2.CreateVPNGateway

    -- ** CreateNetworkACL
    , module Network.AWS.EC2.CreateNetworkACL

    -- ** DeleteKeyPair
    , module Network.AWS.EC2.DeleteKeyPair

    -- ** DescribeTags (Paginated)
    , module Network.AWS.EC2.DescribeTags

    -- ** DetachNetworkInterface
    , module Network.AWS.EC2.DetachNetworkInterface

    -- ** DetachInternetGateway
    , module Network.AWS.EC2.DetachInternetGateway

    -- ** DeleteVPCEndpoints
    , module Network.AWS.EC2.DeleteVPCEndpoints

    -- ** DeleteFlowLogs
    , module Network.AWS.EC2.DeleteFlowLogs

    -- ** DescribeVPCClassicLink
    , module Network.AWS.EC2.DescribeVPCClassicLink

    -- ** ModifySubnetAttribute
    , module Network.AWS.EC2.ModifySubnetAttribute

    -- ** DetachVolume
    , module Network.AWS.EC2.DetachVolume

    -- ** CancelBundleTask
    , module Network.AWS.EC2.CancelBundleTask

    -- ** ReleaseAddress
    , module Network.AWS.EC2.ReleaseAddress

    -- ** CreateInternetGateway
    , module Network.AWS.EC2.CreateInternetGateway

    -- ** DeleteVPNConnection
    , module Network.AWS.EC2.DeleteVPNConnection

    -- ** DescribeBundleTasks
    , module Network.AWS.EC2.DescribeBundleTasks

    -- ** AuthorizeSecurityGroupEgress
    , module Network.AWS.EC2.AuthorizeSecurityGroupEgress

    -- ** DeregisterImage
    , module Network.AWS.EC2.DeregisterImage

    -- ** ModifyNetworkInterfaceAttribute
    , module Network.AWS.EC2.ModifyNetworkInterfaceAttribute

    -- ** CancelReservedInstancesListing
    , module Network.AWS.EC2.CancelReservedInstancesListing

    -- ** AttachClassicLinkVPC
    , module Network.AWS.EC2.AttachClassicLinkVPC

    -- ** CancelSpotFleetRequests
    , module Network.AWS.EC2.CancelSpotFleetRequests

    -- ** DescribeSpotPriceHistory (Paginated)
    , module Network.AWS.EC2.DescribeSpotPriceHistory

    -- ** DescribeDHCPOptions
    , module Network.AWS.EC2.DescribeDHCPOptions

    -- ** ImportImage
    , module Network.AWS.EC2.ImportImage

    -- ** StopInstances
    , module Network.AWS.EC2.StopInstances

    -- ** DescribeInternetGateways
    , module Network.AWS.EC2.DescribeInternetGateways

    -- ** DisableVPCClassicLink
    , module Network.AWS.EC2.DisableVPCClassicLink

    -- ** BundleInstance
    , module Network.AWS.EC2.BundleInstance

    -- ** DescribeNetworkInterfaces
    , module Network.AWS.EC2.DescribeNetworkInterfaces

    -- ** ReplaceNetworkACLAssociation
    , module Network.AWS.EC2.ReplaceNetworkACLAssociation

    -- ** DescribeAddresses
    , module Network.AWS.EC2.DescribeAddresses

    -- ** DescribeSnapshotAttribute
    , module Network.AWS.EC2.DescribeSnapshotAttribute

    -- ** ReplaceRoute
    , module Network.AWS.EC2.ReplaceRoute

    -- ** DescribeVPCEndpointServices
    , module Network.AWS.EC2.DescribeVPCEndpointServices

    -- ** AuthorizeSecurityGroupIngress
    , module Network.AWS.EC2.AuthorizeSecurityGroupIngress

    -- ** CreateVPCPeeringConnection
    , module Network.AWS.EC2.CreateVPCPeeringConnection

    -- ** DescribeSubnets
    , module Network.AWS.EC2.DescribeSubnets

    -- ** CreateTags
    , module Network.AWS.EC2.CreateTags

    -- ** PurchaseReservedInstancesOffering
    , module Network.AWS.EC2.PurchaseReservedInstancesOffering

    -- ** DeleteNetworkACLEntry
    , module Network.AWS.EC2.DeleteNetworkACLEntry

    -- ** ResetSnapshotAttribute
    , module Network.AWS.EC2.ResetSnapshotAttribute

    -- ** DescribeVPNConnections
    , module Network.AWS.EC2.DescribeVPNConnections

    -- ** DeleteRoute
    , module Network.AWS.EC2.DeleteRoute

    -- ** ReplaceNetworkACLEntry
    , module Network.AWS.EC2.ReplaceNetworkACLEntry

    -- ** DescribeVPCEndpoints
    , module Network.AWS.EC2.DescribeVPCEndpoints

    -- ** ResetInstanceAttribute
    , module Network.AWS.EC2.ResetInstanceAttribute

    -- ** AttachNetworkInterface
    , module Network.AWS.EC2.AttachNetworkInterface

    -- ** DescribeInstanceStatus (Paginated)
    , module Network.AWS.EC2.DescribeInstanceStatus

    -- ** ImportKeyPair
    , module Network.AWS.EC2.ImportKeyPair

    -- ** DeleteTags
    , module Network.AWS.EC2.DeleteTags

    -- ** ConfirmProductInstance
    , module Network.AWS.EC2.ConfirmProductInstance

    -- ** DescribeInstanceAttribute
    , module Network.AWS.EC2.DescribeInstanceAttribute

    -- ** DescribeReservedInstancesOfferings (Paginated)
    , module Network.AWS.EC2.DescribeReservedInstancesOfferings

    -- ** CreateCustomerGateway
    , module Network.AWS.EC2.CreateCustomerGateway

    -- ** DeleteSecurityGroup
    , module Network.AWS.EC2.DeleteSecurityGroup

    -- ** DeleteVPCPeeringConnection
    , module Network.AWS.EC2.DeleteVPCPeeringConnection

    -- ** AttachInternetGateway
    , module Network.AWS.EC2.AttachInternetGateway

    -- ** DescribeFlowLogs
    , module Network.AWS.EC2.DescribeFlowLogs

    -- ** RunInstances
    , module Network.AWS.EC2.RunInstances

    -- ** AssociateDHCPOptions
    , module Network.AWS.EC2.AssociateDHCPOptions

    -- ** DescribeReservedInstances
    , module Network.AWS.EC2.DescribeReservedInstances

    -- ** DescribeVPCs
    , module Network.AWS.EC2.DescribeVPCs

    -- ** DescribeConversionTasks
    , module Network.AWS.EC2.DescribeConversionTasks

    -- ** AllocateAddress
    , module Network.AWS.EC2.AllocateAddress

    -- ** CancelConversionTask
    , module Network.AWS.EC2.CancelConversionTask

    -- ** ModifyImageAttribute
    , module Network.AWS.EC2.ModifyImageAttribute

    -- ** CreateRouteTable
    , module Network.AWS.EC2.CreateRouteTable

    -- ** ReportInstanceStatus
    , module Network.AWS.EC2.ReportInstanceStatus

    -- ** AttachVolume
    , module Network.AWS.EC2.AttachVolume

    -- ** RequestSpotInstances
    , module Network.AWS.EC2.RequestSpotInstances

    -- ** DescribeVolumes
    , module Network.AWS.EC2.DescribeVolumes

    -- ** RejectVPCPeeringConnection
    , module Network.AWS.EC2.RejectVPCPeeringConnection

    -- ** DeleteVPNConnectionRoute
    , module Network.AWS.EC2.DeleteVPNConnectionRoute

    -- ** ModifyVPCEndpoint
    , module Network.AWS.EC2.ModifyVPCEndpoint

    -- ** RegisterImage
    , module Network.AWS.EC2.RegisterImage

    -- ** RevokeSecurityGroupIngress
    , module Network.AWS.EC2.RevokeSecurityGroupIngress

    -- ** ModifyReservedInstances
    , module Network.AWS.EC2.ModifyReservedInstances

    -- ** CreateFlowLogs
    , module Network.AWS.EC2.CreateFlowLogs

    -- ** DescribeSpotFleetRequests
    , module Network.AWS.EC2.DescribeSpotFleetRequests

    -- ** MoveAddressToVPC
    , module Network.AWS.EC2.MoveAddressToVPC

    -- ** DeleteNetworkACL
    , module Network.AWS.EC2.DeleteNetworkACL

    -- ** DeleteVPNGateway
    , module Network.AWS.EC2.DeleteVPNGateway

    -- ** DescribeImportImageTasks
    , module Network.AWS.EC2.DescribeImportImageTasks

    -- ** DescribeVolumeAttribute
    , module Network.AWS.EC2.DescribeVolumeAttribute

    -- ** DescribeMovingAddresses
    , module Network.AWS.EC2.DescribeMovingAddresses

    -- ** GetPasswordData
    , module Network.AWS.EC2.GetPasswordData

    -- ** CreateVPC
    , module Network.AWS.EC2.CreateVPC

    -- ** CopySnapshot
    , module Network.AWS.EC2.CopySnapshot

    -- ** DisassociateAddress
    , module Network.AWS.EC2.DisassociateAddress

    -- ** DeleteVPC
    , module Network.AWS.EC2.DeleteVPC

    -- ** CreateInstanceExportTask
    , module Network.AWS.EC2.CreateInstanceExportTask

    -- ** DescribeVPCAttribute
    , module Network.AWS.EC2.DescribeVPCAttribute

    -- ** CreateVolume
    , module Network.AWS.EC2.CreateVolume

    -- ** ModifyVolumeAttribute
    , module Network.AWS.EC2.ModifyVolumeAttribute

    -- ** DescribeSpotDatafeedSubscription
    , module Network.AWS.EC2.DescribeSpotDatafeedSubscription

    -- ** DescribePrefixLists
    , module Network.AWS.EC2.DescribePrefixLists

    -- ** DeletePlacementGroup
    , module Network.AWS.EC2.DeletePlacementGroup

    -- ** RequestSpotFleet
    , module Network.AWS.EC2.RequestSpotFleet

    -- ** CreateSubnet
    , module Network.AWS.EC2.CreateSubnet

    -- ** CreateNetworkInterface
    , module Network.AWS.EC2.CreateNetworkInterface

    -- ** DescribeSecurityGroups
    , module Network.AWS.EC2.DescribeSecurityGroups

    -- ** DescribeExportTasks
    , module Network.AWS.EC2.DescribeExportTasks

    -- ** DetachVPNGateway
    , module Network.AWS.EC2.DetachVPNGateway

    -- ** EnableVolumeIO
    , module Network.AWS.EC2.EnableVolumeIO

    -- ** DescribeInstances (Paginated)
    , module Network.AWS.EC2.DescribeInstances

    -- ** DescribeVPCPeeringConnections
    , module Network.AWS.EC2.DescribeVPCPeeringConnections

    -- ** CancelExportTask
    , module Network.AWS.EC2.CancelExportTask

    -- ** DeleteNetworkInterface
    , module Network.AWS.EC2.DeleteNetworkInterface

    -- ** ReplaceRouteTableAssociation
    , module Network.AWS.EC2.ReplaceRouteTableAssociation

    -- ** StartInstances
    , module Network.AWS.EC2.StartInstances

    -- ** CreatePlacementGroup
    , module Network.AWS.EC2.CreatePlacementGroup

    -- ** DescribeSnapshots (Paginated)
    , module Network.AWS.EC2.DescribeSnapshots

    -- ** AssociateAddress
    , module Network.AWS.EC2.AssociateAddress

    -- ** DescribeNetworkInterfaceAttribute
    , module Network.AWS.EC2.DescribeNetworkInterfaceAttribute

    -- ** ResetNetworkInterfaceAttribute
    , module Network.AWS.EC2.ResetNetworkInterfaceAttribute

    -- ** DeleteInternetGateway
    , module Network.AWS.EC2.DeleteInternetGateway

    -- ** DescribeReservedInstancesListings
    , module Network.AWS.EC2.DescribeReservedInstancesListings

    -- ** CreateVPNConnection
    , module Network.AWS.EC2.CreateVPNConnection

    -- ** DescribeImportSnapshotTasks
    , module Network.AWS.EC2.DescribeImportSnapshotTasks

    -- ** DescribeCustomerGateways
    , module Network.AWS.EC2.DescribeCustomerGateways

    -- ** DeleteSubnet
    , module Network.AWS.EC2.DeleteSubnet

    -- ** CopyImage
    , module Network.AWS.EC2.CopyImage

    -- ** CreateVPCEndpoint
    , module Network.AWS.EC2.CreateVPCEndpoint

    -- ** UnmonitorInstances
    , module Network.AWS.EC2.UnmonitorInstances

    -- ** CreateSecurityGroup
    , module Network.AWS.EC2.CreateSecurityGroup

    -- ** ImportVolume
    , module Network.AWS.EC2.ImportVolume

    -- ** DisableVGWRoutePropagation
    , module Network.AWS.EC2.DisableVGWRoutePropagation

    -- ** CreateSpotDatafeedSubscription
    , module Network.AWS.EC2.CreateSpotDatafeedSubscription

    -- ** CancelSpotInstanceRequests
    , module Network.AWS.EC2.CancelSpotInstanceRequests

    -- ** CreateRoute
    , module Network.AWS.EC2.CreateRoute

    -- ** DeleteSnapshot
    , module Network.AWS.EC2.DeleteSnapshot

    -- ** AssignPrivateIPAddresses
    , module Network.AWS.EC2.AssignPrivateIPAddresses

    -- ** ModifyInstanceAttribute
    , module Network.AWS.EC2.ModifyInstanceAttribute

    -- ** DeleteCustomerGateway
    , module Network.AWS.EC2.DeleteCustomerGateway

    -- ** DisassociateRouteTable
    , module Network.AWS.EC2.DisassociateRouteTable

    -- ** DeleteSpotDatafeedSubscription
    , module Network.AWS.EC2.DeleteSpotDatafeedSubscription

    -- ** EnableVPCClassicLink
    , module Network.AWS.EC2.EnableVPCClassicLink

    -- ** DescribePlacementGroups
    , module Network.AWS.EC2.DescribePlacementGroups

    -- ** EnableVGWRoutePropagation
    , module Network.AWS.EC2.EnableVGWRoutePropagation

    -- ** DescribeSpotFleetRequestHistory
    , module Network.AWS.EC2.DescribeSpotFleetRequestHistory

    -- ** ModifySnapshotAttribute
    , module Network.AWS.EC2.ModifySnapshotAttribute

    -- ** CreateSnapshot
    , module Network.AWS.EC2.CreateSnapshot

    -- ** CreateNetworkACLEntry
    , module Network.AWS.EC2.CreateNetworkACLEntry

    -- ** CreateReservedInstancesListing
    , module Network.AWS.EC2.CreateReservedInstancesListing

    -- ** AttachVPNGateway
    , module Network.AWS.EC2.AttachVPNGateway

    -- ** CreateDHCPOptions
    , module Network.AWS.EC2.CreateDHCPOptions

    -- ** DescribeAccountAttributes
    , module Network.AWS.EC2.DescribeAccountAttributes

    -- ** RebootInstances
    , module Network.AWS.EC2.RebootInstances

    -- ** CreateImage
    , module Network.AWS.EC2.CreateImage

    -- ** DescribeClassicLinkInstances
    , module Network.AWS.EC2.DescribeClassicLinkInstances

    -- ** TerminateInstances
    , module Network.AWS.EC2.TerminateInstances

    -- ** DescribeKeyPairs
    , module Network.AWS.EC2.DescribeKeyPairs

    -- ** CreateVPNConnectionRoute
    , module Network.AWS.EC2.CreateVPNConnectionRoute

    -- ** AssociateRouteTable
    , module Network.AWS.EC2.AssociateRouteTable

    -- ** DescribeVPNGateways
    , module Network.AWS.EC2.DescribeVPNGateways

    -- ** GetConsoleOutput
    , module Network.AWS.EC2.GetConsoleOutput

    -- ** DescribeImageAttribute
    , module Network.AWS.EC2.DescribeImageAttribute

    -- ** DeleteRouteTable
    , module Network.AWS.EC2.DeleteRouteTable

    -- ** ResetImageAttribute
    , module Network.AWS.EC2.ResetImageAttribute

    -- ** DescribeReservedInstancesModifications (Paginated)
    , module Network.AWS.EC2.DescribeReservedInstancesModifications

    -- ** DescribeSpotInstanceRequests
    , module Network.AWS.EC2.DescribeSpotInstanceRequests

    -- ** UnassignPrivateIPAddresses
    , module Network.AWS.EC2.UnassignPrivateIPAddresses

    -- ** DeleteDHCPOptions
    , module Network.AWS.EC2.DeleteDHCPOptions

    -- ** DescribeNetworkACLs
    , module Network.AWS.EC2.DescribeNetworkACLs

    -- ** CancelImportTask
    , module Network.AWS.EC2.CancelImportTask

    -- ** DetachClassicLinkVPC
    , module Network.AWS.EC2.DetachClassicLinkVPC

    -- ** DescribeRegions
    , module Network.AWS.EC2.DescribeRegions

    -- ** MonitorInstances
    , module Network.AWS.EC2.MonitorInstances

    -- ** AcceptVPCPeeringConnection
    , module Network.AWS.EC2.AcceptVPCPeeringConnection

    -- ** ImportSnapshot
    , module Network.AWS.EC2.ImportSnapshot

    -- ** DescribeVolumeStatus (Paginated)
    , module Network.AWS.EC2.DescribeVolumeStatus

    -- ** DescribeRouteTables
    , module Network.AWS.EC2.DescribeRouteTables

    -- ** DescribeAvailabilityZones
    , module Network.AWS.EC2.DescribeAvailabilityZones

    -- ** ModifyVPCAttribute
    , module Network.AWS.EC2.ModifyVPCAttribute

    -- ** DescribeImages
    , module Network.AWS.EC2.DescribeImages

    -- ** RestoreAddressToClassic
    , module Network.AWS.EC2.RestoreAddressToClassic

    -- ** CreateKeyPair
    , module Network.AWS.EC2.CreateKeyPair

    -- ** DeleteVolume
    , module Network.AWS.EC2.DeleteVolume

    -- ** DescribeSpotFleetInstances
    , module Network.AWS.EC2.DescribeSpotFleetInstances

    -- * Types

    -- ** AccountAttributeName
    , AccountAttributeName (..)

    -- ** AddressStatus
    , AddressStatus (..)

    -- ** ArchitectureValues
    , ArchitectureValues (..)

    -- ** AttachmentStatus
    , AttachmentStatus (..)

    -- ** AvailabilityZoneState
    , AvailabilityZoneState (..)

    -- ** BatchState
    , BatchState (..)

    -- ** BundleTaskState
    , BundleTaskState (..)

    -- ** CancelBatchErrorCode
    , CancelBatchErrorCode (..)

    -- ** CancelSpotInstanceRequestState
    , CancelSpotInstanceRequestState (..)

    -- ** ContainerFormat
    , ContainerFormat (..)

    -- ** ConversionTaskState
    , ConversionTaskState (..)

    -- ** CurrencyCodeValues
    , CurrencyCodeValues (..)

    -- ** DatafeedSubscriptionState
    , DatafeedSubscriptionState (..)

    -- ** DeviceType
    , DeviceType (..)

    -- ** DiskImageFormat
    , DiskImageFormat (..)

    -- ** DomainType
    , DomainType (..)

    -- ** EventCode
    , EventCode (..)

    -- ** EventType
    , EventType (..)

    -- ** ExportEnvironment
    , ExportEnvironment (..)

    -- ** ExportTaskState
    , ExportTaskState (..)

    -- ** FlowLogsResourceType
    , FlowLogsResourceType (..)

    -- ** GatewayType
    , GatewayType (..)

    -- ** HypervisorType
    , HypervisorType (..)

    -- ** ImageAttributeName
    , ImageAttributeName (..)

    -- ** ImageState
    , ImageState (..)

    -- ** ImageTypeValues
    , ImageTypeValues (..)

    -- ** InstanceAttributeName
    , InstanceAttributeName (..)

    -- ** InstanceLifecycleType
    , InstanceLifecycleType (..)

    -- ** InstanceStateName
    , InstanceStateName (..)

    -- ** InstanceType
    , InstanceType (..)

    -- ** ListingState
    , ListingState (..)

    -- ** ListingStatus
    , ListingStatus (..)

    -- ** MonitoringState
    , MonitoringState (..)

    -- ** MoveStatus
    , MoveStatus (..)

    -- ** NetworkInterfaceAttribute
    , NetworkInterfaceAttribute (..)

    -- ** NetworkInterfaceStatus
    , NetworkInterfaceStatus (..)

    -- ** OfferingTypeValues
    , OfferingTypeValues (..)

    -- ** OperationType
    , OperationType (..)

    -- ** PermissionGroup
    , PermissionGroup (..)

    -- ** PlacementGroupState
    , PlacementGroupState (..)

    -- ** PlacementStrategy
    , PlacementStrategy (..)

    -- ** PlatformValues
    , PlatformValues (..)

    -- ** ProductCodeValues
    , ProductCodeValues (..)

    -- ** RIProductDescription
    , RIProductDescription (..)

    -- ** RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- ** ReportInstanceReasonCodes
    , ReportInstanceReasonCodes (..)

    -- ** ReportStatusType
    , ReportStatusType (..)

    -- ** ReservedInstanceState
    , ReservedInstanceState (..)

    -- ** ResetImageAttributeName
    , ResetImageAttributeName (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** RouteOrigin
    , RouteOrigin (..)

    -- ** RouteState
    , RouteState (..)

    -- ** RuleAction
    , RuleAction (..)

    -- ** ShutdownBehavior
    , ShutdownBehavior (..)

    -- ** SnapshotAttributeName
    , SnapshotAttributeName (..)

    -- ** SnapshotState
    , SnapshotState (..)

    -- ** SpotInstanceState
    , SpotInstanceState (..)

    -- ** SpotInstanceType
    , SpotInstanceType (..)

    -- ** State
    , State (..)

    -- ** StatusName
    , StatusName (..)

    -- ** StatusType
    , StatusType (..)

    -- ** SubnetState
    , SubnetState (..)

    -- ** SummaryStatus
    , SummaryStatus (..)

    -- ** TelemetryStatus
    , TelemetryStatus (..)

    -- ** Tenancy
    , Tenancy (..)

    -- ** TrafficType
    , TrafficType (..)

    -- ** VPCAttributeName
    , VPCAttributeName (..)

    -- ** VPCPeeringConnectionStateReasonCode
    , VPCPeeringConnectionStateReasonCode (..)

    -- ** VPCState
    , VPCState (..)

    -- ** VPNState
    , VPNState (..)

    -- ** VPNStaticRouteSource
    , VPNStaticRouteSource (..)

    -- ** VirtualizationType
    , VirtualizationType (..)

    -- ** VolumeAttachmentState
    , VolumeAttachmentState (..)

    -- ** VolumeAttributeName
    , VolumeAttributeName (..)

    -- ** VolumeState
    , VolumeState (..)

    -- ** VolumeStatusInfoStatus
    , VolumeStatusInfoStatus (..)

    -- ** VolumeStatusName
    , VolumeStatusName (..)

    -- ** VolumeType
    , VolumeType (..)

    -- ** AccountAttribute
    , AccountAttribute
    , accountAttribute
    , aaAttributeValues
    , aaAttributeName

    -- ** AccountAttributeValue
    , AccountAttributeValue
    , accountAttributeValue
    , aavAttributeValue

    -- ** ActiveInstance
    , ActiveInstance
    , activeInstance
    , aiInstanceId
    , aiInstanceType
    , aiSpotInstanceRequestId

    -- ** Address
    , Address
    , address
    , aAssociationId
    , aInstanceId
    , aNetworkInterfaceOwnerId
    , aAllocationId
    , aDomain
    , aNetworkInterfaceId
    , aPrivateIPAddress
    , aPublicIP

    -- ** AttributeBooleanValue
    , AttributeBooleanValue
    , attributeBooleanValue
    , abvValue

    -- ** AttributeValue
    , AttributeValue
    , attributeValue
    , avValue

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azState
    , azRegionName
    , azZoneName
    , azMessages

    -- ** AvailabilityZoneMessage
    , AvailabilityZoneMessage
    , availabilityZoneMessage
    , azmMessage

    -- ** BlobAttributeValue
    , BlobAttributeValue
    , blobAttributeValue
    , bavValue

    -- ** BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- ** BundleTask
    , BundleTask
    , bundleTask
    , btBundleTaskError
    , btBundleId
    , btInstanceId
    , btProgress
    , btStartTime
    , btState
    , btStorage
    , btUpdateTime

    -- ** BundleTaskError
    , BundleTaskError
    , bundleTaskError
    , bteCode
    , bteMessage

    -- ** CancelSpotFleetRequestsError
    , CancelSpotFleetRequestsError
    , cancelSpotFleetRequestsError
    , csfreCode
    , csfreMessage

    -- ** CancelSpotFleetRequestsErrorItem
    , CancelSpotFleetRequestsErrorItem
    , cancelSpotFleetRequestsErrorItem
    , csfreiSpotFleetRequestId
    , csfreiError

    -- ** CancelSpotFleetRequestsSuccessItem
    , CancelSpotFleetRequestsSuccessItem
    , cancelSpotFleetRequestsSuccessItem
    , csfrsiSpotFleetRequestId
    , csfrsiCurrentSpotFleetRequestState
    , csfrsiPreviousSpotFleetRequestState

    -- ** CancelledSpotInstanceRequest
    , CancelledSpotInstanceRequest
    , cancelledSpotInstanceRequest
    , csirState
    , csirSpotInstanceRequestId

    -- ** ClassicLinkInstance
    , ClassicLinkInstance
    , classicLinkInstance
    , cliInstanceId
    , cliGroups
    , cliVPCId
    , cliTags

    -- ** ClientData
    , ClientData
    , clientData
    , cdUploadStart
    , cdUploadSize
    , cdUploadEnd
    , cdComment

    -- ** ConversionTask
    , ConversionTask
    , conversionTask
    , ctImportInstance
    , ctStatusMessage
    , ctImportVolume
    , ctExpirationTime
    , ctTags
    , ctConversionTaskId
    , ctState

    -- ** CreateVolumePermission
    , CreateVolumePermission
    , createVolumePermission
    , cvpGroup
    , cvpUserId

    -- ** CreateVolumePermissionModifications
    , CreateVolumePermissionModifications
    , createVolumePermissionModifications
    , cvpmRemove
    , cvpmAdd

    -- ** CustomerGateway
    , CustomerGateway
    , customerGateway
    , cgTags
    , cgBGPASN
    , cgCustomerGatewayId
    , cgIPAddress
    , cgState
    , cgType

    -- ** DHCPConfiguration
    , DHCPConfiguration
    , dhcpConfiguration
    , dcValues
    , dcKey

    -- ** DHCPOptions
    , DHCPOptions
    , dhcpOptions
    , doDHCPConfigurations
    , doDHCPOptionsId
    , doTags

    -- ** DiskImage
    , DiskImage
    , diskImage
    , diImage
    , diVolume
    , diDescription

    -- ** DiskImageDescription
    , DiskImageDescription
    , diskImageDescription
    , dChecksum
    , dFormat
    , dSize
    , dImportManifestURL

    -- ** DiskImageDetail
    , DiskImageDetail
    , diskImageDetail
    , didFormat
    , didBytes
    , didImportManifestURL

    -- ** DiskImageVolumeDescription
    , DiskImageVolumeDescription
    , diskImageVolumeDescription
    , divdSize
    , divdId

    -- ** EBSBlockDevice
    , EBSBlockDevice
    , ebsBlockDevice
    , ebdDeleteOnTermination
    , ebdVolumeSize
    , ebdIOPS
    , ebdEncrypted
    , ebdVolumeType
    , ebdSnapshotId

    -- ** EBSInstanceBlockDevice
    , EBSInstanceBlockDevice
    , ebsInstanceBlockDevice
    , eibdStatus
    , eibdDeleteOnTermination
    , eibdVolumeId
    , eibdAttachTime

    -- ** EBSInstanceBlockDeviceSpecification
    , EBSInstanceBlockDeviceSpecification
    , ebsInstanceBlockDeviceSpecification
    , eibdsDeleteOnTermination
    , eibdsVolumeId

    -- ** EventInformation
    , EventInformation
    , eventInformation
    , eiInstanceId
    , eiEventDescription
    , eiEventSubType

    -- ** ExportTask
    , ExportTask
    , exportTask
    , etDescription
    , etExportTaskId
    , etExportToS3Task
    , etInstanceExportDetails
    , etState
    , etStatusMessage

    -- ** ExportToS3Task
    , ExportToS3Task
    , exportToS3Task
    , etstS3Key
    , etstContainerFormat
    , etstS3Bucket
    , etstDiskImageFormat

    -- ** ExportToS3TaskSpecification
    , ExportToS3TaskSpecification
    , exportToS3TaskSpecification
    , etstsContainerFormat
    , etstsS3Prefix
    , etstsS3Bucket
    , etstsDiskImageFormat

    -- ** Filter
    , Filter
    , filter'
    , fValues
    , fName

    -- ** FlowLog
    , FlowLog
    , flowLog
    , flCreationTime
    , flResourceId
    , flFlowLogStatus
    , flTrafficType
    , flDeliverLogsStatus
    , flDeliverLogsErrorMessage
    , flLogGroupName
    , flDeliverLogsPermissionARN
    , flFlowLogId

    -- ** GroupIdentifier
    , GroupIdentifier
    , groupIdentifier
    , giGroupId
    , giGroupName

    -- ** HistoryRecord
    , HistoryRecord
    , historyRecord
    , hrTimestamp
    , hrEventType
    , hrEventInformation

    -- ** IAMInstanceProfile
    , IAMInstanceProfile
    , iamInstanceProfile
    , iapARN
    , iapId

    -- ** IAMInstanceProfileSpecification
    , IAMInstanceProfileSpecification
    , iamInstanceProfileSpecification
    , iapsARN
    , iapsName

    -- ** ICMPTypeCode
    , ICMPTypeCode
    , icmpTypeCode
    , itcCode
    , itcType

    -- ** IPPermission
    , IPPermission
    , ipPermission
    , ipFromPort
    , ipUserIdGroupPairs
    , ipPrefixListIds
    , ipToPort
    , ipIPRanges
    , ipIPProtocol

    -- ** IPRange
    , IPRange
    , ipRange
    , irCIdRIP

    -- ** Image
    , Image
    , image
    , iPlatform
    , iImageOwnerAlias
    , iRAMDiskId
    , iKernelId
    , iRootDeviceName
    , iSRIOVNetSupport
    , iName
    , iCreationDate
    , iProductCodes
    , iStateReason
    , iDescription
    , iBlockDeviceMappings
    , iTags
    , iImageId
    , iImageLocation
    , iState
    , iOwnerId
    , iPublic
    , iArchitecture
    , iImageType
    , iRootDeviceType
    , iVirtualizationType
    , iHypervisor

    -- ** ImageDiskContainer
    , ImageDiskContainer
    , imageDiskContainer
    , idcFormat
    , idcURL
    , idcDeviceName
    , idcUserBucket
    , idcDescription
    , idcSnapshotId

    -- ** ImportImageTask
    , ImportImageTask
    , importImageTask
    , iitStatus
    , iitHypervisor
    , iitPlatform
    , iitProgress
    , iitLicenseType
    , iitSnapshotDetails
    , iitStatusMessage
    , iitImageId
    , iitImportTaskId
    , iitArchitecture
    , iitDescription

    -- ** ImportInstanceLaunchSpecification
    , ImportInstanceLaunchSpecification
    , importInstanceLaunchSpecification
    , iilsAdditionalInfo
    , iilsGroupNames
    , iilsSubnetId
    , iilsInstanceType
    , iilsGroupIds
    , iilsUserData
    , iilsMonitoring
    , iilsPrivateIPAddress
    , iilsInstanceInitiatedShutdownBehavior
    , iilsArchitecture
    , iilsPlacement

    -- ** ImportInstanceTaskDetails
    , ImportInstanceTaskDetails
    , importInstanceTaskDetails
    , iitdInstanceId
    , iitdPlatform
    , iitdDescription
    , iitdVolumes

    -- ** ImportInstanceVolumeDetailItem
    , ImportInstanceVolumeDetailItem
    , importInstanceVolumeDetailItem
    , iivdiStatusMessage
    , iivdiDescription
    , iivdiBytesConverted
    , iivdiAvailabilityZone
    , iivdiImage
    , iivdiVolume
    , iivdiStatus

    -- ** ImportSnapshotTask
    , ImportSnapshotTask
    , importSnapshotTask
    , istSnapshotTaskDetail
    , istImportTaskId
    , istDescription

    -- ** ImportVolumeTaskDetails
    , ImportVolumeTaskDetails
    , importVolumeTaskDetails
    , ivtdDescription
    , ivtdBytesConverted
    , ivtdAvailabilityZone
    , ivtdImage
    , ivtdVolume

    -- ** Instance
    , Instance
    , instance'
    , insPublicDNSName
    , insPlatform
    , insSecurityGroups
    , insClientToken
    , insSourceDestCheck
    , insVPCId
    , insKeyName
    , insNetworkInterfaces
    , insRAMDiskId
    , insSubnetId
    , insKernelId
    , insRootDeviceName
    , insSRIOVNetSupport
    , insEBSOptimized
    , insStateTransitionReason
    , insInstanceLifecycle
    , insIAMInstanceProfile
    , insPrivateIPAddress
    , insProductCodes
    , insSpotInstanceRequestId
    , insPrivateDNSName
    , insStateReason
    , insBlockDeviceMappings
    , insPublicIPAddress
    , insTags
    , insInstanceId
    , insImageId
    , insAMILaunchIndex
    , insInstanceType
    , insLaunchTime
    , insPlacement
    , insMonitoring
    , insArchitecture
    , insRootDeviceType
    , insVirtualizationType
    , insHypervisor
    , insState

    -- ** InstanceBlockDeviceMapping
    , InstanceBlockDeviceMapping
    , instanceBlockDeviceMapping
    , ibdmEBS
    , ibdmDeviceName

    -- ** InstanceBlockDeviceMappingSpecification
    , InstanceBlockDeviceMappingSpecification
    , instanceBlockDeviceMappingSpecification
    , ibdmsVirtualName
    , ibdmsNoDevice
    , ibdmsEBS
    , ibdmsDeviceName

    -- ** InstanceCount
    , InstanceCount
    , instanceCount
    , icState
    , icInstanceCount

    -- ** InstanceExportDetails
    , InstanceExportDetails
    , instanceExportDetails
    , iedTargetEnvironment
    , iedInstanceId

    -- ** InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imInstanceId
    , imMonitoring

    -- ** InstanceNetworkInterface
    , InstanceNetworkInterface
    , instanceNetworkInterface
    , iniGroups
    , iniStatus
    , iniPrivateIPAddresses
    , iniSourceDestCheck
    , iniVPCId
    , iniNetworkInterfaceId
    , iniSubnetId
    , iniMACAddress
    , iniAttachment
    , iniOwnerId
    , iniPrivateIPAddress
    , iniPrivateDNSName
    , iniDescription
    , iniAssociation

    -- ** InstanceNetworkInterfaceAssociation
    , InstanceNetworkInterfaceAssociation
    , instanceNetworkInterfaceAssociation
    , iniaPublicDNSName
    , iniaIPOwnerId
    , iniaPublicIP

    -- ** InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment
    , instanceNetworkInterfaceAttachment
    , iniaStatus
    , iniaDeleteOnTermination
    , iniaAttachmentId
    , iniaAttachTime
    , iniaDeviceIndex

    -- ** InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification
    , instanceNetworkInterfaceSpecification
    , inisGroups
    , inisPrivateIPAddresses
    , inisDeleteOnTermination
    , inisAssociatePublicIPAddress
    , inisNetworkInterfaceId
    , inisSubnetId
    , inisPrivateIPAddress
    , inisSecondaryPrivateIPAddressCount
    , inisDescription
    , inisDeviceIndex

    -- ** InstancePrivateIPAddress
    , InstancePrivateIPAddress
    , instancePrivateIPAddress
    , ipiaPrimary
    , ipiaPrivateIPAddress
    , ipiaPrivateDNSName
    , ipiaAssociation

    -- ** InstanceState
    , InstanceState
    , instanceState
    , isName
    , isCode

    -- ** InstanceStateChange
    , InstanceStateChange
    , instanceStateChange
    , iscInstanceId
    , iscCurrentState
    , iscPreviousState

    -- ** InstanceStatus
    , InstanceStatus
    , instanceStatus
    , isInstanceId
    , isSystemStatus
    , isEvents
    , isAvailabilityZone
    , isInstanceStatus
    , isInstanceState

    -- ** InstanceStatusDetails
    , InstanceStatusDetails
    , instanceStatusDetails
    , isdStatus
    , isdImpairedSince
    , isdName

    -- ** InstanceStatusEvent
    , InstanceStatusEvent
    , instanceStatusEvent
    , iseNotBefore
    , iseCode
    , iseDescription
    , iseNotAfter

    -- ** InstanceStatusSummary
    , InstanceStatusSummary
    , instanceStatusSummary
    , issDetails
    , issStatus

    -- ** InternetGateway
    , InternetGateway
    , internetGateway
    , igAttachments
    , igTags
    , igInternetGatewayId

    -- ** InternetGatewayAttachment
    , InternetGatewayAttachment
    , internetGatewayAttachment
    , igaState
    , igaVPCId

    -- ** KeyPairInfo
    , KeyPairInfo
    , keyPairInfo
    , kpiKeyFingerprint
    , kpiKeyName

    -- ** LaunchPermission
    , LaunchPermission
    , launchPermission
    , lpGroup
    , lpUserId

    -- ** LaunchPermissionModifications
    , LaunchPermissionModifications
    , launchPermissionModifications
    , lpmRemove
    , lpmAdd

    -- ** LaunchSpecification
    , LaunchSpecification
    , launchSpecification
    , lsSecurityGroups
    , lsKeyName
    , lsNetworkInterfaces
    , lsRAMDiskId
    , lsSubnetId
    , lsKernelId
    , lsInstanceType
    , lsEBSOptimized
    , lsUserData
    , lsMonitoring
    , lsIAMInstanceProfile
    , lsImageId
    , lsAddressingType
    , lsBlockDeviceMappings
    , lsPlacement

    -- ** Monitoring
    , Monitoring
    , monitoring
    , mState

    -- ** MovingAddressStatus
    , MovingAddressStatus
    , movingAddressStatus
    , masMoveStatus
    , masPublicIP

    -- ** NetworkACL
    , NetworkACL
    , networkACL
    , naEntries
    , naNetworkACLId
    , naVPCId
    , naAssociations
    , naTags
    , naIsDefault

    -- ** NetworkACLAssociation
    , NetworkACLAssociation
    , networkACLAssociation
    , naaNetworkACLId
    , naaSubnetId
    , naaNetworkACLAssociationId

    -- ** NetworkACLEntry
    , NetworkACLEntry
    , networkACLEntry
    , naeICMPTypeCode
    , naeRuleNumber
    , naeRuleAction
    , naeProtocol
    , naePortRange
    , naeCIdRBlock
    , naeEgress

    -- ** NetworkInterface
    , NetworkInterface
    , networkInterface
    , niGroups
    , niStatus
    , niPrivateIPAddresses
    , niSourceDestCheck
    , niVPCId
    , niTagSet
    , niRequesterManaged
    , niNetworkInterfaceId
    , niSubnetId
    , niMACAddress
    , niAttachment
    , niOwnerId
    , niAvailabilityZone
    , niPrivateIPAddress
    , niPrivateDNSName
    , niRequesterId
    , niDescription
    , niAssociation

    -- ** NetworkInterfaceAssociation
    , NetworkInterfaceAssociation
    , networkInterfaceAssociation
    , niaAssociationId
    , niaPublicDNSName
    , niaAllocationId
    , niaIPOwnerId
    , niaPublicIP

    -- ** NetworkInterfaceAttachment
    , NetworkInterfaceAttachment
    , networkInterfaceAttachment
    , niaInstanceId
    , niaStatus
    , niaDeleteOnTermination
    , niaAttachmentId
    , niaInstanceOwnerId
    , niaAttachTime
    , niaDeviceIndex

    -- ** NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges
    , networkInterfaceAttachmentChanges
    , niacDeleteOnTermination
    , niacAttachmentId

    -- ** NetworkInterfacePrivateIPAddress
    , NetworkInterfacePrivateIPAddress
    , networkInterfacePrivateIPAddress
    , nipiaPrimary
    , nipiaPrivateIPAddress
    , nipiaPrivateDNSName
    , nipiaAssociation

    -- ** NewDHCPConfiguration
    , NewDHCPConfiguration
    , newDHCPConfiguration
    , ndcValues
    , ndcKey

    -- ** Placement
    , Placement
    , placement
    , pAvailabilityZone
    , pTenancy
    , pGroupName

    -- ** PlacementGroup
    , PlacementGroup
    , placementGroup
    , pgState
    , pgStrategy
    , pgGroupName

    -- ** PortRange
    , PortRange
    , portRange
    , prTo
    , prFrom

    -- ** PrefixList
    , PrefixList
    , prefixList
    , plCIdRs
    , plPrefixListId
    , plPrefixListName

    -- ** PrefixListId
    , PrefixListId
    , prefixListId
    , pliPrefixListId

    -- ** PriceSchedule
    , PriceSchedule
    , priceSchedule
    , psCurrencyCode
    , psTerm
    , psActive
    , psPrice

    -- ** PriceScheduleSpecification
    , PriceScheduleSpecification
    , priceScheduleSpecification
    , pssCurrencyCode
    , pssTerm
    , pssPrice

    -- ** PricingDetail
    , PricingDetail
    , pricingDetail
    , pdCount
    , pdPrice

    -- ** PrivateIPAddressSpecification
    , PrivateIPAddressSpecification
    , privateIPAddressSpecification
    , piasPrimary
    , piasPrivateIPAddress

    -- ** ProductCode
    , ProductCode
    , productCode
    , pcProductCodeType
    , pcProductCodeId

    -- ** PropagatingVGW
    , PropagatingVGW
    , propagatingVGW
    , pvGatewayId

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcAmount
    , rcFrequency

    -- ** RegionInfo
    , RegionInfo
    , regionInfo
    , riRegionName
    , riEndpoint

    -- ** RequestSpotLaunchSpecification
    , RequestSpotLaunchSpecification
    , requestSpotLaunchSpecification
    , rslsSecurityGroupIds
    , rslsSecurityGroups
    , rslsKeyName
    , rslsNetworkInterfaces
    , rslsRAMDiskId
    , rslsSubnetId
    , rslsKernelId
    , rslsInstanceType
    , rslsEBSOptimized
    , rslsUserData
    , rslsMonitoring
    , rslsIAMInstanceProfile
    , rslsImageId
    , rslsAddressingType
    , rslsBlockDeviceMappings
    , rslsPlacement

    -- ** Reservation
    , Reservation
    , reservation
    , rGroups
    , rInstances
    , rRequesterId
    , rReservationId
    , rOwnerId

    -- ** ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice
    , reservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- ** ReservedInstances
    , ReservedInstances
    , reservedInstances
    , riState
    , riCurrencyCode
    , riInstanceCount
    , riProductDescription
    , riStart
    , riInstanceType
    , riEnd
    , riAvailabilityZone
    , riRecurringCharges
    , riOfferingType
    , riUsagePrice
    , riFixedPrice
    , riReservedInstancesId
    , riInstanceTenancy
    , riDuration
    , riTags

    -- ** ReservedInstancesConfiguration
    , ReservedInstancesConfiguration
    , reservedInstancesConfiguration
    , ricPlatform
    , ricInstanceCount
    , ricInstanceType
    , ricAvailabilityZone

    -- ** ReservedInstancesId
    , ReservedInstancesId
    , reservedInstancesId
    , riiReservedInstancesId

    -- ** ReservedInstancesListing
    , ReservedInstancesListing
    , reservedInstancesListing
    , rilStatus
    , rilClientToken
    , rilUpdateDate
    , rilCreateDate
    , rilPriceSchedules
    , rilStatusMessage
    , rilReservedInstancesId
    , rilTags
    , rilInstanceCounts
    , rilReservedInstancesListingId

    -- ** ReservedInstancesModification
    , ReservedInstancesModification
    , reservedInstancesModification
    , rimModificationResults
    , rimStatus
    , rimClientToken
    , rimUpdateDate
    , rimCreateDate
    , rimEffectiveDate
    , rimStatusMessage
    , rimReservedInstancesModificationId
    , rimReservedInstancesIds

    -- ** ReservedInstancesModificationResult
    , ReservedInstancesModificationResult
    , reservedInstancesModificationResult
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- ** ReservedInstancesOffering
    , ReservedInstancesOffering
    , reservedInstancesOffering
    , rioMarketplace
    , rioCurrencyCode
    , rioProductDescription
    , rioInstanceType
    , rioAvailabilityZone
    , rioPricingDetails
    , rioRecurringCharges
    , rioOfferingType
    , rioUsagePrice
    , rioFixedPrice
    , rioInstanceTenancy
    , rioReservedInstancesOfferingId
    , rioDuration

    -- ** Route
    , Route
    , route
    , rVPCPeeringConnectionId
    , rInstanceId
    , rOrigin
    , rState
    , rNetworkInterfaceId
    , rGatewayId
    , rInstanceOwnerId
    , rDestinationPrefixListId
    , rDestinationCIdRBlock

    -- ** RouteTable
    , RouteTable
    , routeTable
    , rtRouteTableId
    , rtRoutes
    , rtVPCId
    , rtPropagatingVGWs
    , rtAssociations
    , rtTags

    -- ** RouteTableAssociation
    , RouteTableAssociation
    , routeTableAssociation
    , rtaRouteTableId
    , rtaRouteTableAssociationId
    , rtaMain
    , rtaSubnetId

    -- ** RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled
    , runInstancesMonitoringEnabled
    , rimeEnabled

    -- ** S3Storage
    , S3Storage
    , s3Storage
    , ssPrefix
    , ssUploadPolicy
    , ssBucket
    , ssUploadPolicySignature
    , ssAWSAccessKeyId

    -- ** SecurityGroup
    , SecurityGroup
    , securityGroup
    , sgVPCId
    , sgIPPermissions
    , sgIPPermissionsEgress
    , sgTags
    , sgOwnerId
    , sgGroupId
    , sgGroupName
    , sgDescription

    -- ** Snapshot
    , Snapshot
    , snapshot
    , sOwnerAlias
    , sKMSKeyId
    , sTags
    , sSnapshotId
    , sOwnerId
    , sVolumeId
    , sVolumeSize
    , sDescription
    , sStartTime
    , sProgress
    , sState
    , sEncrypted

    -- ** SnapshotDetail
    , SnapshotDetail
    , snapshotDetail
    , sdStatus
    , sdProgress
    , sdFormat
    , sdURL
    , sdDeviceName
    , sdStatusMessage
    , sdUserBucket
    , sdDiskImageSize
    , sdDescription
    , sdSnapshotId

    -- ** SnapshotDiskContainer
    , SnapshotDiskContainer
    , snapshotDiskContainer
    , sdcFormat
    , sdcURL
    , sdcUserBucket
    , sdcDescription

    -- ** SnapshotTaskDetail
    , SnapshotTaskDetail
    , snapshotTaskDetail
    , stdStatus
    , stdProgress
    , stdFormat
    , stdURL
    , stdStatusMessage
    , stdUserBucket
    , stdDiskImageSize
    , stdDescription
    , stdSnapshotId

    -- ** SpotDatafeedSubscription
    , SpotDatafeedSubscription
    , spotDatafeedSubscription
    , sdsState
    , sdsPrefix
    , sdsBucket
    , sdsOwnerId
    , sdsFault

    -- ** SpotFleetLaunchSpecification
    , SpotFleetLaunchSpecification
    , spotFleetLaunchSpecification
    , sflsSecurityGroups
    , sflsSpotPrice
    , sflsWeightedCapacity
    , sflsKeyName
    , sflsNetworkInterfaces
    , sflsRAMDiskId
    , sflsSubnetId
    , sflsKernelId
    , sflsInstanceType
    , sflsEBSOptimized
    , sflsUserData
    , sflsMonitoring
    , sflsIAMInstanceProfile
    , sflsImageId
    , sflsAddressingType
    , sflsBlockDeviceMappings
    , sflsPlacement

    -- ** SpotFleetMonitoring
    , SpotFleetMonitoring
    , spotFleetMonitoring
    , sfmEnabled

    -- ** SpotFleetRequestConfig
    , SpotFleetRequestConfig
    , spotFleetRequestConfig
    , sfrcSpotFleetRequestId
    , sfrcSpotFleetRequestState
    , sfrcSpotFleetRequestConfig

    -- ** SpotFleetRequestConfigData
    , SpotFleetRequestConfigData
    , spotFleetRequestConfigData
    , sfrcdClientToken
    , sfrcdValidUntil
    , sfrcdTerminateInstancesWithExpiration
    , sfrcdValidFrom
    , sfrcdSpotPrice
    , sfrcdTargetCapacity
    , sfrcdIAMFleetRole
    , sfrcdLaunchSpecifications

    -- ** SpotInstanceRequest
    , SpotInstanceRequest
    , spotInstanceRequest
    , sirInstanceId
    , sirStatus
    , sirState
    , sirProductDescription
    , sirSpotPrice
    , sirLaunchSpecification
    , sirAvailabilityZoneGroup
    , sirLaunchedAvailabilityZone
    , sirValidUntil
    , sirLaunchGroup
    , sirFault
    , sirSpotInstanceRequestId
    , sirType
    , sirValidFrom
    , sirCreateTime
    , sirTags

    -- ** SpotInstanceStateFault
    , SpotInstanceStateFault
    , spotInstanceStateFault
    , sisfCode
    , sisfMessage

    -- ** SpotInstanceStatus
    , SpotInstanceStatus
    , spotInstanceStatus
    , sisUpdateTime
    , sisCode
    , sisMessage

    -- ** SpotPlacement
    , SpotPlacement
    , spotPlacement
    , spAvailabilityZone
    , spGroupName

    -- ** SpotPrice
    , SpotPrice
    , spotPrice
    , sProductDescription
    , sSpotPrice
    , sInstanceType
    , sAvailabilityZone
    , sTimestamp

    -- ** StateReason
    , StateReason
    , stateReason
    , srCode
    , srMessage

    -- ** Storage
    , Storage
    , storage
    , sS3

    -- ** Subnet
    , Subnet
    , subnet
    , subTags
    , subAvailabilityZone
    , subAvailableIPAddressCount
    , subCIdRBlock
    , subDefaultForAz
    , subMapPublicIPOnLaunch
    , subState
    , subSubnetId
    , subVPCId

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TagDescription
    , TagDescription
    , tagDescription
    , tdResourceId
    , tdResourceType
    , tdKey
    , tdValue

    -- ** UnsuccessfulItem
    , UnsuccessfulItem
    , unsuccessfulItem
    , uiResourceId
    , uiError

    -- ** UnsuccessfulItemError
    , UnsuccessfulItemError
    , unsuccessfulItemError
    , uieCode
    , uieMessage

    -- ** UserBucket
    , UserBucket
    , userBucket
    , ubS3Key
    , ubS3Bucket

    -- ** UserBucketDetails
    , UserBucketDetails
    , userBucketDetails
    , ubdS3Key
    , ubdS3Bucket

    -- ** UserData
    , UserData
    , userData
    , udData

    -- ** UserIdGroupPair
    , UserIdGroupPair
    , userIdGroupPair
    , uigpUserId
    , uigpGroupId
    , uigpGroupName

    -- ** VGWTelemetry
    , VGWTelemetry
    , vgwTelemetry
    , vtStatus
    , vtOutsideIPAddress
    , vtLastStatusChange
    , vtAcceptedRouteCount
    , vtStatusMessage

    -- ** VPC
    , VPC
    , vpc
    , vpcTags
    , vpcCIdRBlock
    , vpcDHCPOptionsId
    , vpcInstanceTenancy
    , vpcIsDefault
    , vpcState
    , vpcVPCId

    -- ** VPCAttachment
    , VPCAttachment
    , vpcAttachment
    , vaState
    , vaVPCId

    -- ** VPCClassicLink
    , VPCClassicLink
    , vpcClassicLink
    , vclVPCId
    , vclTags
    , vclClassicLinkEnabled

    -- ** VPCEndpoint
    , VPCEndpoint
    , vpcEndpoint
    , veState
    , vePolicyDocument
    , veVPCId
    , veCreationTimestamp
    , veServiceName
    , veVPCEndpointId
    , veRouteTableIds

    -- ** VPCPeeringConnection
    , VPCPeeringConnection
    , vpcPeeringConnection
    , vpcpcVPCPeeringConnectionId
    , vpcpcStatus
    , vpcpcAccepterVPCInfo
    , vpcpcRequesterVPCInfo
    , vpcpcExpirationTime
    , vpcpcTags

    -- ** VPCPeeringConnectionStateReason
    , VPCPeeringConnectionStateReason
    , vpcPeeringConnectionStateReason
    , vpcsrCode
    , vpcsrMessage

    -- ** VPCPeeringConnectionVPCInfo
    , VPCPeeringConnectionVPCInfo
    , vpcPeeringConnectionVPCInfo
    , vpcviVPCId
    , vpcviOwnerId
    , vpcviCIdRBlock

    -- ** VPNConnection
    , VPNConnection
    , vpnConnection
    , vcCustomerGatewayConfiguration
    , vcRoutes
    , vcVPNGatewayId
    , vcOptions
    , vcTags
    , vcVGWTelemetry
    , vcVPNConnectionId
    , vcCustomerGatewayId
    , vcState
    , vcType

    -- ** VPNConnectionOptions
    , VPNConnectionOptions
    , vpnConnectionOptions
    , vcoStaticRoutesOnly

    -- ** VPNConnectionOptionsSpecification
    , VPNConnectionOptionsSpecification
    , vpnConnectionOptionsSpecification
    , vcosStaticRoutesOnly

    -- ** VPNGateway
    , VPNGateway
    , vpnGateway
    , vgState
    , vgVPCAttachments
    , vgVPNGatewayId
    , vgAvailabilityZone
    , vgType
    , vgTags

    -- ** VPNStaticRoute
    , VPNStaticRoute
    , vpnStaticRoute
    , vsrState
    , vsrSource
    , vsrDestinationCIdRBlock

    -- ** Volume
    , Volume
    , volume
    , vAttachments
    , vIOPS
    , vKMSKeyId
    , vTags
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vSize
    , vSnapshotId
    , vState
    , vVolumeId
    , vVolumeType

    -- ** VolumeAttachment
    , VolumeAttachment
    , volumeAttachment
    , volInstanceId
    , volDeleteOnTermination
    , volState
    , volDevice
    , volVolumeId
    , volAttachTime

    -- ** VolumeDetail
    , VolumeDetail
    , volumeDetail
    , vdSize

    -- ** VolumeStatusAction
    , VolumeStatusAction
    , volumeStatusAction
    , vsaEventType
    , vsaCode
    , vsaDescription
    , vsaEventId

    -- ** VolumeStatusDetails
    , VolumeStatusDetails
    , volumeStatusDetails
    , vsdStatus
    , vsdName

    -- ** VolumeStatusEvent
    , VolumeStatusEvent
    , volumeStatusEvent
    , vseNotBefore
    , vseEventType
    , vseDescription
    , vseNotAfter
    , vseEventId

    -- ** VolumeStatusInfo
    , VolumeStatusInfo
    , volumeStatusInfo
    , vsiStatus
    , vsiDetails

    -- ** VolumeStatusItem
    , VolumeStatusItem
    , volumeStatusItem
    , vsiVolumeStatus
    , vsiActions
    , vsiEvents
    , vsiAvailabilityZone
    , vsiVolumeId
    ) where

import           Network.AWS.EC2.AcceptVPCPeeringConnection
import           Network.AWS.EC2.AllocateAddress
import           Network.AWS.EC2.AssignPrivateIPAddresses
import           Network.AWS.EC2.AssociateAddress
import           Network.AWS.EC2.AssociateDHCPOptions
import           Network.AWS.EC2.AssociateRouteTable
import           Network.AWS.EC2.AttachClassicLinkVPC
import           Network.AWS.EC2.AttachInternetGateway
import           Network.AWS.EC2.AttachNetworkInterface
import           Network.AWS.EC2.AttachVolume
import           Network.AWS.EC2.AttachVPNGateway
import           Network.AWS.EC2.AuthorizeSecurityGroupEgress
import           Network.AWS.EC2.AuthorizeSecurityGroupIngress
import           Network.AWS.EC2.BundleInstance
import           Network.AWS.EC2.CancelBundleTask
import           Network.AWS.EC2.CancelConversionTask
import           Network.AWS.EC2.CancelExportTask
import           Network.AWS.EC2.CancelImportTask
import           Network.AWS.EC2.CancelReservedInstancesListing
import           Network.AWS.EC2.CancelSpotFleetRequests
import           Network.AWS.EC2.CancelSpotInstanceRequests
import           Network.AWS.EC2.ConfirmProductInstance
import           Network.AWS.EC2.CopyImage
import           Network.AWS.EC2.CopySnapshot
import           Network.AWS.EC2.CreateCustomerGateway
import           Network.AWS.EC2.CreateDHCPOptions
import           Network.AWS.EC2.CreateFlowLogs
import           Network.AWS.EC2.CreateImage
import           Network.AWS.EC2.CreateInstanceExportTask
import           Network.AWS.EC2.CreateInternetGateway
import           Network.AWS.EC2.CreateKeyPair
import           Network.AWS.EC2.CreateNetworkACL
import           Network.AWS.EC2.CreateNetworkACLEntry
import           Network.AWS.EC2.CreateNetworkInterface
import           Network.AWS.EC2.CreatePlacementGroup
import           Network.AWS.EC2.CreateReservedInstancesListing
import           Network.AWS.EC2.CreateRoute
import           Network.AWS.EC2.CreateRouteTable
import           Network.AWS.EC2.CreateSecurityGroup
import           Network.AWS.EC2.CreateSnapshot
import           Network.AWS.EC2.CreateSpotDatafeedSubscription
import           Network.AWS.EC2.CreateSubnet
import           Network.AWS.EC2.CreateTags
import           Network.AWS.EC2.CreateVolume
import           Network.AWS.EC2.CreateVPC
import           Network.AWS.EC2.CreateVPCEndpoint
import           Network.AWS.EC2.CreateVPCPeeringConnection
import           Network.AWS.EC2.CreateVPNConnection
import           Network.AWS.EC2.CreateVPNConnectionRoute
import           Network.AWS.EC2.CreateVPNGateway
import           Network.AWS.EC2.DeleteCustomerGateway
import           Network.AWS.EC2.DeleteDHCPOptions
import           Network.AWS.EC2.DeleteFlowLogs
import           Network.AWS.EC2.DeleteInternetGateway
import           Network.AWS.EC2.DeleteKeyPair
import           Network.AWS.EC2.DeleteNetworkACL
import           Network.AWS.EC2.DeleteNetworkACLEntry
import           Network.AWS.EC2.DeleteNetworkInterface
import           Network.AWS.EC2.DeletePlacementGroup
import           Network.AWS.EC2.DeleteRoute
import           Network.AWS.EC2.DeleteRouteTable
import           Network.AWS.EC2.DeleteSecurityGroup
import           Network.AWS.EC2.DeleteSnapshot
import           Network.AWS.EC2.DeleteSpotDatafeedSubscription
import           Network.AWS.EC2.DeleteSubnet
import           Network.AWS.EC2.DeleteTags
import           Network.AWS.EC2.DeleteVolume
import           Network.AWS.EC2.DeleteVPC
import           Network.AWS.EC2.DeleteVPCEndpoints
import           Network.AWS.EC2.DeleteVPCPeeringConnection
import           Network.AWS.EC2.DeleteVPNConnection
import           Network.AWS.EC2.DeleteVPNConnectionRoute
import           Network.AWS.EC2.DeleteVPNGateway
import           Network.AWS.EC2.DeregisterImage
import           Network.AWS.EC2.DescribeAccountAttributes
import           Network.AWS.EC2.DescribeAddresses
import           Network.AWS.EC2.DescribeAvailabilityZones
import           Network.AWS.EC2.DescribeBundleTasks
import           Network.AWS.EC2.DescribeClassicLinkInstances
import           Network.AWS.EC2.DescribeConversionTasks
import           Network.AWS.EC2.DescribeCustomerGateways
import           Network.AWS.EC2.DescribeDHCPOptions
import           Network.AWS.EC2.DescribeExportTasks
import           Network.AWS.EC2.DescribeFlowLogs
import           Network.AWS.EC2.DescribeImageAttribute
import           Network.AWS.EC2.DescribeImages
import           Network.AWS.EC2.DescribeImportImageTasks
import           Network.AWS.EC2.DescribeImportSnapshotTasks
import           Network.AWS.EC2.DescribeInstanceAttribute
import           Network.AWS.EC2.DescribeInstances
import           Network.AWS.EC2.DescribeInstanceStatus
import           Network.AWS.EC2.DescribeInternetGateways
import           Network.AWS.EC2.DescribeKeyPairs
import           Network.AWS.EC2.DescribeMovingAddresses
import           Network.AWS.EC2.DescribeNetworkACLs
import           Network.AWS.EC2.DescribeNetworkInterfaceAttribute
import           Network.AWS.EC2.DescribeNetworkInterfaces
import           Network.AWS.EC2.DescribePlacementGroups
import           Network.AWS.EC2.DescribePrefixLists
import           Network.AWS.EC2.DescribeRegions
import           Network.AWS.EC2.DescribeReservedInstances
import           Network.AWS.EC2.DescribeReservedInstancesListings
import           Network.AWS.EC2.DescribeReservedInstancesModifications
import           Network.AWS.EC2.DescribeReservedInstancesOfferings
import           Network.AWS.EC2.DescribeRouteTables
import           Network.AWS.EC2.DescribeSecurityGroups
import           Network.AWS.EC2.DescribeSnapshotAttribute
import           Network.AWS.EC2.DescribeSnapshots
import           Network.AWS.EC2.DescribeSpotDatafeedSubscription
import           Network.AWS.EC2.DescribeSpotFleetInstances
import           Network.AWS.EC2.DescribeSpotFleetRequestHistory
import           Network.AWS.EC2.DescribeSpotFleetRequests
import           Network.AWS.EC2.DescribeSpotInstanceRequests
import           Network.AWS.EC2.DescribeSpotPriceHistory
import           Network.AWS.EC2.DescribeSubnets
import           Network.AWS.EC2.DescribeTags
import           Network.AWS.EC2.DescribeVolumeAttribute
import           Network.AWS.EC2.DescribeVolumes
import           Network.AWS.EC2.DescribeVolumeStatus
import           Network.AWS.EC2.DescribeVPCAttribute
import           Network.AWS.EC2.DescribeVPCClassicLink
import           Network.AWS.EC2.DescribeVPCEndpoints
import           Network.AWS.EC2.DescribeVPCEndpointServices
import           Network.AWS.EC2.DescribeVPCPeeringConnections
import           Network.AWS.EC2.DescribeVPCs
import           Network.AWS.EC2.DescribeVPNConnections
import           Network.AWS.EC2.DescribeVPNGateways
import           Network.AWS.EC2.DetachClassicLinkVPC
import           Network.AWS.EC2.DetachInternetGateway
import           Network.AWS.EC2.DetachNetworkInterface
import           Network.AWS.EC2.DetachVolume
import           Network.AWS.EC2.DetachVPNGateway
import           Network.AWS.EC2.DisableVGWRoutePropagation
import           Network.AWS.EC2.DisableVPCClassicLink
import           Network.AWS.EC2.DisassociateAddress
import           Network.AWS.EC2.DisassociateRouteTable
import           Network.AWS.EC2.EnableVGWRoutePropagation
import           Network.AWS.EC2.EnableVolumeIO
import           Network.AWS.EC2.EnableVPCClassicLink
import           Network.AWS.EC2.GetConsoleOutput
import           Network.AWS.EC2.GetPasswordData
import           Network.AWS.EC2.ImportImage
import           Network.AWS.EC2.ImportInstance
import           Network.AWS.EC2.ImportKeyPair
import           Network.AWS.EC2.ImportSnapshot
import           Network.AWS.EC2.ImportVolume
import           Network.AWS.EC2.ModifyImageAttribute
import           Network.AWS.EC2.ModifyInstanceAttribute
import           Network.AWS.EC2.ModifyNetworkInterfaceAttribute
import           Network.AWS.EC2.ModifyReservedInstances
import           Network.AWS.EC2.ModifySnapshotAttribute
import           Network.AWS.EC2.ModifySubnetAttribute
import           Network.AWS.EC2.ModifyVolumeAttribute
import           Network.AWS.EC2.ModifyVPCAttribute
import           Network.AWS.EC2.ModifyVPCEndpoint
import           Network.AWS.EC2.MonitorInstances
import           Network.AWS.EC2.MoveAddressToVPC
import           Network.AWS.EC2.PurchaseReservedInstancesOffering
import           Network.AWS.EC2.RebootInstances
import           Network.AWS.EC2.RegisterImage
import           Network.AWS.EC2.RejectVPCPeeringConnection
import           Network.AWS.EC2.ReleaseAddress
import           Network.AWS.EC2.ReplaceNetworkACLAssociation
import           Network.AWS.EC2.ReplaceNetworkACLEntry
import           Network.AWS.EC2.ReplaceRoute
import           Network.AWS.EC2.ReplaceRouteTableAssociation
import           Network.AWS.EC2.ReportInstanceStatus
import           Network.AWS.EC2.RequestSpotFleet
import           Network.AWS.EC2.RequestSpotInstances
import           Network.AWS.EC2.ResetImageAttribute
import           Network.AWS.EC2.ResetInstanceAttribute
import           Network.AWS.EC2.ResetNetworkInterfaceAttribute
import           Network.AWS.EC2.ResetSnapshotAttribute
import           Network.AWS.EC2.RestoreAddressToClassic
import           Network.AWS.EC2.RevokeSecurityGroupEgress
import           Network.AWS.EC2.RevokeSecurityGroupIngress
import           Network.AWS.EC2.RunInstances
import           Network.AWS.EC2.StartInstances
import           Network.AWS.EC2.StopInstances
import           Network.AWS.EC2.TerminateInstances
import           Network.AWS.EC2.Types
import           Network.AWS.EC2.UnassignPrivateIPAddresses
import           Network.AWS.EC2.UnmonitorInstances
import           Network.AWS.EC2.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'EC2'.
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
