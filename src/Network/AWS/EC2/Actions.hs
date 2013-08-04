{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Network.AWS.EC2.Actions
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Actions where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Network.AWS.Request
import           Network.AWS.TH
import           Network.AWS.Types
import           Network.Http.Client

signer :: AWSQuery a => Method -> ByteString -> a -> AWS SignedRequest
signer meth action qry = sign Version2 rq
  where
    rq = (emptyRequest meth version endpoint "/" Nothing)
        { rqAction = Just action
        , rqQuery  = queryString qry
        }

    version  = "2013-06-15"
    endpoint = "ec2.amazonaws.com"

data AllocateAddress = AllocateAddress
    { allocateAddressDomain :: !(Maybe ByteString)
    } deriving (Show)

$(deriveQueryString "allocateAddress" ''AllocateAddress)

instance AWSRequest AllocateAddress where
    signRequest = signer GET "AllocateAddress"

data DescribeInstances = DescribeInstances
    { describeInstancesInstanceId :: [ByteString]
    } deriving (Show)

$(deriveQueryString "describeInstances" ''DescribeInstances)

instance AWSRequest DescribeInstances where
    signRequest = signer GET "DescribeInstances"

--
-- Internal
--

data Action
    = ActivateLicense
--    | AllocateAddress
    | AssignPrivateIpAddresses
    | AssociateAddress
    | AssociateDhcpOptions
    | AssociateRouteTable
    | AttachInternetGateway
    | AttachNetworkInterface
    | AttachVolume
    | AttachVpnGateway
    | AuthorizeSecurityGroupEgress
    | AuthorizeSecurityGroupIngress
    | BundleInstance
    | CancelBundleTask
    | CancelConversionTask
    | CancelExportTask
    | CancelReservedInstancesListing
    | CancelSpotInstanceRequests
    | ConfirmProductInstance
    | CopyImage
    | CopySnapshot
    | CreateCustomerGateway
    | CreateDhcpOptions
    | CreateImage
    | CreateInstanceExportTask
    | CreateInternetGateway
    | CreateKeyPair
    | CreateNetworkAcl
    | CreateNetworkAclEntry
    | CreateNetworkInterface
    | CreatePlacementGroup
    | CreateReservedInstancesListing
    | CreateRoute
    | CreateRouteTable
    | CreateSecurityGroup
    | CreateSnapshot
    | CreateSpotDatafeedSubscription
    | CreateSubnet
    | CreateTags
    | CreateVolume
    | CreateVpc
    | CreateVpnConnection
    | CreateVpnConnectionRoute
    | CreateVpnGateway
    | DeactivateLicense
    | DeleteCustomerGateway
    | DeleteDhcpOptions
    | DeleteInternetGateway
    | DeleteKeyPair
    | DeleteNetworkAcl
    | DeleteNetworkAclEntry
    | DeleteNetworkInterface
    | DeletePlacementGroup
    | DeleteRoute
    | DeleteRouteTable
    | DeleteSecurityGroup
    | DeleteSnapshot
    | DeleteSpotDatafeedSubscription
    | DeleteSubnet
    | DeleteTags
    | DeleteVolume
    | DeleteVpc
    | DeleteVpnConnection
    | DeleteVpnConnectionRoute
    | DeleteVpnGateway
    | DeregisterImage
    | DescribeAccountAttributes
    | DescribeAddresses
    | DescribeAvailabilityZones
    | DescribeBundleTasks
    | DescribeConversionTasks
    | DescribeCustomerGateways
    | DescribeDhcpOptions
    | DescribeExportTasks
    | DescribeImageAttribute
    | DescribeImages
    | DescribeInstanceAttribute
--    | DescribeInstances
    | DescribeInstanceStatus
    | DescribeInternetGateways
    | DescribeKeyPairs
    | DescribeLicenses
    | DescribeNetworkAcls
    | DescribeNetworkInterfaceAttribute
    | DescribeNetworkInterfaces
    | DescribePlacementGroups
    | DescribeRegions
    | DescribeReservedInstances
    | DescribeReservedInstancesListings
    | DescribeReservedInstancesOfferings
    | DescribeRouteTables
    | DescribeSecurityGroups
    | DescribeSnapshotAttribute
    | DescribeSnapshots
    | DescribeSpotDatafeedSubscription
    | DescribeSpotInstanceRequests
    | DescribeSpotPriceHistory
    | DescribeSubnets
    | DescribeTags
    | DescribeVolumeAttribute
    | DescribeVolumes
    | DescribeVolumeStatus
    | DescribeVpcAttribute
    | DescribeVpcs
    | DescribeVpnConnections
    | DescribeVpnGateways
    | DetachInternetGateway
    | DetachNetworkInterface
    | DetachVolume
    | DetachVpnGateway
    | DisableVgwRoutePropagation
    | DisassociateAddress
    | DisassociateRouteTable
    | EnableVgwRoutePropagation
    | EnableVolumeIO
    | GetConsoleOutput
    | GetPasswordData
    | ImportInstance
    | ImportKeyPair
    | ImportVolume
    | ModifyImageAttribute
    | ModifyInstanceAttribute
    | ModifyNetworkInterfaceAttribute
    | ModifySnapshotAttribute
    | ModifyVolumeAttribute
    | ModifyVpcAttribute
    | MonitorInstances
    | PurchaseReservedInstancesOffering
    | RebootInstances
    | RegisterImage
    | ReleaseAddress
    | ReplaceNetworkAclAssociation
    | ReplaceNetworkAclEntry
    | ReplaceRoute
    | ReplaceRouteTableAssociation
    | ReportInstanceStatus
    | RequestSpotInstances
    | ResetImageAttribute
    | ResetInstanceAttribute
    | ResetNetworkInterfaceAttribute
    | ResetSnapshotAttribute
    | RevokeSecurityGroupEgress
    | RevokeSecurityGroupIngress
    | RunInstances
    | StartInstances
    | StopInstances
    | TerminateInstances
    | UnassignPrivateIpAddresses
    | UnmonitorInstances
