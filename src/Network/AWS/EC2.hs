{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- |
-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2 where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.Monoid
import Network.AWS.Internal
import Network.Http.Client

--
-- EC2 Requests
--

-- <Response>
--     <Errors>
--          <Error>
--            <Code>Error code text</Code>
--            <Message>Error message</Message>
--          </Error>
--     </Errors>
--     <RequestID>request ID</RequestID>
-- </Response>

data ErrorResponse = ErrorResponse
    { errErrors    :: ![(String, String)]
    , errRequestId :: !String
    } deriving (Show)

data EC2Response a
    = Error !ErrorResponse
    | Valid !a
      deriving (Show)

data EC2

instance AWSSigner EC2 where
    sign = version2

instance AWSRegion EC2 where
    regionalise reg rq = rq { rqHost = "ec2." <> toBS reg <> ".amazonaws.com" }

get :: (QueryString a, FromJSON b) => ByteString -> a -> AWS (RawRequest EC2 b)
get = req GET

req :: (QueryString a, FromJSON b)
    => Method
    -> ByteString
    -> a
    -> AWS (RawRequest EC2 b)
req meth action qry = return $ (emptyRequest meth version endpoint "" Nothing)
    { rqAction = Just action
    , rqQuery  = queryString qry
    }

version :: ApiVersion
version  = "2013-06-15"

endpoint :: ByteString
endpoint = "ec2.amazonaws.com"

--
-- Actions
--

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AllocateAddress.html>
data AllocateAddress = AllocateAddress
    { allocateAddressDomain :: !(Maybe ByteString)
    } deriving (Show)

$(deriveQS ''AllocateAddress)

instance AWSRequest EC2 AllocateAddress Object where
    request = get "AllocateAddress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssignPrivateIpAddresses.html>
data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { assignPrivateIpAddressesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AssignPrivateIpAddresses)

instance AWSRequest EC2 AssignPrivateIpAddresses Object where
    request = get "AssignPrivateIpAddresses"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateAddress.html>
data AssociateAddress = AssociateAddress
    { associateAddressDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AssociateAddress)

instance AWSRequest EC2 AssociateAddress Object where
    request = get "AssociateAddress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateDhcpOptions.html>
data AssociateDhcpOptions = AssociateDhcpOptions
    { associateDhcpOptionsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AssociateDhcpOptions)

instance AWSRequest EC2 AssociateDhcpOptions Object where
    request = get "AssociateDhcpOptions"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateRouteTable.html>
data AssociateRouteTable = AssociateRouteTable
    { associateRouteTableDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AssociateRouteTable)

instance AWSRequest EC2 AssociateRouteTable Object where
    request = get "AssociateRouteTable"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachInternetGateway.html>
data AttachInternetGateway = AttachInternetGateway
    { attachInternetGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AttachInternetGateway)

instance AWSRequest EC2 AttachInternetGateway Object where
    request = get "AttachInternetGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachNetworkInterface.html>
data AttachNetworkInterface = AttachNetworkInterface
    { attachNetworkInterfaceDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AttachNetworkInterface)

instance AWSRequest EC2 AttachNetworkInterface Object where
    request = get "AttachNetworkInterface"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVolume.html>
data AttachVolume = AttachVolume
    { attachVolumeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AttachVolume)

instance AWSRequest EC2 AttachVolume Object where
    request = get "AttachVolume"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVpnGateway.html>
data AttachVpnGateway = AttachVpnGateway
    { attachVpnGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AttachVpnGateway)

instance AWSRequest EC2 AttachVpnGateway Object where
    request = get "AttachVpnGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupEgress.html>
data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { authorizeSecurityGroupEgressDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AuthorizeSecurityGroupEgress)

instance AWSRequest EC2 AuthorizeSecurityGroupEgress Object where
    request = get "AuthorizeSecurityGroupEgress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AuthorizeSecurityGroupIngress.html>
data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { authorizeSecurityGroupIngressDummy :: ByteString
    } deriving (Show)

$(deriveQS ''AuthorizeSecurityGroupIngress)

instance AWSRequest EC2 AuthorizeSecurityGroupIngress Object where
    request = get "AuthorizeSecurityGroupIngress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-BundleInstance.html>
data BundleInstance = BundleInstance
    { bundleInstanceDummy :: ByteString
    } deriving (Show)

$(deriveQS ''BundleInstance)

instance AWSRequest EC2 BundleInstance Object where
    request = get "BundleInstance"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelBundleTask.html>
data CancelBundleTask = CancelBundleTask
    { cancelBundleTaskDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CancelBundleTask)

instance AWSRequest EC2 CancelBundleTask Object where
    request = get "CancelBundleTask"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelConversionTask.html>
data CancelConversionTask = CancelConversionTask
    { cancelConversionTaskDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CancelConversionTask)

instance AWSRequest EC2 CancelConversionTask Object where
    request = get "CancelConversionTask"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelExportTask.html>
data CancelExportTask = CancelExportTask
    { cancelExportTaskDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CancelExportTask)

instance AWSRequest EC2 CancelExportTask Object where
    request = get "CancelExportTask"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelReservedInstancesListing.html>
data CancelReservedInstancesListing = CancelReservedInstancesListing
    { cancelReservedInstancesListingDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CancelReservedInstancesListing)

instance AWSRequest EC2 CancelReservedInstancesListing Object where
    request = get "CancelReservedInstancesListing"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotInstanceRequests.html>
data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { cancelSpotInstanceRequestsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CancelSpotInstanceRequests)

instance AWSRequest EC2 CancelSpotInstanceRequests Object where
    request = get "CancelSpotInstanceRequests"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ConfirmProductInstance.html>
data ConfirmProductInstance = ConfirmProductInstance
    { confirmProductInstanceDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ConfirmProductInstance)

instance AWSRequest EC2 ConfirmProductInstance Object where
    request = get "ConfirmProductInstance"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopyImage.html>
data CopyImage = CopyImage
    { copyImageDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CopyImage)

instance AWSRequest EC2 CopyImage Object where
    request = get "CopyImage"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopySnapshot.html>
data CopySnapshot = CopySnapshot
    { copySnapshotDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CopySnapshot)

instance AWSRequest EC2 CopySnapshot Object where
    request = get "CopySnapshot"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateCustomerGateway.html>
data CreateCustomerGateway = CreateCustomerGateway
    { createCustomerGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateCustomerGateway)

instance AWSRequest EC2 CreateCustomerGateway Object where
    request = get "CreateCustomerGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateDhcpOptions.html>
data CreateDhcpOptions = CreateDhcpOptions
    { createDhcpOptionsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateDhcpOptions)

instance AWSRequest EC2 CreateDhcpOptions Object where
    request = get "CreateDhcpOptions"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateImage.html>
data CreateImage = CreateImage
    { createImageDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateImage)

instance AWSRequest EC2 CreateImage Object where
    request = get "CreateImage"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInstanceExportTask.html>
data CreateInstanceExportTask = CreateInstanceExportTask
    { createInstanceExportTaskDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateInstanceExportTask)

instance AWSRequest EC2 CreateInstanceExportTask Object where
    request = get "CreateInstanceExportTask"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInternetGateway.html>
data CreateInternetGateway = CreateInternetGateway
    { createInternetGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateInternetGateway)

instance AWSRequest EC2 CreateInternetGateway Object where
    request = get "CreateInternetGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateKeyPair.html>
data CreateKeyPair = CreateKeyPair
    { createKeyPairDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateKeyPair)

instance AWSRequest EC2 CreateKeyPair Object where
    request = get "CreateKeyPair"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAcl.html>
data CreateNetworkAcl = CreateNetworkAcl
    { createNetworkAclDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateNetworkAcl)

instance AWSRequest EC2 CreateNetworkAcl Object where
    request = get "CreateNetworkAcl"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAclEntry.html>
data CreateNetworkAclEntry = CreateNetworkAclEntry
    { createNetworkAclEntryDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateNetworkAclEntry)

instance AWSRequest EC2 CreateNetworkAclEntry Object where
    request = get "CreateNetworkAclEntry"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkInterface.html>
data CreateNetworkInterface = CreateNetworkInterface
    { createNetworkInterfaceDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateNetworkInterface)

instance AWSRequest EC2 CreateNetworkInterface Object where
    request = get "CreateNetworkInterface"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreatePlacementGroup.html>
data CreatePlacementGroup = CreatePlacementGroup
    { createPlacementGroupDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreatePlacementGroup)

instance AWSRequest EC2 CreatePlacementGroup Object where
    request = get "CreatePlacementGroup"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateReservedInstancesListing.html>
data CreateReservedInstancesListing = CreateReservedInstancesListing
    { createReservedInstancesListingDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateReservedInstancesListing)

instance AWSRequest EC2 CreateReservedInstancesListing Object where
    request = get "CreateReservedInstancesListing"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRoute.html>
data CreateRoute = CreateRoute
    { createRouteDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateRoute)

instance AWSRequest EC2 CreateRoute Object where
    request = get "CreateRoute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRouteTable.html>
data CreateRouteTable = CreateRouteTable
    { createRouteTableDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateRouteTable)

instance AWSRequest EC2 CreateRouteTable Object where
    request = get "CreateRouteTable"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSecurityGroup.html>
data CreateSecurityGroup = CreateSecurityGroup
    { createSecurityGroupDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateSecurityGroup)

instance AWSRequest EC2 CreateSecurityGroup Object where
    request = get "CreateSecurityGroup"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSnapshot.html>
data CreateSnapshot = CreateSnapshot
    { createSnapshotDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateSnapshot)

instance AWSRequest EC2 CreateSnapshot Object where
    request = get "CreateSnapshot"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSpotDatafeedSubscription.html>
data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { createSpotDatafeedSubscriptionDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateSpotDatafeedSubscription)

instance AWSRequest EC2 CreateSpotDatafeedSubscription Object where
    request = get "CreateSpotDatafeedSubscription"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSubnet.html>
data CreateSubnet = CreateSubnet
    { createSubnetDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateSubnet)

instance AWSRequest EC2 CreateSubnet Object where
    request = get "CreateSubnet"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateTags.html>
data CreateTags = CreateTags
    { createTagsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateTags)

instance AWSRequest EC2 CreateTags Object where
    request = get "CreateTags"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVolume.html>
data CreateVolume = CreateVolume
    { createVolumeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateVolume)

instance AWSRequest EC2 CreateVolume Object where
    request = get "CreateVolume"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPC.html>
data CreateVPC = CreateVPC
    { createVPCDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateVPC)

instance AWSRequest EC2 CreateVPC Object where
    request = get "CreateVPC"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnection.html>
data CreateVpnConnection = CreateVpnConnection
    { createVpnConnectionDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateVpnConnection)

instance AWSRequest EC2 CreateVpnConnection Object where
    request = get "CreateVpnConnection"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnectionRoute.html>
data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { createVpnConnectionRouteDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateVpnConnectionRoute)

instance AWSRequest EC2 CreateVpnConnectionRoute Object where
    request = get "CreateVpnConnectionRoute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html>
data CreateVpnGateway = CreateVpnGateway
    { createVpnGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''CreateVpnGateway)

instance AWSRequest EC2 CreateVpnGateway Object where
    request = get "CreateVpnGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeactivateLicense.html>
data DeactivateLicense = DeactivateLicense
    { deactivateLicenseDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeactivateLicense)

instance AWSRequest EC2 DeactivateLicense Object where
    request = get "DeactivateLicense"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteCustomerGateway.html>
data DeleteCustomerGateway = DeleteCustomerGateway
    { deleteCustomerGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteCustomerGateway)

instance AWSRequest EC2 DeleteCustomerGateway Object where
    request = get "DeleteCustomerGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteDhcpOptions.html>
data DeleteDhcpOptions = DeleteDhcpOptions
    { deleteDhcpOptionsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteDhcpOptions)

instance AWSRequest EC2 DeleteDhcpOptions Object where
    request = get "DeleteDhcpOptions"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteInternetGateway.html>
data DeleteInternetGateway = DeleteInternetGateway
    { deleteInternetGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteInternetGateway)

instance AWSRequest EC2 DeleteInternetGateway Object where
    request = get "DeleteInternetGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteKeyPair.html>
data DeleteKeyPair = DeleteKeyPair
    { deleteKeyPairDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteKeyPair)

instance AWSRequest EC2 DeleteKeyPair Object where
    request = get "DeleteKeyPair"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAcl.html>
data DeleteNetworkAcl = DeleteNetworkAcl
    { deleteNetworkAclDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteNetworkAcl)

instance AWSRequest EC2 DeleteNetworkAcl Object where
    request = get "DeleteNetworkAcl"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAclEntry.html>
data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { deleteNetworkAclEntryDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteNetworkAclEntry)

instance AWSRequest EC2 DeleteNetworkAclEntry Object where
    request = get "DeleteNetworkAclEntry"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkInterface.html>
data DeleteNetworkInterface = DeleteNetworkInterface
    { deleteNetworkInterfaceDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteNetworkInterface)

instance AWSRequest EC2 DeleteNetworkInterface Object where
    request = get "DeleteNetworkInterface"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeletePlacementGroup.html>
data DeletePlacementGroup = DeletePlacementGroup
    { deletePlacementGroupDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeletePlacementGroup)

instance AWSRequest EC2 DeletePlacementGroup Object where
    request = get "DeletePlacementGroup"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRoute.html>
data DeleteRoute = DeleteRoute
    { deleteRouteDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteRoute)

instance AWSRequest EC2 DeleteRoute Object where
    request = get "DeleteRoute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRouteTable.html>
data DeleteRouteTable = DeleteRouteTable
    { deleteRouteTableDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteRouteTable)

instance AWSRequest EC2 DeleteRouteTable Object where
    request = get "DeleteRouteTable"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSecurityGroup.html>
data DeleteSecurityGroup = DeleteSecurityGroup
    { deleteSecurityGroupDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteSecurityGroup)

instance AWSRequest EC2 DeleteSecurityGroup Object where
    request = get "DeleteSecurityGroup"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSnapshot.html>
data DeleteSnapshot = DeleteSnapshot
    { deleteSnapshotDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteSnapshot)

instance AWSRequest EC2 DeleteSnapshot Object where
    request = get "DeleteSnapshot"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSpotDatafeedSubscription.html>
data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { deleteSpotDatafeedSubscriptionDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteSpotDatafeedSubscription)

instance AWSRequest EC2 DeleteSpotDatafeedSubscription Object where
    request = get "DeleteSpotDatafeedSubscription"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSubnet.html>
data DeleteSubnet = DeleteSubnet
    { deleteSubnetDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteSubnet)

instance AWSRequest EC2 DeleteSubnet Object where
    request = get "DeleteSubnet"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteTags.html>
data DeleteTags = DeleteTags
    { deleteTagsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteTags)

instance AWSRequest EC2 DeleteTags Object where
    request = get "DeleteTags"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html>
data DeleteVolume = DeleteVolume
    { deleteVolumeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteVolume)

instance AWSRequest EC2 DeleteVolume Object where
    request = get "DeleteVolume"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPC.html>
data DeleteVPC = DeleteVPC
    { deleteVPCDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteVPC)

instance AWSRequest EC2 DeleteVPC Object where
    request = get "DeleteVPC"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnConnection.html>
data DeleteVpnConnection = DeleteVpnConnection
    { deleteVpnConnectionDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteVpnConnection)

instance AWSRequest EC2 DeleteVpnConnection Object where
    request = get "DeleteVpnConnection"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnConnectionRoute.html>
data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { deleteVpnConnectionRouteDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteVpnConnectionRoute)

instance AWSRequest EC2 DeleteVpnConnectionRoute Object where
    request = get "DeleteVpnConnectionRoute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpnGateway.html>
data DeleteVpnGateway = DeleteVpnGateway
    { deleteVpnGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeleteVpnGateway)

instance AWSRequest EC2 DeleteVpnGateway Object where
    request = get "DeleteVpnGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeregisterImage.html>
data DeregisterImage = DeregisterImage
    { deregisterImageDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DeregisterImage)

instance AWSRequest EC2 DeregisterImage Object where
    request = get "DeregisterImage"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAccountAttributes.html>
data DescribeAccountAttributes = DescribeAccountAttributes
    { describeAccountAttributesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeAccountAttributes)

instance AWSRequest EC2 DescribeAccountAttributes Object where
    request = get "DescribeAccountAttributes"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAddresses.html>
data DescribeAddresses = DescribeAddresses
    { describeAddressesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeAddresses)

instance AWSRequest EC2 DescribeAddresses Object where
    request = get "DescribeAddresses"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAvailabilityZones.html>
data DescribeAvailabilityZones = DescribeAvailabilityZones
    { describeAvailabilityZonesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeAvailabilityZones)

instance AWSRequest EC2 DescribeAvailabilityZones Object where
    request = get "DescribeAvailabilityZones"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeBundleTasks.html>
data DescribeBundleTasks = DescribeBundleTasks
    { describeBundleTasksDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeBundleTasks)

instance AWSRequest EC2 DescribeBundleTasks Object where
    request = get "DescribeBundleTasks"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeConversionTasks.html>
data DescribeConversionTasks = DescribeConversionTasks
    { describeConversionTasksDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeConversionTasks)

instance AWSRequest EC2 DescribeConversionTasks Object where
    request = get "DescribeConversionTasks"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeCustomerGateways.html>
data DescribeCustomerGateways = DescribeCustomerGateways
    { describeCustomerGatewaysDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeCustomerGateways)

instance AWSRequest EC2 DescribeCustomerGateways Object where
    request = get "DescribeCustomerGateways"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeDhcpOptions.html>
data DescribeDhcpOptions = DescribeDhcpOptions
    { describeDhcpOptionsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeDhcpOptions)

instance AWSRequest EC2 DescribeDhcpOptions Object where
    request = get "DescribeDhcpOptions"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeExportTasks.html>
data DescribeExportTasks = DescribeExportTasks
    { describeExportTasksDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeExportTasks)

instance AWSRequest EC2 DescribeExportTasks Object where
    request = get "DescribeExportTasks"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImageAttribute.html>
data DescribeImageAttribute = DescribeImageAttribute
    { describeImageAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeImageAttribute)

instance AWSRequest EC2 DescribeImageAttribute Object where
    request = get "DescribeImageAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImages.html>
data DescribeImages = DescribeImages
    { describeImagesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeImages)

instance AWSRequest EC2 DescribeImages Object where
    request = get "DescribeImages"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceAttribute.html>
data DescribeInstanceAttribute = DescribeInstanceAttribute
    { describeInstanceAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeInstanceAttribute)

instance AWSRequest EC2 DescribeInstanceAttribute Object where
    request = get "DescribeInstanceAttribute"

-- |
--
--
data DescribeInstances = DescribeInstances
    { describeInstancesInstanceId :: [ByteString]
    } deriving (Show)

$(deriveQS ''DescribeInstances)

instance AWSRequest EC2 DescribeInstances Object where
    request = get "DescribeInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceStatus.html>
data DescribeInstanceStatus = DescribeInstanceStatus
    { describeInstanceStatusDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeInstanceStatus)

instance AWSRequest EC2 DescribeInstanceStatus Object where
    request = get "DescribeInstanceStatus"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInternetGateways.html>
data DescribeInternetGateways = DescribeInternetGateways
    { describeInternetGatewaysDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeInternetGateways)

instance AWSRequest EC2 DescribeInternetGateways Object where
    request = get "DescribeInternetGateways"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeKeyPairs.html>
data DescribeKeyPairs = DescribeKeyPairs
    { describeKeyPairsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeKeyPairs)

instance AWSRequest EC2 DescribeKeyPairs Object where
    request = get "DescribeKeyPairs"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeLicenses.html>
data DescribeLicenses = DescribeLicenses
    { describeLicensesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeLicenses)

instance AWSRequest EC2 DescribeLicenses Object where
    request = get "DescribeLicenses"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkAcls.html>
data DescribeNetworkAcls = DescribeNetworkAcls
    { describeNetworkAclsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeNetworkAcls)

instance AWSRequest EC2 DescribeNetworkAcls Object where
    request = get "DescribeNetworkAcls"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaceAttribute.html>
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { describeNetworkInterfaceAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeNetworkInterfaceAttribute)

instance AWSRequest EC2 DescribeNetworkInterfaceAttribute Object where
    request = get "DescribeNetworkInterfaceAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaces.html>
data DescribeNetworkInterfaces = DescribeNetworkInterfaces
    { describeNetworkInterfacesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeNetworkInterfaces)

instance AWSRequest EC2 DescribeNetworkInterfaces Object where
    request = get "DescribeNetworkInterfaces"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePlacementGroups.html>
data DescribePlacementGroups = DescribePlacementGroups
    { describePlacementGroupsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribePlacementGroups)

instance AWSRequest EC2 DescribePlacementGroups Object where
    request = get "DescribePlacementGroups"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html>
data DescribeRegions = DescribeRegions
    { describeRegionsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeRegions)

instance AWSRequest EC2 DescribeRegions Object where
    request = get "DescribeRegions"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstances.html>
data DescribeReservedInstances = DescribeReservedInstances
    { describeReservedInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeReservedInstances)

instance AWSRequest EC2 DescribeReservedInstances Object where
    request = get "DescribeReservedInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesListings.html>
data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { describeReservedInstancesListingsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeReservedInstancesListings)

instance AWSRequest EC2 DescribeReservedInstancesListings Object where
    request = get "DescribeReservedInstancesListings"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesOfferings.html>
data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { describeReservedInstancesOfferingsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeReservedInstancesOfferings)

instance AWSRequest EC2 DescribeReservedInstancesOfferings Object where
    request = get "DescribeReservedInstancesOfferings"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRouteTables.html>
data DescribeRouteTables = DescribeRouteTables
    { describeRouteTablesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeRouteTables)

instance AWSRequest EC2 DescribeRouteTables Object where
    request = get "DescribeRouteTables"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSecurityGroups.html>
data DescribeSecurityGroups = DescribeSecurityGroups
    { describeSecurityGroupsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeSecurityGroups)

instance AWSRequest EC2 DescribeSecurityGroups Object where
    request = get "DescribeSecurityGroups"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshotAttribute.html>
data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { describeSnapshotAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeSnapshotAttribute)

instance AWSRequest EC2 DescribeSnapshotAttribute Object where
    request = get "DescribeSnapshotAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html>
data DescribeSnapshots = DescribeSnapshots
    { describeSnapshotsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeSnapshots)

instance AWSRequest EC2 DescribeSnapshots Object where
    request = get "DescribeSnapshots"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotDatafeedSubscription.html>
data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    { describeSpotDatafeedSubscriptionDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeSpotDatafeedSubscription)

instance AWSRequest EC2 DescribeSpotDatafeedSubscription Object where
    request = get "DescribeSpotDatafeedSubscription"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotInstanceRequests.html>
data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests
    { describeSpotInstanceRequestsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeSpotInstanceRequests)

instance AWSRequest EC2 DescribeSpotInstanceRequests Object where
    request = get "DescribeSpotInstanceRequests"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotPriceHistory.html>
data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { describeSpotPriceHistoryDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeSpotPriceHistory)

instance AWSRequest EC2 DescribeSpotPriceHistory Object where
    request = get "DescribeSpotPriceHistory"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSubnets.html>
data DescribeSubnets = DescribeSubnets
    { describeSubnetsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeSubnets)

instance AWSRequest EC2 DescribeSubnets Object where
    request = get "DescribeSubnets"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html>
data DescribeTags = DescribeTags
    { describeTagsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeTags)

instance AWSRequest EC2 DescribeTags Object where
    request = get "DescribeTags"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeAttribute.html>
data DescribeVolumeAttribute = DescribeVolumeAttribute
    { describeVolumeAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeVolumeAttribute)

instance AWSRequest EC2 DescribeVolumeAttribute Object where
    request = get "DescribeVolumeAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html>
data DescribeVolumes = DescribeVolumes
    { describeVolumesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeVolumes)

instance AWSRequest EC2 DescribeVolumes Object where
    request = get "DescribeVolumes"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumeStatus.html>
data DescribeVolumeStatus = DescribeVolumeStatus
    { describeVolumeStatusDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeVolumeStatus)

instance AWSRequest EC2 DescribeVolumeStatus Object where
    request = get "DescribeVolumeStatus"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCAttribute.html>
data DescribeVPCAttribute = DescribeVPCAttribute
    { describeVPCAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeVPCAttribute)

instance AWSRequest EC2 DescribeVPCAttribute Object where
    request = get "DescribeVPCAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCs.html>
data DescribeVPCs = DescribeVPCs
    { describeVPCsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeVPCs)

instance AWSRequest EC2 DescribeVPCs Object where
    request = get "DescribeVPCs"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnConnections.html>
data DescribeVpnConnections = DescribeVpnConnections
    { describeVpnConnectionsDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeVpnConnections)

instance AWSRequest EC2 DescribeVpnConnections Object where
    request = get "DescribeVpnConnections"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnGateways.html>
data DescribeVpnGateways = DescribeVpnGateways
    { describeVpnGatewaysDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DescribeVpnGateways)

instance AWSRequest EC2 DescribeVpnGateways Object where
    request = get "DescribeVpnGateways"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachInternetGateway.html>
data DetachInternetGateway = DetachInternetGateway
    { detachInternetGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DetachInternetGateway)

instance AWSRequest EC2 DetachInternetGateway Object where
    request = get "DetachInternetGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachNetworkInterface.html>
data DetachNetworkInterface = DetachNetworkInterface
    { detachNetworkInterfaceDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DetachNetworkInterface)

instance AWSRequest EC2 DetachNetworkInterface Object where
    request = get "DetachNetworkInterface"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVolume.html>
data DetachVolume = DetachVolume
    { detachVolumeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DetachVolume)

instance AWSRequest EC2 DetachVolume Object where
    request = get "DetachVolume"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVpnGateway.html>
data DetachVpnGateway = DetachVpnGateway
    { detachVpnGatewayDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DetachVpnGateway)

instance AWSRequest EC2 DetachVpnGateway Object where
    request = get "DetachVpnGateway"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisableVgwRoutePropagation.html>
data DisableVgwRoutePropagation = DisableVgwRoutePropagation
    { disableVgwRoutePropagationDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DisableVgwRoutePropagation)

instance AWSRequest EC2 DisableVgwRoutePropagation Object where
    request = get "DisableVgwRoutePropagation"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateAddress.html>
data DisassociateAddress = DisassociateAddress
    { disassociateAddressDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DisassociateAddress)

instance AWSRequest EC2 DisassociateAddress Object where
    request = get "DisassociateAddress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateRouteTable.html>
data DisassociateRouteTable = DisassociateRouteTable
    { disassociateRouteTableDummy :: ByteString
    } deriving (Show)

$(deriveQS ''DisassociateRouteTable)

instance AWSRequest EC2 DisassociateRouteTable Object where
    request = get "DisassociateRouteTable"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVgwRoutePropagation.html>
data EnableVgwRoutePropagation = EnableVgwRoutePropagation
    { enableVgwRoutePropagationDummy :: ByteString
    } deriving (Show)

$(deriveQS ''EnableVgwRoutePropagation)

instance AWSRequest EC2 EnableVgwRoutePropagation Object where
    request = get "EnableVgwRoutePropagation"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVolumeIO.html>
data EnableVolumeIO = EnableVolumeIO
    { enableVolumeIODummy :: ByteString
    } deriving (Show)

$(deriveQS ''EnableVolumeIO)

instance AWSRequest EC2 EnableVolumeIO Object where
    request = get "EnableVolumeIO"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetConsoleOutput.html>
data GetConsoleOutput = GetConsoleOutput
    { getConsoleOutputDummy :: ByteString
    } deriving (Show)

$(deriveQS ''GetConsoleOutput)

instance AWSRequest EC2 GetConsoleOutput Object where
    request = get "GetConsoleOutput"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetPasswordData.html>
data GetPasswordData = GetPasswordData
    { getPasswordDataDummy :: ByteString
    } deriving (Show)

$(deriveQS ''GetPasswordData)

instance AWSRequest EC2 GetPasswordData Object where
    request = get "GetPasswordData"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportInstance.html>
data ImportInstance = ImportInstance
    { importInstanceDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ImportInstance)

instance AWSRequest EC2 ImportInstance Object where
    request = get "ImportInstance"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportKeyPair.html>
data ImportKeyPair = ImportKeyPair
    { importKeyPairDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ImportKeyPair)

instance AWSRequest EC2 ImportKeyPair Object where
    request = get "ImportKeyPair"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportVolume.html>
data ImportVolume = ImportVolume
    { importVolumeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ImportVolume)

instance AWSRequest EC2 ImportVolume Object where
    request = get "ImportVolume"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyImageAttribute.html>
data ModifyImageAttribute = ModifyImageAttribute
    { modifyImageAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ModifyImageAttribute)

instance AWSRequest EC2 ModifyImageAttribute Object where
    request = get "ModifyImageAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyInstanceAttribute.html>
data ModifyInstanceAttribute = ModifyInstanceAttribute
    { modifyInstanceAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ModifyInstanceAttribute)

instance AWSRequest EC2 ModifyInstanceAttribute Object where
    request = get "ModifyInstanceAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyNetworkInterfaceAttribute.html>
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { modifyNetworkInterfaceAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ModifyNetworkInterfaceAttribute)

instance AWSRequest EC2 ModifyNetworkInterfaceAttribute Object where
    request = get "ModifyNetworkInterfaceAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySnapshotAttribute.html>
data ModifySnapshotAttribute = ModifySnapshotAttribute
    { modifySnapshotAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ModifySnapshotAttribute)

instance AWSRequest EC2 ModifySnapshotAttribute Object where
    request = get "ModifySnapshotAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVolumeAttribute.html>
data ModifyVolumeAttribute = ModifyVolumeAttribute
    { modifyVolumeAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ModifyVolumeAttribute)

instance AWSRequest EC2 ModifyVolumeAttribute Object where
    request = get "ModifyVolumeAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVPCAttribute.html>
data ModifyVPCAttribute = ModifyVPCAttribute
    { modifyVPCAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ModifyVPCAttribute)

instance AWSRequest EC2 ModifyVPCAttribute Object where
    request = get "ModifyVPCAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MonitorInstances.html>
data MonitorInstances = MonitorInstances
    { monitorInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''MonitorInstances)

instance AWSRequest EC2 MonitorInstances Object where
    request = get "MonitorInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-PurchaseReservedInstancesOffering.html>
data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
    { purchaseReservedInstancesOfferingDummy :: ByteString
    } deriving (Show)

$(deriveQS ''PurchaseReservedInstancesOffering)

instance AWSRequest EC2 PurchaseReservedInstancesOffering Object where
    request = get "PurchaseReservedInstancesOffering"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RebootInstances.html>
data RebootInstances = RebootInstances
    { rebootInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''RebootInstances)

instance AWSRequest EC2 RebootInstances Object where
    request = get "RebootInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RegisterImage.html>
data RegisterImage = RegisterImage
    { registerImageDummy :: ByteString
    } deriving (Show)

$(deriveQS ''RegisterImage)

instance AWSRequest EC2 RegisterImage Object where
    request = get "RegisterImage"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReleaseAddress.html>
data ReleaseAddress = ReleaseAddress
    { releaseAddressDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ReleaseAddress)

instance AWSRequest EC2 ReleaseAddress Object where
    request = get "ReleaseAddress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclAssociation.html>
data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { replaceNetworkAclAssociationDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ReplaceNetworkAclAssociation)

instance AWSRequest EC2 ReplaceNetworkAclAssociation Object where
    request = get "ReplaceNetworkAclAssociation"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclEntry.html>
data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { replaceNetworkAclEntryDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ReplaceNetworkAclEntry)

instance AWSRequest EC2 ReplaceNetworkAclEntry Object where
    request = get "ReplaceNetworkAclEntry"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRoute.html>
data ReplaceRoute = ReplaceRoute
    { replaceRouteDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ReplaceRoute)

instance AWSRequest EC2 ReplaceRoute Object where
    request = get "ReplaceRoute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRouteTableAssociation.html>
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { replaceRouteTableAssociationDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ReplaceRouteTableAssociation)

instance AWSRequest EC2 ReplaceRouteTableAssociation Object where
    request = get "ReplaceRouteTableAssociation"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReportInstanceStatus.html>
data ReportInstanceStatus = ReportInstanceStatus
    { reportInstanceStatusDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ReportInstanceStatus)

instance AWSRequest EC2 ReportInstanceStatus Object where
    request = get "ReportInstanceStatus"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotInstances.html>
data RequestSpotInstances = RequestSpotInstances
    { requestSpotInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''RequestSpotInstances)

instance AWSRequest EC2 RequestSpotInstances Object where
    request = get "RequestSpotInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetImageAttribute.html>
data ResetImageAttribute = ResetImageAttribute
    { resetImageAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ResetImageAttribute)

instance AWSRequest EC2 ResetImageAttribute Object where
    request = get "ResetImageAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetInstanceAttribute.html>
data ResetInstanceAttribute = ResetInstanceAttribute
    { resetInstanceAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ResetInstanceAttribute)

instance AWSRequest EC2 ResetInstanceAttribute Object where
    request = get "ResetInstanceAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetNetworkInterfaceAttribute.html>
data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
    { resetNetworkInterfaceAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ResetNetworkInterfaceAttribute)

instance AWSRequest EC2 ResetNetworkInterfaceAttribute Object where
    request = get "ResetNetworkInterfaceAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetSnapshotAttribute.html>
data ResetSnapshotAttribute = ResetSnapshotAttribute
    { resetSnapshotAttributeDummy :: ByteString
    } deriving (Show)

$(deriveQS ''ResetSnapshotAttribute)

instance AWSRequest EC2 ResetSnapshotAttribute Object where
    request = get "ResetSnapshotAttribute"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupEgress.html>
data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { revokeSecurityGroupEgressDummy :: ByteString
    } deriving (Show)

$(deriveQS ''RevokeSecurityGroupEgress)

instance AWSRequest EC2 RevokeSecurityGroupEgress Object where
    request = get "RevokeSecurityGroupEgress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RevokeSecurityGroupIngress.html>
data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { revokeSecurityGroupIngressDummy :: ByteString
    } deriving (Show)

$(deriveQS ''RevokeSecurityGroupIngress)

instance AWSRequest EC2 RevokeSecurityGroupIngress Object where
    request = get "RevokeSecurityGroupIngress"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RunInstances.html>
data RunInstances = RunInstances
    { runInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''RunInstances)

instance AWSRequest EC2 RunInstances Object where
    request = get "RunInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StartInstances.html>
data StartInstances = StartInstances
    { startInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''StartInstances)

instance AWSRequest EC2 StartInstances Object where
    request = get "StartInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StopInstances.html>
data StopInstances = StopInstances
    { stopInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''StopInstances)

instance AWSRequest EC2 StopInstances Object where
    request = get "StopInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-TerminateInstances.html>
data TerminateInstances = TerminateInstances
    { terminateInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''TerminateInstances)

instance AWSRequest EC2 TerminateInstances Object where
    request = get "TerminateInstances"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnassignPrivateIpAddresses.html>
data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { unassignPrivateIpAddressesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''UnassignPrivateIpAddresses)

instance AWSRequest EC2 UnassignPrivateIpAddresses Object where
    request = get "UnassignPrivateIpAddresses"

-- |
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnmonitorInstances.html>
data UnmonitorInstances = UnmonitorInstances
    { unmonitorInstancesDummy :: ByteString
    } deriving (Show)

$(deriveQS ''UnmonitorInstances)

instance AWSRequest EC2 UnmonitorInstances Object where
    request = get "UnmonitorInstances"
