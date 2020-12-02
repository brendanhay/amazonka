{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Sum where

import Network.AWS.Prelude

-- | Indicates the address family for the BGP peer.
--
--
--     * __ipv4__ : IPv4 address family
--
--     * __ipv6__ : IPv6 address family
--
--
--
data AddressFamily
  = IPV4
  | IPV6
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AddressFamily where
    parser = takeLowerText >>= \case
        "ipv4" -> pure IPV4
        "ipv6" -> pure IPV6
        e -> fromTextError $ "Failure parsing AddressFamily from value: '" <> e
           <> "'. Accepted values: ipv4, ipv6"

instance ToText AddressFamily where
    toText = \case
        IPV4 -> "ipv4"
        IPV6 -> "ipv6"

instance Hashable     AddressFamily
instance NFData       AddressFamily
instance ToByteString AddressFamily
instance ToQuery      AddressFamily
instance ToHeader     AddressFamily

instance ToJSON AddressFamily where
    toJSON = toJSONText

instance FromJSON AddressFamily where
    parseJSON = parseJSONText "AddressFamily"

-- | The state of the BGP peer.
--
--
--     * __Verifying__ : The BGP peering addresses or ASN require validation before the BGP peer can be created. This state only applies to BGP peers on a public virtual interface.
--
--     * __Pending__ : The BGP peer has been created, and is in this state until it is ready to be established.
--
--     * __Available__ : The BGP peer can be established.
--
--     * __Deleting__ : The BGP peer is in the process of being deleted.
--
--     * __Deleted__ : The BGP peer has been deleted and cannot be established.
--
--
--
data BGPPeerState
  = Available
  | Deleted
  | Deleting
  | Pending
  | Verifying
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BGPPeerState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "pending" -> pure Pending
        "verifying" -> pure Verifying
        e -> fromTextError $ "Failure parsing BGPPeerState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending, verifying"

instance ToText BGPPeerState where
    toText = \case
        Available -> "available"
        Deleted -> "deleted"
        Deleting -> "deleting"
        Pending -> "pending"
        Verifying -> "verifying"

instance Hashable     BGPPeerState
instance NFData       BGPPeerState
instance ToByteString BGPPeerState
instance ToQuery      BGPPeerState
instance ToHeader     BGPPeerState

instance FromJSON BGPPeerState where
    parseJSON = parseJSONText "BGPPeerState"

-- | The Up/Down state of the BGP peer.
--
--
--     * __Up__ : The BGP peer is established.
--
--     * __Down__ : The BGP peer is down.
--
--
--
data BGPStatus
  = Down
  | UP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BGPStatus where
    parser = takeLowerText >>= \case
        "down" -> pure Down
        "up" -> pure UP
        e -> fromTextError $ "Failure parsing BGPStatus from value: '" <> e
           <> "'. Accepted values: down, up"

instance ToText BGPStatus where
    toText = \case
        Down -> "down"
        UP -> "up"

instance Hashable     BGPStatus
instance NFData       BGPStatus
instance ToByteString BGPStatus
instance ToQuery      BGPStatus
instance ToHeader     BGPStatus

instance FromJSON BGPStatus where
    parseJSON = parseJSONText "BGPStatus"

-- | State of the connection.
--
--
--     * __Ordering__ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
--
--     * __Requested__ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--     * __Pending__ : The connection has been approved, and is being initialized.
--
--     * __Available__ : The network link is up, and the connection is ready for use.
--
--     * __Down__ : The network link is down.
--
--     * __Deleting__ : The connection is in the process of being deleted.
--
--     * __Deleted__ : The connection has been deleted.
--
--     * __Rejected__ : A hosted connection in the 'Ordering' state will enter the 'Rejected' state if it is deleted by the end customer.
--
--
--
data ConnectionState
  = CSAvailable
  | CSDeleted
  | CSDeleting
  | CSDown
  | CSOrdering
  | CSPending
  | CSRejected
  | CSRequested
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionState where
    parser = takeLowerText >>= \case
        "available" -> pure CSAvailable
        "deleted" -> pure CSDeleted
        "deleting" -> pure CSDeleting
        "down" -> pure CSDown
        "ordering" -> pure CSOrdering
        "pending" -> pure CSPending
        "rejected" -> pure CSRejected
        "requested" -> pure CSRequested
        e -> fromTextError $ "Failure parsing ConnectionState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, ordering, pending, rejected, requested"

instance ToText ConnectionState where
    toText = \case
        CSAvailable -> "available"
        CSDeleted -> "deleted"
        CSDeleting -> "deleting"
        CSDown -> "down"
        CSOrdering -> "ordering"
        CSPending -> "pending"
        CSRejected -> "rejected"
        CSRequested -> "requested"

instance Hashable     ConnectionState
instance NFData       ConnectionState
instance ToByteString ConnectionState
instance ToQuery      ConnectionState
instance ToHeader     ConnectionState

instance FromJSON ConnectionState where
    parseJSON = parseJSONText "ConnectionState"

-- | State of the direct connect gateway association.
--
--
--     * __Associating__ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .
--
--     * __Associated__ : The direct connect gateway and virtual private gateway are successfully associated and ready to pass traffic.
--
--     * __Disassociating__ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .
--
--     * __Disassociated__ : The virtual private gateway is successfully disassociated from the direct connect gateway. Traffic flow between the direct connect gateway and virtual private gateway stops.
--
--
--
data DirectConnectGatewayAssociationState
  = Associated
  | Associating
  | Disassociated
  | Disassociating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectConnectGatewayAssociationState where
    parser = takeLowerText >>= \case
        "associated" -> pure Associated
        "associating" -> pure Associating
        "disassociated" -> pure Disassociated
        "disassociating" -> pure Disassociating
        e -> fromTextError $ "Failure parsing DirectConnectGatewayAssociationState from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating"

instance ToText DirectConnectGatewayAssociationState where
    toText = \case
        Associated -> "associated"
        Associating -> "associating"
        Disassociated -> "disassociated"
        Disassociating -> "disassociating"

instance Hashable     DirectConnectGatewayAssociationState
instance NFData       DirectConnectGatewayAssociationState
instance ToByteString DirectConnectGatewayAssociationState
instance ToQuery      DirectConnectGatewayAssociationState
instance ToHeader     DirectConnectGatewayAssociationState

instance FromJSON DirectConnectGatewayAssociationState where
    parseJSON = parseJSONText "DirectConnectGatewayAssociationState"

-- | State of the direct connect gateway attachment.
--
--
--     * __Attaching__ : The initial state after a virtual interface is created using the direct connect gateway.
--
--     * __Attached__ : The direct connect gateway and virtual interface are successfully attached and ready to pass traffic.
--
--     * __Detaching__ : The initial state after calling 'DeleteVirtualInterface' on a virtual interface that is attached to a direct connect gateway.
--
--     * __Detached__ : The virtual interface is successfully detached from the direct connect gateway. Traffic flow between the direct connect gateway and virtual interface stops.
--
--
--
data DirectConnectGatewayAttachmentState
  = Attached
  | Attaching
  | Detached
  | Detaching
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectConnectGatewayAttachmentState where
    parser = takeLowerText >>= \case
        "attached" -> pure Attached
        "attaching" -> pure Attaching
        "detached" -> pure Detached
        "detaching" -> pure Detaching
        e -> fromTextError $ "Failure parsing DirectConnectGatewayAttachmentState from value: '" <> e
           <> "'. Accepted values: attached, attaching, detached, detaching"

instance ToText DirectConnectGatewayAttachmentState where
    toText = \case
        Attached -> "attached"
        Attaching -> "attaching"
        Detached -> "detached"
        Detaching -> "detaching"

instance Hashable     DirectConnectGatewayAttachmentState
instance NFData       DirectConnectGatewayAttachmentState
instance ToByteString DirectConnectGatewayAttachmentState
instance ToQuery      DirectConnectGatewayAttachmentState
instance ToHeader     DirectConnectGatewayAttachmentState

instance FromJSON DirectConnectGatewayAttachmentState where
    parseJSON = parseJSONText "DirectConnectGatewayAttachmentState"

-- | State of the direct connect gateway.
--
--
--     * __Pending__ : The initial state after calling 'CreateDirectConnectGateway' .
--
--     * __Available__ : The direct connect gateway is ready for use.
--
--     * __Deleting__ : The initial state after calling 'DeleteDirectConnectGateway' .
--
--     * __Deleted__ : The direct connect gateway is deleted and cannot pass traffic.
--
--
--
data DirectConnectGatewayState
  = DCGSAvailable
  | DCGSDeleted
  | DCGSDeleting
  | DCGSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectConnectGatewayState where
    parser = takeLowerText >>= \case
        "available" -> pure DCGSAvailable
        "deleted" -> pure DCGSDeleted
        "deleting" -> pure DCGSDeleting
        "pending" -> pure DCGSPending
        e -> fromTextError $ "Failure parsing DirectConnectGatewayState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText DirectConnectGatewayState where
    toText = \case
        DCGSAvailable -> "available"
        DCGSDeleted -> "deleted"
        DCGSDeleting -> "deleting"
        DCGSPending -> "pending"

instance Hashable     DirectConnectGatewayState
instance NFData       DirectConnectGatewayState
instance ToByteString DirectConnectGatewayState
instance ToQuery      DirectConnectGatewayState
instance ToHeader     DirectConnectGatewayState

instance FromJSON DirectConnectGatewayState where
    parseJSON = parseJSONText "DirectConnectGatewayState"

-- | State of the interconnect.
--
--
--     * __Requested__ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--     * __Pending__ : The interconnect has been approved, and is being initialized.
--
--     * __Available__ : The network link is up, and the interconnect is ready for use.
--
--     * __Down__ : The network link is down.
--
--     * __Deleting__ : The interconnect is in the process of being deleted.
--
--     * __Deleted__ : The interconnect has been deleted.
--
--
--
data InterconnectState
  = ISAvailable
  | ISDeleted
  | ISDeleting
  | ISDown
  | ISPending
  | ISRequested
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InterconnectState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deleted" -> pure ISDeleted
        "deleting" -> pure ISDeleting
        "down" -> pure ISDown
        "pending" -> pure ISPending
        "requested" -> pure ISRequested
        e -> fromTextError $ "Failure parsing InterconnectState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, pending, requested"

instance ToText InterconnectState where
    toText = \case
        ISAvailable -> "available"
        ISDeleted -> "deleted"
        ISDeleting -> "deleting"
        ISDown -> "down"
        ISPending -> "pending"
        ISRequested -> "requested"

instance Hashable     InterconnectState
instance NFData       InterconnectState
instance ToByteString InterconnectState
instance ToQuery      InterconnectState
instance ToHeader     InterconnectState

instance FromJSON InterconnectState where
    parseJSON = parseJSONText "InterconnectState"

-- | The state of the LAG.
--
--
--     * __Requested__ : The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.
--
--     * __Pending__ : The LAG has been approved, and is being initialized.
--
--     * __Available__ : The network link is established, and the LAG is ready for use.
--
--     * __Down__ : The network link is down.
--
--     * __Deleting__ : The LAG is in the process of being deleted.
--
--     * __Deleted__ : The LAG has been deleted.
--
--
--
data LagState
  = LSAvailable
  | LSDeleted
  | LSDeleting
  | LSDown
  | LSPending
  | LSRequested
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LagState where
    parser = takeLowerText >>= \case
        "available" -> pure LSAvailable
        "deleted" -> pure LSDeleted
        "deleting" -> pure LSDeleting
        "down" -> pure LSDown
        "pending" -> pure LSPending
        "requested" -> pure LSRequested
        e -> fromTextError $ "Failure parsing LagState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, pending, requested"

instance ToText LagState where
    toText = \case
        LSAvailable -> "available"
        LSDeleted -> "deleted"
        LSDeleting -> "deleting"
        LSDown -> "down"
        LSPending -> "pending"
        LSRequested -> "requested"

instance Hashable     LagState
instance NFData       LagState
instance ToByteString LagState
instance ToQuery      LagState
instance ToHeader     LagState

instance FromJSON LagState where
    parseJSON = parseJSONText "LagState"

-- | A standard media type indicating the content type of the LOA-CFA document. Currently, the only supported value is "application/pdf".
--
--
-- Default: application/pdf
--
data LoaContentType =
  ApplicationPdf
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoaContentType where
    parser = takeLowerText >>= \case
        "application/pdf" -> pure ApplicationPdf
        e -> fromTextError $ "Failure parsing LoaContentType from value: '" <> e
           <> "'. Accepted values: application/pdf"

instance ToText LoaContentType where
    toText = \case
        ApplicationPdf -> "application/pdf"

instance Hashable     LoaContentType
instance NFData       LoaContentType
instance ToByteString LoaContentType
instance ToQuery      LoaContentType
instance ToHeader     LoaContentType

instance ToJSON LoaContentType where
    toJSON = toJSONText

instance FromJSON LoaContentType where
    parseJSON = parseJSONText "LoaContentType"

-- | State of the virtual interface.
--
--
--     * __Confirming__ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
--
--     * __Verifying__ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
--
--     * __Pending__ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
--
--     * __Available__ : A virtual interface that is able to forward traffic.
--
--     * __Down__ : A virtual interface that is BGP down.
--
--     * __Deleting__ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.
--
--     * __Deleted__ : A virtual interface that cannot forward traffic.
--
--     * __Rejected__ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the 'Confirming' state is deleted by the virtual interface owner, the virtual interface will enter the 'Rejected' state.
--
--
--
data VirtualInterfaceState
  = VISAvailable
  | VISConfirming
  | VISDeleted
  | VISDeleting
  | VISDown
  | VISPending
  | VISRejected
  | VISVerifying
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VirtualInterfaceState where
    parser = takeLowerText >>= \case
        "available" -> pure VISAvailable
        "confirming" -> pure VISConfirming
        "deleted" -> pure VISDeleted
        "deleting" -> pure VISDeleting
        "down" -> pure VISDown
        "pending" -> pure VISPending
        "rejected" -> pure VISRejected
        "verifying" -> pure VISVerifying
        e -> fromTextError $ "Failure parsing VirtualInterfaceState from value: '" <> e
           <> "'. Accepted values: available, confirming, deleted, deleting, down, pending, rejected, verifying"

instance ToText VirtualInterfaceState where
    toText = \case
        VISAvailable -> "available"
        VISConfirming -> "confirming"
        VISDeleted -> "deleted"
        VISDeleting -> "deleting"
        VISDown -> "down"
        VISPending -> "pending"
        VISRejected -> "rejected"
        VISVerifying -> "verifying"

instance Hashable     VirtualInterfaceState
instance NFData       VirtualInterfaceState
instance ToByteString VirtualInterfaceState
instance ToQuery      VirtualInterfaceState
instance ToHeader     VirtualInterfaceState

instance FromJSON VirtualInterfaceState where
    parseJSON = parseJSONText "VirtualInterfaceState"
