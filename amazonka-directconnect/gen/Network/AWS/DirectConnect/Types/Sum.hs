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

data BGPStatus
  = Down
  | UP
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BGPStatus where
    parser = takeLowerText >>= \case
        "down" -> pure Down
        "up" -> pure UP
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing BGPStatus from value: '" <> e
           <> "'. Accepted values: down, up, unknown"

instance ToText BGPStatus where
    toText = \case
        Down -> "down"
        UP -> "up"
        Unknown -> "unknown"

instance Hashable     BGPStatus
instance NFData       BGPStatus
instance ToByteString BGPStatus
instance ToQuery      BGPStatus
instance ToHeader     BGPStatus

instance FromJSON BGPStatus where
    parseJSON = parseJSONText "BGPStatus"

data ConnectionState
  = CSAvailable
  | CSDeleted
  | CSDeleting
  | CSDown
  | CSOrdering
  | CSPending
  | CSRejected
  | CSRequested
  | CSUnknown
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
        "unknown" -> pure CSUnknown
        e -> fromTextError $ "Failure parsing ConnectionState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, ordering, pending, rejected, requested, unknown"

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
        CSUnknown -> "unknown"

instance Hashable     ConnectionState
instance NFData       ConnectionState
instance ToByteString ConnectionState
instance ToQuery      ConnectionState
instance ToHeader     ConnectionState

instance FromJSON ConnectionState where
    parseJSON = parseJSONText "ConnectionState"

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

data HasLogicalRedundancy
  = HLRNO
  | HLRUnknown
  | HLRYes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HasLogicalRedundancy where
    parser = takeLowerText >>= \case
        "no" -> pure HLRNO
        "unknown" -> pure HLRUnknown
        "yes" -> pure HLRYes
        e -> fromTextError $ "Failure parsing HasLogicalRedundancy from value: '" <> e
           <> "'. Accepted values: no, unknown, yes"

instance ToText HasLogicalRedundancy where
    toText = \case
        HLRNO -> "no"
        HLRUnknown -> "unknown"
        HLRYes -> "yes"

instance Hashable     HasLogicalRedundancy
instance NFData       HasLogicalRedundancy
instance ToByteString HasLogicalRedundancy
instance ToQuery      HasLogicalRedundancy
instance ToHeader     HasLogicalRedundancy

instance FromJSON HasLogicalRedundancy where
    parseJSON = parseJSONText "HasLogicalRedundancy"

data InterconnectState
  = ISAvailable
  | ISDeleted
  | ISDeleting
  | ISDown
  | ISPending
  | ISRequested
  | ISUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InterconnectState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deleted" -> pure ISDeleted
        "deleting" -> pure ISDeleting
        "down" -> pure ISDown
        "pending" -> pure ISPending
        "requested" -> pure ISRequested
        "unknown" -> pure ISUnknown
        e -> fromTextError $ "Failure parsing InterconnectState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, pending, requested, unknown"

instance ToText InterconnectState where
    toText = \case
        ISAvailable -> "available"
        ISDeleted -> "deleted"
        ISDeleting -> "deleting"
        ISDown -> "down"
        ISPending -> "pending"
        ISRequested -> "requested"
        ISUnknown -> "unknown"

instance Hashable     InterconnectState
instance NFData       InterconnectState
instance ToByteString InterconnectState
instance ToQuery      InterconnectState
instance ToHeader     InterconnectState

instance FromJSON InterconnectState where
    parseJSON = parseJSONText "InterconnectState"

data LagState
  = LSAvailable
  | LSDeleted
  | LSDeleting
  | LSDown
  | LSPending
  | LSRequested
  | LSUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LagState where
    parser = takeLowerText >>= \case
        "available" -> pure LSAvailable
        "deleted" -> pure LSDeleted
        "deleting" -> pure LSDeleting
        "down" -> pure LSDown
        "pending" -> pure LSPending
        "requested" -> pure LSRequested
        "unknown" -> pure LSUnknown
        e -> fromTextError $ "Failure parsing LagState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, down, pending, requested, unknown"

instance ToText LagState where
    toText = \case
        LSAvailable -> "available"
        LSDeleted -> "deleted"
        LSDeleting -> "deleting"
        LSDown -> "down"
        LSPending -> "pending"
        LSRequested -> "requested"
        LSUnknown -> "unknown"

instance Hashable     LagState
instance NFData       LagState
instance ToByteString LagState
instance ToQuery      LagState
instance ToHeader     LagState

instance FromJSON LagState where
    parseJSON = parseJSONText "LagState"

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

data VirtualInterfaceState
  = VISAvailable
  | VISConfirming
  | VISDeleted
  | VISDeleting
  | VISDown
  | VISPending
  | VISRejected
  | VISUnknown
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
        "unknown" -> pure VISUnknown
        "verifying" -> pure VISVerifying
        e -> fromTextError $ "Failure parsing VirtualInterfaceState from value: '" <> e
           <> "'. Accepted values: available, confirming, deleted, deleting, down, pending, rejected, unknown, verifying"

instance ToText VirtualInterfaceState where
    toText = \case
        VISAvailable -> "available"
        VISConfirming -> "confirming"
        VISDeleted -> "deleted"
        VISDeleting -> "deleting"
        VISDown -> "down"
        VISPending -> "pending"
        VISRejected -> "rejected"
        VISUnknown -> "unknown"
        VISVerifying -> "verifying"

instance Hashable     VirtualInterfaceState
instance NFData       VirtualInterfaceState
instance ToByteString VirtualInterfaceState
instance ToQuery      VirtualInterfaceState
instance ToHeader     VirtualInterfaceState

instance FromJSON VirtualInterfaceState where
    parseJSON = parseJSONText "VirtualInterfaceState"
