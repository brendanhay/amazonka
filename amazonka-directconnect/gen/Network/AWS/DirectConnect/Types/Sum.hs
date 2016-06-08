{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Sum where

import           Network.AWS.Prelude

-- | State of the connection.
--
-- -   __Ordering__: The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
-- -   __Requested__: The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
-- -   __Pending__: The connection has been approved, and is being initialized.
-- -   __Available__: The network link is up, and the connection is ready for use.
-- -   __Down__: The network link is down.
-- -   __Deleting__: The connection is in the process of being deleted.
-- -   __Deleted__: The connection has been deleted.
-- -   __Rejected__: A hosted connection in the \'Ordering\' state will enter the \'Rejected\' state if it is deleted by the end customer.
data ConnectionState
    = CSAvailable
    | CSDeleted
    | CSDeleting
    | CSDown
    | CSOrdering
    | CSPending
    | CSRejected
    | CSRequested
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

-- | State of the interconnect.
--
-- -   __Requested__: The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
-- -   __Pending__: The interconnect has been approved, and is being initialized.
-- -   __Available__: The network link is up, and the interconnect is ready for use.
-- -   __Down__: The network link is down.
-- -   __Deleting__: The interconnect is in the process of being deleted.
-- -   __Deleted__: The interconnect has been deleted.
data InterconnectState
    = ISAvailable
    | ISDeleted
    | ISDeleting
    | ISDown
    | ISPending
    | ISRequested
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

-- | State of the virtual interface.
--
-- -   __Confirming__: The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
-- -   __Verifying__: This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
-- -   __Pending__: A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
-- -   __Available__: A virtual interface that is able to forward traffic.
-- -   __Down__: A virtual interface that is BGP down.
-- -   __Deleting__: A virtual interface is in this state immediately after calling /DeleteVirtualInterface/ until it can no longer forward traffic.
-- -   __Deleted__: A virtual interface that cannot forward traffic.
-- -   __Rejected__: The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the \'Confirming\' state is deleted by the virtual interface owner, the virtual interface will enter the \'Rejected\' state.
data VirtualInterfaceState
    = Available
    | Confirming
    | Deleted
    | Deleting
    | Pending
    | Rejected
    | Verifying
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText VirtualInterfaceState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "confirming" -> pure Confirming
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "pending" -> pure Pending
        "rejected" -> pure Rejected
        "verifying" -> pure Verifying
        e -> fromTextError $ "Failure parsing VirtualInterfaceState from value: '" <> e
           <> "'. Accepted values: available, confirming, deleted, deleting, pending, rejected, verifying"

instance ToText VirtualInterfaceState where
    toText = \case
        Available -> "available"
        Confirming -> "confirming"
        Deleted -> "deleted"
        Deleting -> "deleting"
        Pending -> "pending"
        Rejected -> "rejected"
        Verifying -> "verifying"

instance Hashable     VirtualInterfaceState
instance NFData       VirtualInterfaceState
instance ToByteString VirtualInterfaceState
instance ToQuery      VirtualInterfaceState
instance ToHeader     VirtualInterfaceState

instance FromJSON VirtualInterfaceState where
    parseJSON = parseJSONText "VirtualInterfaceState"
