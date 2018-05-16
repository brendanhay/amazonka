{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.Sum where

import Network.AWS.Prelude

data EntityState
  = Deleted
  | Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntityState where
    parser = takeLowerText >>= \case
        "deleted" -> pure Deleted
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing EntityState from value: '" <> e
           <> "'. Accepted values: deleted, disabled, enabled"

instance ToText EntityState where
    toText = \case
        Deleted -> "DELETED"
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     EntityState
instance NFData       EntityState
instance ToByteString EntityState
instance ToQuery      EntityState
instance ToHeader     EntityState

instance FromJSON EntityState where
    parseJSON = parseJSONText "EntityState"

data MemberType
  = Group
  | User
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MemberType where
    parser = takeLowerText >>= \case
        "group" -> pure Group
        "user" -> pure User
        e -> fromTextError $ "Failure parsing MemberType from value: '" <> e
           <> "'. Accepted values: group, user"

instance ToText MemberType where
    toText = \case
        Group -> "GROUP"
        User -> "USER"

instance Hashable     MemberType
instance NFData       MemberType
instance ToByteString MemberType
instance ToQuery      MemberType
instance ToHeader     MemberType

instance FromJSON MemberType where
    parseJSON = parseJSONText "MemberType"

data PermissionType
  = FullAccess
  | SendAs
  | SendOnBehalf
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PermissionType where
    parser = takeLowerText >>= \case
        "full_access" -> pure FullAccess
        "send_as" -> pure SendAs
        "send_on_behalf" -> pure SendOnBehalf
        e -> fromTextError $ "Failure parsing PermissionType from value: '" <> e
           <> "'. Accepted values: full_access, send_as, send_on_behalf"

instance ToText PermissionType where
    toText = \case
        FullAccess -> "FULL_ACCESS"
        SendAs -> "SEND_AS"
        SendOnBehalf -> "SEND_ON_BEHALF"

instance Hashable     PermissionType
instance NFData       PermissionType
instance ToByteString PermissionType
instance ToQuery      PermissionType
instance ToHeader     PermissionType

instance ToJSON PermissionType where
    toJSON = toJSONText

instance FromJSON PermissionType where
    parseJSON = parseJSONText "PermissionType"

data ResourceType
  = Equipment
  | Room
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "equipment" -> pure Equipment
        "room" -> pure Room
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: equipment, room"

instance ToText ResourceType where
    toText = \case
        Equipment -> "EQUIPMENT"
        Room -> "ROOM"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

data UserRole
  = URResource
  | URSystemUser
  | URUser
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UserRole where
    parser = takeLowerText >>= \case
        "resource" -> pure URResource
        "system_user" -> pure URSystemUser
        "user" -> pure URUser
        e -> fromTextError $ "Failure parsing UserRole from value: '" <> e
           <> "'. Accepted values: resource, system_user, user"

instance ToText UserRole where
    toText = \case
        URResource -> "RESOURCE"
        URSystemUser -> "SYSTEM_USER"
        URUser -> "USER"

instance Hashable     UserRole
instance NFData       UserRole
instance ToByteString UserRole
instance ToQuery      UserRole
instance ToHeader     UserRole

instance FromJSON UserRole where
    parseJSON = parseJSONText "UserRole"
