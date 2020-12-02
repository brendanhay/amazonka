{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Cloud9.Types.Sum where

import Network.AWS.Prelude

data EnvironmentStatus
  = Connecting
  | Creating
  | Deleting
  | Error'
  | Ready
  | Stopped
  | Stopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentStatus where
    parser = takeLowerText >>= \case
        "connecting" -> pure Connecting
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        "error" -> pure Error'
        "ready" -> pure Ready
        "stopped" -> pure Stopped
        "stopping" -> pure Stopping
        e -> fromTextError $ "Failure parsing EnvironmentStatus from value: '" <> e
           <> "'. Accepted values: connecting, creating, deleting, error, ready, stopped, stopping"

instance ToText EnvironmentStatus where
    toText = \case
        Connecting -> "connecting"
        Creating -> "creating"
        Deleting -> "deleting"
        Error' -> "error"
        Ready -> "ready"
        Stopped -> "stopped"
        Stopping -> "stopping"

instance Hashable     EnvironmentStatus
instance NFData       EnvironmentStatus
instance ToByteString EnvironmentStatus
instance ToQuery      EnvironmentStatus
instance ToHeader     EnvironmentStatus

instance FromJSON EnvironmentStatus where
    parseJSON = parseJSONText "EnvironmentStatus"

data EnvironmentType
  = EC2
  | SSH
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentType where
    parser = takeLowerText >>= \case
        "ec2" -> pure EC2
        "ssh" -> pure SSH
        e -> fromTextError $ "Failure parsing EnvironmentType from value: '" <> e
           <> "'. Accepted values: ec2, ssh"

instance ToText EnvironmentType where
    toText = \case
        EC2 -> "ec2"
        SSH -> "ssh"

instance Hashable     EnvironmentType
instance NFData       EnvironmentType
instance ToByteString EnvironmentType
instance ToQuery      EnvironmentType
instance ToHeader     EnvironmentType

instance FromJSON EnvironmentType where
    parseJSON = parseJSONText "EnvironmentType"

data MemberPermissions
  = MPReadOnly
  | MPReadWrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MemberPermissions where
    parser = takeLowerText >>= \case
        "read-only" -> pure MPReadOnly
        "read-write" -> pure MPReadWrite
        e -> fromTextError $ "Failure parsing MemberPermissions from value: '" <> e
           <> "'. Accepted values: read-only, read-write"

instance ToText MemberPermissions where
    toText = \case
        MPReadOnly -> "read-only"
        MPReadWrite -> "read-write"

instance Hashable     MemberPermissions
instance NFData       MemberPermissions
instance ToByteString MemberPermissions
instance ToQuery      MemberPermissions
instance ToHeader     MemberPermissions

instance ToJSON MemberPermissions where
    toJSON = toJSONText

data Permissions
  = Owner
  | ReadOnly
  | ReadWrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Permissions where
    parser = takeLowerText >>= \case
        "owner" -> pure Owner
        "read-only" -> pure ReadOnly
        "read-write" -> pure ReadWrite
        e -> fromTextError $ "Failure parsing Permissions from value: '" <> e
           <> "'. Accepted values: owner, read-only, read-write"

instance ToText Permissions where
    toText = \case
        Owner -> "owner"
        ReadOnly -> "read-only"
        ReadWrite -> "read-write"

instance Hashable     Permissions
instance NFData       Permissions
instance ToByteString Permissions
instance ToQuery      Permissions
instance ToHeader     Permissions

instance ToJSON Permissions where
    toJSON = toJSONText

instance FromJSON Permissions where
    parseJSON = parseJSONText "Permissions"
