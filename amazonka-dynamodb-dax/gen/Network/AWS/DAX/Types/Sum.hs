{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.Sum where

import Network.AWS.Prelude

data ChangeType
  = Immediate
  | RequiresReboot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeType where
    parser = takeLowerText >>= \case
        "immediate" -> pure Immediate
        "requires_reboot" -> pure RequiresReboot
        e -> fromTextError $ "Failure parsing ChangeType from value: '" <> e
           <> "'. Accepted values: immediate, requires_reboot"

instance ToText ChangeType where
    toText = \case
        Immediate -> "IMMEDIATE"
        RequiresReboot -> "REQUIRES_REBOOT"

instance Hashable     ChangeType
instance NFData       ChangeType
instance ToByteString ChangeType
instance ToQuery      ChangeType
instance ToHeader     ChangeType

instance FromJSON ChangeType where
    parseJSON = parseJSONText "ChangeType"

data IsModifiable
  = Conditional
  | False'
  | True'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IsModifiable where
    parser = takeLowerText >>= \case
        "conditional" -> pure Conditional
        "false" -> pure False'
        "true" -> pure True'
        e -> fromTextError $ "Failure parsing IsModifiable from value: '" <> e
           <> "'. Accepted values: conditional, false, true"

instance ToText IsModifiable where
    toText = \case
        Conditional -> "CONDITIONAL"
        False' -> "FALSE"
        True' -> "TRUE"

instance Hashable     IsModifiable
instance NFData       IsModifiable
instance ToByteString IsModifiable
instance ToQuery      IsModifiable
instance ToHeader     IsModifiable

instance FromJSON IsModifiable where
    parseJSON = parseJSONText "IsModifiable"

data ParameterType
  = Default
  | NodeTypeSpecific
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ParameterType where
    parser = takeLowerText >>= \case
        "default" -> pure Default
        "node_type_specific" -> pure NodeTypeSpecific
        e -> fromTextError $ "Failure parsing ParameterType from value: '" <> e
           <> "'. Accepted values: default, node_type_specific"

instance ToText ParameterType where
    toText = \case
        Default -> "DEFAULT"
        NodeTypeSpecific -> "NODE_TYPE_SPECIFIC"

instance Hashable     ParameterType
instance NFData       ParameterType
instance ToByteString ParameterType
instance ToQuery      ParameterType
instance ToHeader     ParameterType

instance FromJSON ParameterType where
    parseJSON = parseJSONText "ParameterType"

data SourceType
  = Cluster
  | ParameterGroup
  | SubnetGroup
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceType where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        "parameter_group" -> pure ParameterGroup
        "subnet_group" -> pure SubnetGroup
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: cluster, parameter_group, subnet_group"

instance ToText SourceType where
    toText = \case
        Cluster -> "CLUSTER"
        ParameterGroup -> "PARAMETER_GROUP"
        SubnetGroup -> "SUBNET_GROUP"

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance ToJSON SourceType where
    toJSON = toJSONText

instance FromJSON SourceType where
    parseJSON = parseJSONText "SourceType"
