{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationFilterKey where

import Network.AWS.Prelude

data InstanceInformationFilterKey
  = ActivationIds
  | AgentVersion
  | AssociationStatus
  | IAMRole
  | InstanceIds
  | PingStatus
  | PlatformTypes
  | ResourceType
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText InstanceInformationFilterKey where
  parser =
    takeLowerText >>= \case
      "activationids" -> pure ActivationIds
      "agentversion" -> pure AgentVersion
      "associationstatus" -> pure AssociationStatus
      "iamrole" -> pure IAMRole
      "instanceids" -> pure InstanceIds
      "pingstatus" -> pure PingStatus
      "platformtypes" -> pure PlatformTypes
      "resourcetype" -> pure ResourceType
      e ->
        fromTextError $
          "Failure parsing InstanceInformationFilterKey from value: '" <> e
            <> "'. Accepted values: activationids, agentversion, associationstatus, iamrole, instanceids, pingstatus, platformtypes, resourcetype"

instance ToText InstanceInformationFilterKey where
  toText = \case
    ActivationIds -> "ActivationIds"
    AgentVersion -> "AgentVersion"
    AssociationStatus -> "AssociationStatus"
    IAMRole -> "IamRole"
    InstanceIds -> "InstanceIds"
    PingStatus -> "PingStatus"
    PlatformTypes -> "PlatformTypes"
    ResourceType -> "ResourceType"

instance Hashable InstanceInformationFilterKey

instance NFData InstanceInformationFilterKey

instance ToByteString InstanceInformationFilterKey

instance ToQuery InstanceInformationFilterKey

instance ToHeader InstanceInformationFilterKey

instance ToJSON InstanceInformationFilterKey where
  toJSON = toJSONText
