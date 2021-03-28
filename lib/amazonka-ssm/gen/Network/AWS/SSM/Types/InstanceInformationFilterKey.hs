{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InstanceInformationFilterKey
  ( InstanceInformationFilterKey
    ( InstanceInformationFilterKey'
    , InstanceInformationFilterKeyInstanceIds
    , InstanceInformationFilterKeyAgentVersion
    , InstanceInformationFilterKeyPingStatus
    , InstanceInformationFilterKeyPlatformTypes
    , InstanceInformationFilterKeyActivationIds
    , InstanceInformationFilterKeyIamRole
    , InstanceInformationFilterKeyResourceType
    , InstanceInformationFilterKeyAssociationStatus
    , fromInstanceInformationFilterKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceInformationFilterKey = InstanceInformationFilterKey'{fromInstanceInformationFilterKey
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern InstanceInformationFilterKeyInstanceIds :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyInstanceIds = InstanceInformationFilterKey' "InstanceIds"

pattern InstanceInformationFilterKeyAgentVersion :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyAgentVersion = InstanceInformationFilterKey' "AgentVersion"

pattern InstanceInformationFilterKeyPingStatus :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyPingStatus = InstanceInformationFilterKey' "PingStatus"

pattern InstanceInformationFilterKeyPlatformTypes :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyPlatformTypes = InstanceInformationFilterKey' "PlatformTypes"

pattern InstanceInformationFilterKeyActivationIds :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyActivationIds = InstanceInformationFilterKey' "ActivationIds"

pattern InstanceInformationFilterKeyIamRole :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyIamRole = InstanceInformationFilterKey' "IamRole"

pattern InstanceInformationFilterKeyResourceType :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyResourceType = InstanceInformationFilterKey' "ResourceType"

pattern InstanceInformationFilterKeyAssociationStatus :: InstanceInformationFilterKey
pattern InstanceInformationFilterKeyAssociationStatus = InstanceInformationFilterKey' "AssociationStatus"

{-# COMPLETE 
  InstanceInformationFilterKeyInstanceIds,

  InstanceInformationFilterKeyAgentVersion,

  InstanceInformationFilterKeyPingStatus,

  InstanceInformationFilterKeyPlatformTypes,

  InstanceInformationFilterKeyActivationIds,

  InstanceInformationFilterKeyIamRole,

  InstanceInformationFilterKeyResourceType,

  InstanceInformationFilterKeyAssociationStatus,
  InstanceInformationFilterKey'
  #-}
