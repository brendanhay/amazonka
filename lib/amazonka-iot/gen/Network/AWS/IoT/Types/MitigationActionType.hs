{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationActionType
  ( MitigationActionType
      ( MitigationActionType',
        MitigationActionTypeUpdateDeviceCertificate,
        MitigationActionTypeUpdateCaCertificate,
        MitigationActionTypeAddThingsToThingGroup,
        MitigationActionTypeReplaceDefaultPolicyVersion,
        MitigationActionTypeEnableIotLogging,
        MitigationActionTypePublishFindingToSns,
        fromMitigationActionType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MitigationActionType = MitigationActionType'
  { fromMitigationActionType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern MitigationActionTypeUpdateDeviceCertificate :: MitigationActionType
pattern MitigationActionTypeUpdateDeviceCertificate = MitigationActionType' "UPDATE_DEVICE_CERTIFICATE"

pattern MitigationActionTypeUpdateCaCertificate :: MitigationActionType
pattern MitigationActionTypeUpdateCaCertificate = MitigationActionType' "UPDATE_CA_CERTIFICATE"

pattern MitigationActionTypeAddThingsToThingGroup :: MitigationActionType
pattern MitigationActionTypeAddThingsToThingGroup = MitigationActionType' "ADD_THINGS_TO_THING_GROUP"

pattern MitigationActionTypeReplaceDefaultPolicyVersion :: MitigationActionType
pattern MitigationActionTypeReplaceDefaultPolicyVersion = MitigationActionType' "REPLACE_DEFAULT_POLICY_VERSION"

pattern MitigationActionTypeEnableIotLogging :: MitigationActionType
pattern MitigationActionTypeEnableIotLogging = MitigationActionType' "ENABLE_IOT_LOGGING"

pattern MitigationActionTypePublishFindingToSns :: MitigationActionType
pattern MitigationActionTypePublishFindingToSns = MitigationActionType' "PUBLISH_FINDING_TO_SNS"

{-# COMPLETE
  MitigationActionTypeUpdateDeviceCertificate,
  MitigationActionTypeUpdateCaCertificate,
  MitigationActionTypeAddThingsToThingGroup,
  MitigationActionTypeReplaceDefaultPolicyVersion,
  MitigationActionTypeEnableIotLogging,
  MitigationActionTypePublishFindingToSns,
  MitigationActionType'
  #-}
