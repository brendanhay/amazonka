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
        UpdateDeviceCertificate,
        UpdateCaCertificate,
        AddThingsToThingGroup,
        ReplaceDefaultPolicyVersion,
        EnableIotLogging,
        PublishFindingToSNS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MitigationActionType = MitigationActionType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern UpdateDeviceCertificate :: MitigationActionType
pattern UpdateDeviceCertificate = MitigationActionType' "UPDATE_DEVICE_CERTIFICATE"

pattern UpdateCaCertificate :: MitigationActionType
pattern UpdateCaCertificate = MitigationActionType' "UPDATE_CA_CERTIFICATE"

pattern AddThingsToThingGroup :: MitigationActionType
pattern AddThingsToThingGroup = MitigationActionType' "ADD_THINGS_TO_THING_GROUP"

pattern ReplaceDefaultPolicyVersion :: MitigationActionType
pattern ReplaceDefaultPolicyVersion = MitigationActionType' "REPLACE_DEFAULT_POLICY_VERSION"

pattern EnableIotLogging :: MitigationActionType
pattern EnableIotLogging = MitigationActionType' "ENABLE_IOT_LOGGING"

pattern PublishFindingToSNS :: MitigationActionType
pattern PublishFindingToSNS = MitigationActionType' "PUBLISH_FINDING_TO_SNS"

{-# COMPLETE
  UpdateDeviceCertificate,
  UpdateCaCertificate,
  AddThingsToThingGroup,
  ReplaceDefaultPolicyVersion,
  EnableIotLogging,
  PublishFindingToSNS,
  MitigationActionType'
  #-}
