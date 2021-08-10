{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationFilterKey
  ( InstanceInformationFilterKey
      ( ..,
        InstanceInformationFilterKey_ActivationIds,
        InstanceInformationFilterKey_AgentVersion,
        InstanceInformationFilterKey_AssociationStatus,
        InstanceInformationFilterKey_IamRole,
        InstanceInformationFilterKey_InstanceIds,
        InstanceInformationFilterKey_PingStatus,
        InstanceInformationFilterKey_PlatformTypes,
        InstanceInformationFilterKey_ResourceType
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceInformationFilterKey = InstanceInformationFilterKey'
  { fromInstanceInformationFilterKey ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern InstanceInformationFilterKey_ActivationIds :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_ActivationIds = InstanceInformationFilterKey' "ActivationIds"

pattern InstanceInformationFilterKey_AgentVersion :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_AgentVersion = InstanceInformationFilterKey' "AgentVersion"

pattern InstanceInformationFilterKey_AssociationStatus :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_AssociationStatus = InstanceInformationFilterKey' "AssociationStatus"

pattern InstanceInformationFilterKey_IamRole :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_IamRole = InstanceInformationFilterKey' "IamRole"

pattern InstanceInformationFilterKey_InstanceIds :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_InstanceIds = InstanceInformationFilterKey' "InstanceIds"

pattern InstanceInformationFilterKey_PingStatus :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_PingStatus = InstanceInformationFilterKey' "PingStatus"

pattern InstanceInformationFilterKey_PlatformTypes :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_PlatformTypes = InstanceInformationFilterKey' "PlatformTypes"

pattern InstanceInformationFilterKey_ResourceType :: InstanceInformationFilterKey
pattern InstanceInformationFilterKey_ResourceType = InstanceInformationFilterKey' "ResourceType"

{-# COMPLETE
  InstanceInformationFilterKey_ActivationIds,
  InstanceInformationFilterKey_AgentVersion,
  InstanceInformationFilterKey_AssociationStatus,
  InstanceInformationFilterKey_IamRole,
  InstanceInformationFilterKey_InstanceIds,
  InstanceInformationFilterKey_PingStatus,
  InstanceInformationFilterKey_PlatformTypes,
  InstanceInformationFilterKey_ResourceType,
  InstanceInformationFilterKey'
  #-}
