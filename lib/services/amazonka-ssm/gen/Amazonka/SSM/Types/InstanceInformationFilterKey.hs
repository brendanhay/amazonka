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
-- Module      : Amazonka.SSM.Types.InstanceInformationFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceInformationFilterKey
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceInformationFilterKey = InstanceInformationFilterKey'
  { fromInstanceInformationFilterKey ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
