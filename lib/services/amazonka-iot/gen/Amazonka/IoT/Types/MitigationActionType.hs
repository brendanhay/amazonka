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
-- Module      : Amazonka.IoT.Types.MitigationActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MitigationActionType
  ( MitigationActionType
      ( ..,
        MitigationActionType_ADD_THINGS_TO_THING_GROUP,
        MitigationActionType_ENABLE_IOT_LOGGING,
        MitigationActionType_PUBLISH_FINDING_TO_SNS,
        MitigationActionType_REPLACE_DEFAULT_POLICY_VERSION,
        MitigationActionType_UPDATE_CA_CERTIFICATE,
        MitigationActionType_UPDATE_DEVICE_CERTIFICATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MitigationActionType = MitigationActionType'
  { fromMitigationActionType ::
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

pattern MitigationActionType_ADD_THINGS_TO_THING_GROUP :: MitigationActionType
pattern MitigationActionType_ADD_THINGS_TO_THING_GROUP = MitigationActionType' "ADD_THINGS_TO_THING_GROUP"

pattern MitigationActionType_ENABLE_IOT_LOGGING :: MitigationActionType
pattern MitigationActionType_ENABLE_IOT_LOGGING = MitigationActionType' "ENABLE_IOT_LOGGING"

pattern MitigationActionType_PUBLISH_FINDING_TO_SNS :: MitigationActionType
pattern MitigationActionType_PUBLISH_FINDING_TO_SNS = MitigationActionType' "PUBLISH_FINDING_TO_SNS"

pattern MitigationActionType_REPLACE_DEFAULT_POLICY_VERSION :: MitigationActionType
pattern MitigationActionType_REPLACE_DEFAULT_POLICY_VERSION = MitigationActionType' "REPLACE_DEFAULT_POLICY_VERSION"

pattern MitigationActionType_UPDATE_CA_CERTIFICATE :: MitigationActionType
pattern MitigationActionType_UPDATE_CA_CERTIFICATE = MitigationActionType' "UPDATE_CA_CERTIFICATE"

pattern MitigationActionType_UPDATE_DEVICE_CERTIFICATE :: MitigationActionType
pattern MitigationActionType_UPDATE_DEVICE_CERTIFICATE = MitigationActionType' "UPDATE_DEVICE_CERTIFICATE"

{-# COMPLETE
  MitigationActionType_ADD_THINGS_TO_THING_GROUP,
  MitigationActionType_ENABLE_IOT_LOGGING,
  MitigationActionType_PUBLISH_FINDING_TO_SNS,
  MitigationActionType_REPLACE_DEFAULT_POLICY_VERSION,
  MitigationActionType_UPDATE_CA_CERTIFICATE,
  MitigationActionType_UPDATE_DEVICE_CERTIFICATE,
  MitigationActionType'
  #-}
