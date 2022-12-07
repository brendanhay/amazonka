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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MitigationActionType = MitigationActionType'
  { fromMitigationActionType ::
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
