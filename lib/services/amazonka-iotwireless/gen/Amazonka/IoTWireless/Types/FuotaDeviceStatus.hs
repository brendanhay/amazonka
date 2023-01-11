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
-- Module      : Amazonka.IoTWireless.Types.FuotaDeviceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.FuotaDeviceStatus
  ( FuotaDeviceStatus
      ( ..,
        FuotaDeviceStatus_FragAlgo_unsupported,
        FuotaDeviceStatus_FragIndex_unsupported,
        FuotaDeviceStatus_Initial,
        FuotaDeviceStatus_MICError,
        FuotaDeviceStatus_MemoryError,
        FuotaDeviceStatus_MissingFrag,
        FuotaDeviceStatus_Not_enough_memory,
        FuotaDeviceStatus_Package_Not_Supported,
        FuotaDeviceStatus_SessionCnt_replay,
        FuotaDeviceStatus_Successful,
        FuotaDeviceStatus_Wrong_descriptor
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of a wireless device in a FUOTA task.
newtype FuotaDeviceStatus = FuotaDeviceStatus'
  { fromFuotaDeviceStatus ::
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

pattern FuotaDeviceStatus_FragAlgo_unsupported :: FuotaDeviceStatus
pattern FuotaDeviceStatus_FragAlgo_unsupported = FuotaDeviceStatus' "FragAlgo_unsupported"

pattern FuotaDeviceStatus_FragIndex_unsupported :: FuotaDeviceStatus
pattern FuotaDeviceStatus_FragIndex_unsupported = FuotaDeviceStatus' "FragIndex_unsupported"

pattern FuotaDeviceStatus_Initial :: FuotaDeviceStatus
pattern FuotaDeviceStatus_Initial = FuotaDeviceStatus' "Initial"

pattern FuotaDeviceStatus_MICError :: FuotaDeviceStatus
pattern FuotaDeviceStatus_MICError = FuotaDeviceStatus' "MICError"

pattern FuotaDeviceStatus_MemoryError :: FuotaDeviceStatus
pattern FuotaDeviceStatus_MemoryError = FuotaDeviceStatus' "MemoryError"

pattern FuotaDeviceStatus_MissingFrag :: FuotaDeviceStatus
pattern FuotaDeviceStatus_MissingFrag = FuotaDeviceStatus' "MissingFrag"

pattern FuotaDeviceStatus_Not_enough_memory :: FuotaDeviceStatus
pattern FuotaDeviceStatus_Not_enough_memory = FuotaDeviceStatus' "Not_enough_memory"

pattern FuotaDeviceStatus_Package_Not_Supported :: FuotaDeviceStatus
pattern FuotaDeviceStatus_Package_Not_Supported = FuotaDeviceStatus' "Package_Not_Supported"

pattern FuotaDeviceStatus_SessionCnt_replay :: FuotaDeviceStatus
pattern FuotaDeviceStatus_SessionCnt_replay = FuotaDeviceStatus' "SessionCnt_replay"

pattern FuotaDeviceStatus_Successful :: FuotaDeviceStatus
pattern FuotaDeviceStatus_Successful = FuotaDeviceStatus' "Successful"

pattern FuotaDeviceStatus_Wrong_descriptor :: FuotaDeviceStatus
pattern FuotaDeviceStatus_Wrong_descriptor = FuotaDeviceStatus' "Wrong_descriptor"

{-# COMPLETE
  FuotaDeviceStatus_FragAlgo_unsupported,
  FuotaDeviceStatus_FragIndex_unsupported,
  FuotaDeviceStatus_Initial,
  FuotaDeviceStatus_MICError,
  FuotaDeviceStatus_MemoryError,
  FuotaDeviceStatus_MissingFrag,
  FuotaDeviceStatus_Not_enough_memory,
  FuotaDeviceStatus_Package_Not_Supported,
  FuotaDeviceStatus_SessionCnt_replay,
  FuotaDeviceStatus_Successful,
  FuotaDeviceStatus_Wrong_descriptor,
  FuotaDeviceStatus'
  #-}
