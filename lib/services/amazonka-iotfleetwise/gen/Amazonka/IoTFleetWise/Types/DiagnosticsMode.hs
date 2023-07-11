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
-- Module      : Amazonka.IoTFleetWise.Types.DiagnosticsMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.DiagnosticsMode
  ( DiagnosticsMode
      ( ..,
        DiagnosticsMode_OFF,
        DiagnosticsMode_SEND_ACTIVE_DTCS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DiagnosticsMode = DiagnosticsMode'
  { fromDiagnosticsMode ::
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

pattern DiagnosticsMode_OFF :: DiagnosticsMode
pattern DiagnosticsMode_OFF = DiagnosticsMode' "OFF"

pattern DiagnosticsMode_SEND_ACTIVE_DTCS :: DiagnosticsMode
pattern DiagnosticsMode_SEND_ACTIVE_DTCS = DiagnosticsMode' "SEND_ACTIVE_DTCS"

{-# COMPLETE
  DiagnosticsMode_OFF,
  DiagnosticsMode_SEND_ACTIVE_DTCS,
  DiagnosticsMode'
  #-}
