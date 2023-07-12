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
-- Module      : Amazonka.GroundStation.Types.EphemerisStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisStatus
  ( EphemerisStatus
      ( ..,
        EphemerisStatus_DISABLED,
        EphemerisStatus_ENABLED,
        EphemerisStatus_ERROR,
        EphemerisStatus_EXPIRED,
        EphemerisStatus_INVALID,
        EphemerisStatus_VALIDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EphemerisStatus = EphemerisStatus'
  { fromEphemerisStatus ::
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

pattern EphemerisStatus_DISABLED :: EphemerisStatus
pattern EphemerisStatus_DISABLED = EphemerisStatus' "DISABLED"

pattern EphemerisStatus_ENABLED :: EphemerisStatus
pattern EphemerisStatus_ENABLED = EphemerisStatus' "ENABLED"

pattern EphemerisStatus_ERROR :: EphemerisStatus
pattern EphemerisStatus_ERROR = EphemerisStatus' "ERROR"

pattern EphemerisStatus_EXPIRED :: EphemerisStatus
pattern EphemerisStatus_EXPIRED = EphemerisStatus' "EXPIRED"

pattern EphemerisStatus_INVALID :: EphemerisStatus
pattern EphemerisStatus_INVALID = EphemerisStatus' "INVALID"

pattern EphemerisStatus_VALIDATING :: EphemerisStatus
pattern EphemerisStatus_VALIDATING = EphemerisStatus' "VALIDATING"

{-# COMPLETE
  EphemerisStatus_DISABLED,
  EphemerisStatus_ENABLED,
  EphemerisStatus_ERROR,
  EphemerisStatus_EXPIRED,
  EphemerisStatus_INVALID,
  EphemerisStatus_VALIDATING,
  EphemerisStatus'
  #-}
