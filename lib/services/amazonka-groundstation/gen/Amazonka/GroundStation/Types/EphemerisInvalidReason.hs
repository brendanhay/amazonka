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
-- Module      : Amazonka.GroundStation.Types.EphemerisInvalidReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisInvalidReason
  ( EphemerisInvalidReason
      ( ..,
        EphemerisInvalidReason_KMS_KEY_INVALID,
        EphemerisInvalidReason_METADATA_INVALID,
        EphemerisInvalidReason_TIME_RANGE_INVALID,
        EphemerisInvalidReason_TRAJECTORY_INVALID,
        EphemerisInvalidReason_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EphemerisInvalidReason = EphemerisInvalidReason'
  { fromEphemerisInvalidReason ::
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

pattern EphemerisInvalidReason_KMS_KEY_INVALID :: EphemerisInvalidReason
pattern EphemerisInvalidReason_KMS_KEY_INVALID = EphemerisInvalidReason' "KMS_KEY_INVALID"

pattern EphemerisInvalidReason_METADATA_INVALID :: EphemerisInvalidReason
pattern EphemerisInvalidReason_METADATA_INVALID = EphemerisInvalidReason' "METADATA_INVALID"

pattern EphemerisInvalidReason_TIME_RANGE_INVALID :: EphemerisInvalidReason
pattern EphemerisInvalidReason_TIME_RANGE_INVALID = EphemerisInvalidReason' "TIME_RANGE_INVALID"

pattern EphemerisInvalidReason_TRAJECTORY_INVALID :: EphemerisInvalidReason
pattern EphemerisInvalidReason_TRAJECTORY_INVALID = EphemerisInvalidReason' "TRAJECTORY_INVALID"

pattern EphemerisInvalidReason_VALIDATION_ERROR :: EphemerisInvalidReason
pattern EphemerisInvalidReason_VALIDATION_ERROR = EphemerisInvalidReason' "VALIDATION_ERROR"

{-# COMPLETE
  EphemerisInvalidReason_KMS_KEY_INVALID,
  EphemerisInvalidReason_METADATA_INVALID,
  EphemerisInvalidReason_TIME_RANGE_INVALID,
  EphemerisInvalidReason_TRAJECTORY_INVALID,
  EphemerisInvalidReason_VALIDATION_ERROR,
  EphemerisInvalidReason'
  #-}
