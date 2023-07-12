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
-- Module      : Amazonka.AlexaBusiness.Types.EnrollmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.EnrollmentStatus
  ( EnrollmentStatus
      ( ..,
        EnrollmentStatus_DEREGISTERING,
        EnrollmentStatus_DISASSOCIATING,
        EnrollmentStatus_INITIALIZED,
        EnrollmentStatus_PENDING,
        EnrollmentStatus_REGISTERED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnrollmentStatus = EnrollmentStatus'
  { fromEnrollmentStatus ::
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

pattern EnrollmentStatus_DEREGISTERING :: EnrollmentStatus
pattern EnrollmentStatus_DEREGISTERING = EnrollmentStatus' "DEREGISTERING"

pattern EnrollmentStatus_DISASSOCIATING :: EnrollmentStatus
pattern EnrollmentStatus_DISASSOCIATING = EnrollmentStatus' "DISASSOCIATING"

pattern EnrollmentStatus_INITIALIZED :: EnrollmentStatus
pattern EnrollmentStatus_INITIALIZED = EnrollmentStatus' "INITIALIZED"

pattern EnrollmentStatus_PENDING :: EnrollmentStatus
pattern EnrollmentStatus_PENDING = EnrollmentStatus' "PENDING"

pattern EnrollmentStatus_REGISTERED :: EnrollmentStatus
pattern EnrollmentStatus_REGISTERED = EnrollmentStatus' "REGISTERED"

{-# COMPLETE
  EnrollmentStatus_DEREGISTERING,
  EnrollmentStatus_DISASSOCIATING,
  EnrollmentStatus_INITIALIZED,
  EnrollmentStatus_PENDING,
  EnrollmentStatus_REGISTERED,
  EnrollmentStatus'
  #-}
