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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryStatus
  ( ProtectedQueryStatus
      ( ..,
        ProtectedQueryStatus_CANCELLED,
        ProtectedQueryStatus_CANCELLING,
        ProtectedQueryStatus_FAILED,
        ProtectedQueryStatus_STARTED,
        ProtectedQueryStatus_SUBMITTED,
        ProtectedQueryStatus_SUCCESS,
        ProtectedQueryStatus_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProtectedQueryStatus = ProtectedQueryStatus'
  { fromProtectedQueryStatus ::
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

pattern ProtectedQueryStatus_CANCELLED :: ProtectedQueryStatus
pattern ProtectedQueryStatus_CANCELLED = ProtectedQueryStatus' "CANCELLED"

pattern ProtectedQueryStatus_CANCELLING :: ProtectedQueryStatus
pattern ProtectedQueryStatus_CANCELLING = ProtectedQueryStatus' "CANCELLING"

pattern ProtectedQueryStatus_FAILED :: ProtectedQueryStatus
pattern ProtectedQueryStatus_FAILED = ProtectedQueryStatus' "FAILED"

pattern ProtectedQueryStatus_STARTED :: ProtectedQueryStatus
pattern ProtectedQueryStatus_STARTED = ProtectedQueryStatus' "STARTED"

pattern ProtectedQueryStatus_SUBMITTED :: ProtectedQueryStatus
pattern ProtectedQueryStatus_SUBMITTED = ProtectedQueryStatus' "SUBMITTED"

pattern ProtectedQueryStatus_SUCCESS :: ProtectedQueryStatus
pattern ProtectedQueryStatus_SUCCESS = ProtectedQueryStatus' "SUCCESS"

pattern ProtectedQueryStatus_TIMED_OUT :: ProtectedQueryStatus
pattern ProtectedQueryStatus_TIMED_OUT = ProtectedQueryStatus' "TIMED_OUT"

{-# COMPLETE
  ProtectedQueryStatus_CANCELLED,
  ProtectedQueryStatus_CANCELLING,
  ProtectedQueryStatus_FAILED,
  ProtectedQueryStatus_STARTED,
  ProtectedQueryStatus_SUBMITTED,
  ProtectedQueryStatus_SUCCESS,
  ProtectedQueryStatus_TIMED_OUT,
  ProtectedQueryStatus'
  #-}
