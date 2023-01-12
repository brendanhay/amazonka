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
-- Module      : Amazonka.Synthetics.Types.CanaryRunState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryRunState
  ( CanaryRunState
      ( ..,
        CanaryRunState_FAILED,
        CanaryRunState_PASSED,
        CanaryRunState_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CanaryRunState = CanaryRunState'
  { fromCanaryRunState ::
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

pattern CanaryRunState_FAILED :: CanaryRunState
pattern CanaryRunState_FAILED = CanaryRunState' "FAILED"

pattern CanaryRunState_PASSED :: CanaryRunState
pattern CanaryRunState_PASSED = CanaryRunState' "PASSED"

pattern CanaryRunState_RUNNING :: CanaryRunState
pattern CanaryRunState_RUNNING = CanaryRunState' "RUNNING"

{-# COMPLETE
  CanaryRunState_FAILED,
  CanaryRunState_PASSED,
  CanaryRunState_RUNNING,
  CanaryRunState'
  #-}
