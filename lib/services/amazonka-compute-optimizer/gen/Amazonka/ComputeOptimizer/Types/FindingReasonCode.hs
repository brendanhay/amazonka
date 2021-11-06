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
-- Module      : Amazonka.ComputeOptimizer.Types.FindingReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.FindingReasonCode
  ( FindingReasonCode
      ( ..,
        FindingReasonCode_MemoryOverprovisioned,
        FindingReasonCode_MemoryUnderprovisioned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FindingReasonCode = FindingReasonCode'
  { fromFindingReasonCode ::
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

pattern FindingReasonCode_MemoryOverprovisioned :: FindingReasonCode
pattern FindingReasonCode_MemoryOverprovisioned = FindingReasonCode' "MemoryOverprovisioned"

pattern FindingReasonCode_MemoryUnderprovisioned :: FindingReasonCode
pattern FindingReasonCode_MemoryUnderprovisioned = FindingReasonCode' "MemoryUnderprovisioned"

{-# COMPLETE
  FindingReasonCode_MemoryOverprovisioned,
  FindingReasonCode_MemoryUnderprovisioned,
  FindingReasonCode'
  #-}
