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
-- Module      : Amazonka.OpenSearch.Types.ScheduledAutoTuneActionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ScheduledAutoTuneActionType
  ( ScheduledAutoTuneActionType
      ( ..,
        ScheduledAutoTuneActionType_JVM_HEAP_SIZE_TUNING,
        ScheduledAutoTuneActionType_JVM_YOUNG_GEN_TUNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The Auto-Tune action type.
newtype ScheduledAutoTuneActionType = ScheduledAutoTuneActionType'
  { fromScheduledAutoTuneActionType ::
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

pattern ScheduledAutoTuneActionType_JVM_HEAP_SIZE_TUNING :: ScheduledAutoTuneActionType
pattern ScheduledAutoTuneActionType_JVM_HEAP_SIZE_TUNING = ScheduledAutoTuneActionType' "JVM_HEAP_SIZE_TUNING"

pattern ScheduledAutoTuneActionType_JVM_YOUNG_GEN_TUNING :: ScheduledAutoTuneActionType
pattern ScheduledAutoTuneActionType_JVM_YOUNG_GEN_TUNING = ScheduledAutoTuneActionType' "JVM_YOUNG_GEN_TUNING"

{-# COMPLETE
  ScheduledAutoTuneActionType_JVM_HEAP_SIZE_TUNING,
  ScheduledAutoTuneActionType_JVM_YOUNG_GEN_TUNING,
  ScheduledAutoTuneActionType'
  #-}
