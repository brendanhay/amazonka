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
-- Module      : Network.AWS.ElasticSearch.Types.ScheduledAutoTuneActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ScheduledAutoTuneActionType
  ( ScheduledAutoTuneActionType
      ( ..,
        ScheduledAutoTuneActionType_JVM_HEAP_SIZE_TUNING,
        ScheduledAutoTuneActionType_JVM_YOUNG_GEN_TUNING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies Auto-Tune action type. Valid values are JVM_HEAP_SIZE_TUNING
-- and JVM_YOUNG_GEN_TUNING.
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
