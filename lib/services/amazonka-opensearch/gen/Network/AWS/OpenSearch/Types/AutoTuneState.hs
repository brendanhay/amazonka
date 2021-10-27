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
-- Module      : Network.AWS.OpenSearch.Types.AutoTuneState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.AutoTuneState
  ( AutoTuneState
      ( ..,
        AutoTuneState_DISABLED,
        AutoTuneState_DISABLED_AND_ROLLBACK_COMPLETE,
        AutoTuneState_DISABLED_AND_ROLLBACK_ERROR,
        AutoTuneState_DISABLED_AND_ROLLBACK_IN_PROGRESS,
        AutoTuneState_DISABLED_AND_ROLLBACK_SCHEDULED,
        AutoTuneState_DISABLE_IN_PROGRESS,
        AutoTuneState_ENABLED,
        AutoTuneState_ENABLE_IN_PROGRESS,
        AutoTuneState_ERROR
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The Auto-Tune state for the domain. For valid states see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>.
newtype AutoTuneState = AutoTuneState'
  { fromAutoTuneState ::
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

pattern AutoTuneState_DISABLED :: AutoTuneState
pattern AutoTuneState_DISABLED = AutoTuneState' "DISABLED"

pattern AutoTuneState_DISABLED_AND_ROLLBACK_COMPLETE :: AutoTuneState
pattern AutoTuneState_DISABLED_AND_ROLLBACK_COMPLETE = AutoTuneState' "DISABLED_AND_ROLLBACK_COMPLETE"

pattern AutoTuneState_DISABLED_AND_ROLLBACK_ERROR :: AutoTuneState
pattern AutoTuneState_DISABLED_AND_ROLLBACK_ERROR = AutoTuneState' "DISABLED_AND_ROLLBACK_ERROR"

pattern AutoTuneState_DISABLED_AND_ROLLBACK_IN_PROGRESS :: AutoTuneState
pattern AutoTuneState_DISABLED_AND_ROLLBACK_IN_PROGRESS = AutoTuneState' "DISABLED_AND_ROLLBACK_IN_PROGRESS"

pattern AutoTuneState_DISABLED_AND_ROLLBACK_SCHEDULED :: AutoTuneState
pattern AutoTuneState_DISABLED_AND_ROLLBACK_SCHEDULED = AutoTuneState' "DISABLED_AND_ROLLBACK_SCHEDULED"

pattern AutoTuneState_DISABLE_IN_PROGRESS :: AutoTuneState
pattern AutoTuneState_DISABLE_IN_PROGRESS = AutoTuneState' "DISABLE_IN_PROGRESS"

pattern AutoTuneState_ENABLED :: AutoTuneState
pattern AutoTuneState_ENABLED = AutoTuneState' "ENABLED"

pattern AutoTuneState_ENABLE_IN_PROGRESS :: AutoTuneState
pattern AutoTuneState_ENABLE_IN_PROGRESS = AutoTuneState' "ENABLE_IN_PROGRESS"

pattern AutoTuneState_ERROR :: AutoTuneState
pattern AutoTuneState_ERROR = AutoTuneState' "ERROR"

{-# COMPLETE
  AutoTuneState_DISABLED,
  AutoTuneState_DISABLED_AND_ROLLBACK_COMPLETE,
  AutoTuneState_DISABLED_AND_ROLLBACK_ERROR,
  AutoTuneState_DISABLED_AND_ROLLBACK_IN_PROGRESS,
  AutoTuneState_DISABLED_AND_ROLLBACK_SCHEDULED,
  AutoTuneState_DISABLE_IN_PROGRESS,
  AutoTuneState_ENABLED,
  AutoTuneState_ENABLE_IN_PROGRESS,
  AutoTuneState_ERROR,
  AutoTuneState'
  #-}
