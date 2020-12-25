{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionTypeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionTypeValues
  ( ScheduledActionTypeValues
      ( ScheduledActionTypeValues',
        ScheduledActionTypeValuesResizeCluster,
        ScheduledActionTypeValuesPauseCluster,
        ScheduledActionTypeValuesResumeCluster,
        fromScheduledActionTypeValues
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

newtype ScheduledActionTypeValues = ScheduledActionTypeValues'
  { fromScheduledActionTypeValues ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ScheduledActionTypeValuesResizeCluster :: ScheduledActionTypeValues
pattern ScheduledActionTypeValuesResizeCluster = ScheduledActionTypeValues' "ResizeCluster"

pattern ScheduledActionTypeValuesPauseCluster :: ScheduledActionTypeValues
pattern ScheduledActionTypeValuesPauseCluster = ScheduledActionTypeValues' "PauseCluster"

pattern ScheduledActionTypeValuesResumeCluster :: ScheduledActionTypeValues
pattern ScheduledActionTypeValuesResumeCluster = ScheduledActionTypeValues' "ResumeCluster"

{-# COMPLETE
  ScheduledActionTypeValuesResizeCluster,
  ScheduledActionTypeValuesPauseCluster,
  ScheduledActionTypeValuesResumeCluster,
  ScheduledActionTypeValues'
  #-}
