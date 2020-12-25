{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScaleDownBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScaleDownBehavior
  ( ScaleDownBehavior
      ( ScaleDownBehavior',
        ScaleDownBehaviorTerminateAtInstanceHour,
        ScaleDownBehaviorTerminateAtTaskCompletion,
        fromScaleDownBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScaleDownBehavior = ScaleDownBehavior'
  { fromScaleDownBehavior ::
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

pattern ScaleDownBehaviorTerminateAtInstanceHour :: ScaleDownBehavior
pattern ScaleDownBehaviorTerminateAtInstanceHour = ScaleDownBehavior' "TERMINATE_AT_INSTANCE_HOUR"

pattern ScaleDownBehaviorTerminateAtTaskCompletion :: ScaleDownBehavior
pattern ScaleDownBehaviorTerminateAtTaskCompletion = ScaleDownBehavior' "TERMINATE_AT_TASK_COMPLETION"

{-# COMPLETE
  ScaleDownBehaviorTerminateAtInstanceHour,
  ScaleDownBehaviorTerminateAtTaskCompletion,
  ScaleDownBehavior'
  #-}
