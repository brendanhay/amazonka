{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.StabilityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.StabilityStatus
  ( StabilityStatus
      ( StabilityStatus',
        StabilityStatusSteadyState,
        StabilityStatusStabilizing,
        fromStabilityStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StabilityStatus = StabilityStatus'
  { fromStabilityStatus ::
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

pattern StabilityStatusSteadyState :: StabilityStatus
pattern StabilityStatusSteadyState = StabilityStatus' "STEADY_STATE"

pattern StabilityStatusStabilizing :: StabilityStatus
pattern StabilityStatusStabilizing = StabilityStatus' "STABILIZING"

{-# COMPLETE
  StabilityStatusSteadyState,
  StabilityStatusStabilizing,
  StabilityStatus'
  #-}
