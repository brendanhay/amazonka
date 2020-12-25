{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.TransitionToIARules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.TransitionToIARules
  ( TransitionToIARules
      ( TransitionToIARules',
        TransitionToIARulesAfter7Days,
        TransitionToIARulesAfter14Days,
        TransitionToIARulesAfter30Days,
        TransitionToIARulesAfter60Days,
        TransitionToIARulesAfter90Days,
        fromTransitionToIARules
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TransitionToIARules = TransitionToIARules'
  { fromTransitionToIARules ::
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

pattern TransitionToIARulesAfter7Days :: TransitionToIARules
pattern TransitionToIARulesAfter7Days = TransitionToIARules' "AFTER_7_DAYS"

pattern TransitionToIARulesAfter14Days :: TransitionToIARules
pattern TransitionToIARulesAfter14Days = TransitionToIARules' "AFTER_14_DAYS"

pattern TransitionToIARulesAfter30Days :: TransitionToIARules
pattern TransitionToIARulesAfter30Days = TransitionToIARules' "AFTER_30_DAYS"

pattern TransitionToIARulesAfter60Days :: TransitionToIARules
pattern TransitionToIARulesAfter60Days = TransitionToIARules' "AFTER_60_DAYS"

pattern TransitionToIARulesAfter90Days :: TransitionToIARules
pattern TransitionToIARulesAfter90Days = TransitionToIARules' "AFTER_90_DAYS"

{-# COMPLETE
  TransitionToIARulesAfter7Days,
  TransitionToIARulesAfter14Days,
  TransitionToIARulesAfter30Days,
  TransitionToIARulesAfter60Days,
  TransitionToIARulesAfter90Days,
  TransitionToIARules'
  #-}
