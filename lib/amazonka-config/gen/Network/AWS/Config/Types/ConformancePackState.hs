{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackState
  ( ConformancePackState
      ( ConformancePackState',
        ConformancePackStateCreateInProgress,
        ConformancePackStateCreateComplete,
        ConformancePackStateCreateFailed,
        ConformancePackStateDeleteInProgress,
        ConformancePackStateDeleteFailed,
        fromConformancePackState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConformancePackState = ConformancePackState'
  { fromConformancePackState ::
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

pattern ConformancePackStateCreateInProgress :: ConformancePackState
pattern ConformancePackStateCreateInProgress = ConformancePackState' "CREATE_IN_PROGRESS"

pattern ConformancePackStateCreateComplete :: ConformancePackState
pattern ConformancePackStateCreateComplete = ConformancePackState' "CREATE_COMPLETE"

pattern ConformancePackStateCreateFailed :: ConformancePackState
pattern ConformancePackStateCreateFailed = ConformancePackState' "CREATE_FAILED"

pattern ConformancePackStateDeleteInProgress :: ConformancePackState
pattern ConformancePackStateDeleteInProgress = ConformancePackState' "DELETE_IN_PROGRESS"

pattern ConformancePackStateDeleteFailed :: ConformancePackState
pattern ConformancePackStateDeleteFailed = ConformancePackState' "DELETE_FAILED"

{-# COMPLETE
  ConformancePackStateCreateInProgress,
  ConformancePackStateCreateComplete,
  ConformancePackStateCreateFailed,
  ConformancePackStateDeleteInProgress,
  ConformancePackStateDeleteFailed,
  ConformancePackState'
  #-}
