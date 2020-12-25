{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConversionTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConversionTaskState
  ( ConversionTaskState
      ( ConversionTaskState',
        ConversionTaskStateActive,
        ConversionTaskStateCancelling,
        ConversionTaskStateCancelled,
        ConversionTaskStateCompleted,
        fromConversionTaskState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConversionTaskState = ConversionTaskState'
  { fromConversionTaskState ::
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

pattern ConversionTaskStateActive :: ConversionTaskState
pattern ConversionTaskStateActive = ConversionTaskState' "active"

pattern ConversionTaskStateCancelling :: ConversionTaskState
pattern ConversionTaskStateCancelling = ConversionTaskState' "cancelling"

pattern ConversionTaskStateCancelled :: ConversionTaskState
pattern ConversionTaskStateCancelled = ConversionTaskState' "cancelled"

pattern ConversionTaskStateCompleted :: ConversionTaskState
pattern ConversionTaskStateCompleted = ConversionTaskState' "completed"

{-# COMPLETE
  ConversionTaskStateActive,
  ConversionTaskStateCancelling,
  ConversionTaskStateCancelled,
  ConversionTaskStateCompleted,
  ConversionTaskState'
  #-}
