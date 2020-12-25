{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthStateEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealthStateEnum
  ( TargetHealthStateEnum
      ( TargetHealthStateEnum',
        TargetHealthStateEnumInitial,
        TargetHealthStateEnumHealthy,
        TargetHealthStateEnumUnhealthy,
        TargetHealthStateEnumUnused,
        TargetHealthStateEnumDraining,
        TargetHealthStateEnumUnavailable,
        fromTargetHealthStateEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TargetHealthStateEnum = TargetHealthStateEnum'
  { fromTargetHealthStateEnum ::
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

pattern TargetHealthStateEnumInitial :: TargetHealthStateEnum
pattern TargetHealthStateEnumInitial = TargetHealthStateEnum' "initial"

pattern TargetHealthStateEnumHealthy :: TargetHealthStateEnum
pattern TargetHealthStateEnumHealthy = TargetHealthStateEnum' "healthy"

pattern TargetHealthStateEnumUnhealthy :: TargetHealthStateEnum
pattern TargetHealthStateEnumUnhealthy = TargetHealthStateEnum' "unhealthy"

pattern TargetHealthStateEnumUnused :: TargetHealthStateEnum
pattern TargetHealthStateEnumUnused = TargetHealthStateEnum' "unused"

pattern TargetHealthStateEnumDraining :: TargetHealthStateEnum
pattern TargetHealthStateEnumDraining = TargetHealthStateEnum' "draining"

pattern TargetHealthStateEnumUnavailable :: TargetHealthStateEnum
pattern TargetHealthStateEnumUnavailable = TargetHealthStateEnum' "unavailable"

{-# COMPLETE
  TargetHealthStateEnumInitial,
  TargetHealthStateEnumHealthy,
  TargetHealthStateEnumUnhealthy,
  TargetHealthStateEnumUnused,
  TargetHealthStateEnumDraining,
  TargetHealthStateEnumUnavailable,
  TargetHealthStateEnum'
  #-}
