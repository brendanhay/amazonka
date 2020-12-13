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
        Initial,
        Healthy,
        Unhealthy,
        Unused,
        Draining,
        Unavailable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TargetHealthStateEnum = TargetHealthStateEnum' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Initial :: TargetHealthStateEnum
pattern Initial = TargetHealthStateEnum' "initial"

pattern Healthy :: TargetHealthStateEnum
pattern Healthy = TargetHealthStateEnum' "healthy"

pattern Unhealthy :: TargetHealthStateEnum
pattern Unhealthy = TargetHealthStateEnum' "unhealthy"

pattern Unused :: TargetHealthStateEnum
pattern Unused = TargetHealthStateEnum' "unused"

pattern Draining :: TargetHealthStateEnum
pattern Draining = TargetHealthStateEnum' "draining"

pattern Unavailable :: TargetHealthStateEnum
pattern Unavailable = TargetHealthStateEnum' "unavailable"

{-# COMPLETE
  Initial,
  Healthy,
  Unhealthy,
  Unused,
  Draining,
  Unavailable,
  TargetHealthStateEnum'
  #-}
