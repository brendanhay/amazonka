-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHealthState
  ( InstanceHealthState
      ( InstanceHealthState',
        Draining,
        Healthy,
        Initial,
        Unavailable,
        Unhealthy,
        Unused
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceHealthState = InstanceHealthState' Lude.Text
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

pattern Draining :: InstanceHealthState
pattern Draining = InstanceHealthState' "draining"

pattern Healthy :: InstanceHealthState
pattern Healthy = InstanceHealthState' "healthy"

pattern Initial :: InstanceHealthState
pattern Initial = InstanceHealthState' "initial"

pattern Unavailable :: InstanceHealthState
pattern Unavailable = InstanceHealthState' "unavailable"

pattern Unhealthy :: InstanceHealthState
pattern Unhealthy = InstanceHealthState' "unhealthy"

pattern Unused :: InstanceHealthState
pattern Unused = InstanceHealthState' "unused"

{-# COMPLETE
  Draining,
  Healthy,
  Initial,
  Unavailable,
  Unhealthy,
  Unused,
  InstanceHealthState'
  #-}
