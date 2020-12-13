{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.InsufficientDataHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.InsufficientDataHealthStatus
  ( InsufficientDataHealthStatus
      ( InsufficientDataHealthStatus',
        Healthy,
        Unhealthy,
        LastKnownStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype InsufficientDataHealthStatus = InsufficientDataHealthStatus' Lude.Text
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

pattern Healthy :: InsufficientDataHealthStatus
pattern Healthy = InsufficientDataHealthStatus' "Healthy"

pattern Unhealthy :: InsufficientDataHealthStatus
pattern Unhealthy = InsufficientDataHealthStatus' "Unhealthy"

pattern LastKnownStatus :: InsufficientDataHealthStatus
pattern LastKnownStatus = InsufficientDataHealthStatus' "LastKnownStatus"

{-# COMPLETE
  Healthy,
  Unhealthy,
  LastKnownStatus,
  InsufficientDataHealthStatus'
  #-}
