{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentHealth
  ( AgentHealth
      ( AgentHealth',
        AHHealthy,
        AHUnhealthy,
        AHUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AgentHealth = AgentHealth' Lude.Text
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

pattern AHHealthy :: AgentHealth
pattern AHHealthy = AgentHealth' "HEALTHY"

pattern AHUnhealthy :: AgentHealth
pattern AHUnhealthy = AgentHealth' "UNHEALTHY"

pattern AHUnknown :: AgentHealth
pattern AHUnknown = AgentHealth' "UNKNOWN"

{-# COMPLETE
  AHHealthy,
  AHUnhealthy,
  AHUnknown,
  AgentHealth'
  #-}
