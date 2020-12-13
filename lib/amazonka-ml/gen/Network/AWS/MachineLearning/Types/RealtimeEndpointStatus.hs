{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
  ( RealtimeEndpointStatus
      ( RealtimeEndpointStatus',
        RESNone,
        RESReady,
        RESUpdating,
        RESFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RealtimeEndpointStatus = RealtimeEndpointStatus' Lude.Text
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

pattern RESNone :: RealtimeEndpointStatus
pattern RESNone = RealtimeEndpointStatus' "NONE"

pattern RESReady :: RealtimeEndpointStatus
pattern RESReady = RealtimeEndpointStatus' "READY"

pattern RESUpdating :: RealtimeEndpointStatus
pattern RESUpdating = RealtimeEndpointStatus' "UPDATING"

pattern RESFailed :: RealtimeEndpointStatus
pattern RESFailed = RealtimeEndpointStatus' "FAILED"

{-# COMPLETE
  RESNone,
  RESReady,
  RESUpdating,
  RESFailed,
  RealtimeEndpointStatus'
  #-}
